#! /usr/bin/env guile
!#

(add-to-load-path (dirname (current-filename)))

(use-modules (rnrs io ports)
             (srfi srfi-1)
             (srfi srfi-26)
             (ice-9 match)
             (ice-9 string-fun)
             (dump sql))


;;; GeneNetwork database connection parameters and dump path

(define (call-with-genenetwork-database proc)
  (let ((connection-settings (call-with-input-file "conn.scm" read)))
    (call-with-database "mysql" (string-join
                                 (list (assq-ref connection-settings 'username)
                                       (assq-ref connection-settings 'password)
                                       (assq-ref connection-settings 'database)
                                       "tcp"
                                       (assq-ref connection-settings 'host)
                                       (number->string
                                        (assq-ref connection-settings 'port)))
                                 ":")
                        proc)))

(define %dump-directory
  (string-append (getenv "HOME") "/data/dump"))

(define (call-with-dump-file filename proc)
  (let ((absolute-path (string-append %dump-directory filename)))
    (display absolute-path)
    (newline)
    (call-with-output-file absolute-path proc)))


;; Dump schema annotations to org

(define (get-tables-from-comments db)
  (sql-map (match-lambda
                  ((("TableName" . table)) table))
                db
                "SELECT TableName FROM TableComments"))

(define (dump-table-fields db table)
  (format #t "* ~a~%" table)
  (match (sql-find db (format #f "SELECT Comment FROM TableComments WHERE TableName = '~a'"
                                   table))
    ((("Comment" . comment))
     (format #t "~a~%" comment)))
  (sql-for-each (lambda (row)
                       (match row
                         ((("TableField" . table-field)
                           ("Foreign_Key" . foreign-key)
                           ("Annotation" . annotation))
                          (format #t "** ~a~%" (substring table-field (1+ (string-length table))))
                          (unless (string-null? foreign-key)
                            (format #t "Foreign key to ~a~%" foreign-key))
                          (unless (string-null? annotation)
                            (display annotation)
                            (newline)))))
                     db
                     (format #f "SELECT TableField, Foreign_Key, Annotation FROM TableFieldAnnotation WHERE TableField LIKE '~a.%'"
                             table))
  (newline))

(define (dump-schema-annotations db)
  (call-with-genenetwork-database
   (lambda (db)
     (for-each (cut dump-table-fields db <>)
               (get-tables-from-comments db)))))


;;; Dump tables

(define (camel->kebab str)
  (call-with-output-string
    (lambda (port)
      (string-for-each (lambda (c)
                         (when (and (not (zero? (port-position port)))
                                    (char-set-contains? char-set:upper-case c))
                           (put-char port #\-))
                         (put-char port (char-downcase c)))
                       str))))

(define (snake->lower-camel str)
  (let ((char-list (string->list str)))
    (call-with-output-string
      (lambda (port)
        (put-char port (char-downcase (string-ref str 0)))
        (map (lambda (char previous-char)
               (unless (char=? char #\_)
                 (put-char port (if (char=? previous-char #\_)
                                    (char-upcase char)
                                    char))))
             (drop char-list 1)
             char-list)))))

(define (camel->lower-camel str)
  (string-append (string-downcase (substring str 0 1))
                 (substring str 1)))

(define (scm->triples alist id)
  (for-each (match-lambda
              ((predicate . object)
               (triple id predicate object)))
            alist))

(define (process-metadata-alist alist)
  (filter-map (match-lambda
                ((key . "") #f)
                ((key . value)
                 (cons (string->symbol
                        (string-append
                         "gn:" (camel->lower-camel
                                (snake->lower-camel key))))
                       value)))
              alist))

(define (triple subject predicate object)
  (format #t "~a ~a ~s .~%" subject predicate object))

(define (binomial-name->species-id binomial-name)
  (string->symbol
   (string-append "gn:" (string-replace-substring binomial-name " " "_"))))

(define (dump-species db)
  (sql-for-each (lambda (alist)
                  (match alist
                    (((_ . common-name)
                      (_ . menu-name)
                      (_ . binomial-name))
                     (let ((id (binomial-name->species-id binomial-name)))
                       (triple id 'rdf:type 'gn:species)
                       ;; Common name
                       (triple id 'gn:name common-name)
                       ;; Menu name (TODO: Maybe, drop this
                       ;; field. It can be inferred from the
                       ;; common name.)
                       (triple id 'gn:menuName menu-name)
                       ;; Binomial name
                       (triple id 'gn:binomialName binomial-name)))))
                db
                "SELECT SpeciesName, MenuName, FullName FROM Species"))

(define (dump-strain db)
  (sql-for-each (lambda (alist)
                  (match alist
                    (((_ . binomial-name)
                      (_ . name)
                      (_ . name2)
                      (_ . symbol)
                      (_ . alias))
                     (let ((id
                            ;; TODO: Ensure this identifier does not collide.
                            (string-append "gn:"
                                           (string-map (lambda (c)
                                                         (case c
                                                           ((#\/ #\< #\> #\+ #\( #\) #\space) #\_)
                                                           (else c)))
                                                       name))))
                       (triple id 'rdf:type 'gn:strain)
                       ;; The species this is a strain of
                       (triple id 'gn:strainOfSpecies
                               (binomial-name->species-id binomial-name))
                       ;; Name
                       (triple id 'gn:name name)
                       ;; A second name, if there is one
                       (unless (string=? name name2)
                         (triple id 'gn:name name2))
                       ;; Symbol, if there is one
                       (unless (string-null? symbol)
                         (triple id 'gn:symbol symbol))
                       ;; Alias, if there is one
                       (unless (string-null? alias)
                         (triple id 'gn:alias alias))))))
                db
                "SELECT Species.FullName, Strain.Name, Strain.Name2, Strain.Symbol, Strain.Alias FROM Strain JOIN Species ON Strain.SpeciesId = Species.SpeciesId"))

(define (mapping-method-name->id name)
  (string->symbol (string-append "gn:mappingMethod" name)))

(define (dump-mapping-method db)
  (sql-for-each (match-lambda
                  (((_ . name))
                   (triple (string-append "gn:mappingMethod" name)
                           'rdf:type 'gn:mappingMethod)))
                db
                "SELECT Name FROM MappingMethod"))

(define (inbred-set-name->id name)
  (string->symbol (string-append "gn:inbredSet" name)))

(define (dump-inbred-set db)
  (sql-for-each (lambda (alist)
                  (let ((id (inbred-set-name->id (assoc-ref alist "Name"))))
                    (triple id 'rdf:type 'gn:phenotype)
                    (scm->triples
                     (filter-map (match-lambda
                                   (('gn:binomialName . binomial-name)
                                    (cons 'gn:inbredSetOfSpecies
                                          (binomial-name->species-id binomial-name)))
                                   (('gn:mappingMethodName . mapping-method-name)
                                    (cons 'gn:inbredSetMappingMethod
                                          (mapping-method-name->id mapping-method-name)))
                                   (x x))
                                 (process-metadata-alist alist))
                     id)))
                db
                "SELECT InbredSet.Name, InbredSet.FullName, GeneticType, Family,
Species.FullName AS BinomialName, MappingMethod.Name AS MappingMethodName
FROM InbredSet
INNER JOIN Species USING (SpeciesId)
INNER JOIN MappingMethod ON InbredSet.MappingMethodId = MappingMethod.Id"))

(define (phenotype-id->id id)
  (string->symbol (string-append "gn:phenotype" (number->string id))))

(define (dump-phenotype db)
  (sql-for-each (lambda (alist)
                  (let ((id (phenotype-id->id (assoc-ref alist "Id"))))
                    (triple id 'rdf:type 'gn:phenotype)
                    (scm->triples
                     (filter (match-lambda
                               (('gn:id . _) #f)
                               (('gn:units . value)
                                (string-ci=? value "unknown"))
                               (_ #t))
                             (process-metadata-alist alist))
                     id)))
                db
                "SELECT Id, Pre_publication_description, Post_publication_description,
Original_description, Units, Pre_publication_abbreviation, Post_publication_abbreviation,
Lab_code, Submitter, Owner, Authorized_Users FROM Phenotype"))

(define (dump-publication db)
  (sql-for-each (lambda (alist)
                  (let ((id (string-append "gn:publication"
                                           (number->string (assoc-ref alist "Id")))))
                    (triple id 'rdf:type 'gn:publication)
                    (triple 'gn:title 'rdfs:subPropertyOf 'rdfs:label)
                    (scm->triples
                     (append-map (match-lambda
                                   (('gn:id . _) '())
                                   ;; The authors field is a comma
                                   ;; separated list. Split it.
                                   (('gn:authors . authors)
                                    (map (lambda (author-name)
                                           (cons 'gn:author (string-trim author-name)))
                                         (string-split authors #\,)))
                                   (('gn:abstract . abstract)
                                    ;; TODO: Handle unprintable
                                    ;; characters better.
                                    (list (cons 'gn:abstract
                                                (string-replace-substring abstract "\x01" ""))))
                                   (x (list x)))
                                 (process-metadata-alist alist))
                     id)))
                db
                "SELECT Id, PubMed_ID, Abstract, Authors, Title, Journal, Volume, Pages, Month, Year FROM Publication"))

(define (dump-publish-xref db)
  (sql-for-each (match-lambda
                  (((_ . inbred-set-name)
                    (_ . phenotype-id))
                   (triple (phenotype-id->id phenotype-id)
                           'gn:phenotypeOfSpecies
                           (inbred-set-name->id inbred-set-name))))
                db
                "SELECT Name, PhenotypeId
FROM PublishXRef
INNER JOIN InbredSet USING (InbredSetId)"))

(define (dump-data-table db table-name data-field)
  (let ((dump-directory (string-append %dump-directory "/" table-name))
        (port #f)
        (current-strain-id #f))
    (unless (file-exists? dump-directory)
      (mkdir dump-directory))
    (sql-for-each (match-lambda
                    (((_ . strain-id)
                      (_ . value))
                     ;; Close file if new strain.
                     (when (and port
                                (not (= current-strain-id strain-id)))
                       (close-port port)
                       (set! port #f))
                     ;; If no file is open, open new file.
                     (unless port
                       (set! current-strain-id strain-id)
                       (let ((filename (string-append dump-directory
                                                      "/" (number->string strain-id))))
                         (display filename (current-error-port))
                         (newline (current-error-port))
                         (set! port (open-output-file filename))))
                     (display value port)
                     (newline port)))
                  db
                  (format #f "SELECT StrainId, ~a FROM ~a ORDER BY StrainId"
                          data-field table-name))
    (close-port port)))

(define (prefix prefix iri)
  (format #t "@prefix ~a ~a .~%" prefix iri))

(call-with-genenetwork-database
 (lambda (db)
   (with-output-to-file (string-append %dump-directory "/dump.ttl")
     (lambda ()
       (prefix "rdf:" "<http://www.w3.org/1999/02/22-rdf-syntax-ns#>")
       (prefix "rdfs:" "<http://www.w3.org/2000/01/rdf-schema#>")
       (prefix "foaf:" "<http://xmlns.com/foaf/0.1/>")
       (prefix "gn:" "<https://genenetwork.org/>")
       (newline)
       (dump-species db)
       (dump-strain db)
       (dump-mapping-method db)
       (dump-inbred-set db)
       (dump-phenotype db)
       (dump-publication db)
       (dump-publish-xref db)))))
