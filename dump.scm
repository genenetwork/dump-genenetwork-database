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

(define (delete-substrings str . substrings)
  (fold (lambda (substring result)
          (string-replace-substring result substring ""))
        str
        substrings))

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
                                                (delete-substrings abstract "\x01"))))
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

(define (tissue-short-name->id short-name)
  (string->symbol (string-append "gn:tissue" short-name)))

(define (dump-tissue db)
  ;; The Name and TissueName fields seem to be identical. BIRN_lex_ID
  ;; and BIRN_lex_Name are mostly NULL.
  (sql-for-each (match-lambda
                  (((_ . name)
                    (_ . short-name))
                   ;; Hopefully the Short_Name field is distinct and
                   ;; can be used as an identifier.
                   (let ((id (tissue-short-name->id short-name)))
                     (triple id 'rdf:type 'gn:tissue)
                     (triple id 'gn:name name))))
                db
                "SELECT Name, Short_Name FROM Tissue"))

;; One email ID in the Investigators table has spaces in it. This
;; function fixes that.
(define (fix-email-id email)
  (string-replace-substring email " " ""))

(define (investigator-email->id email)
  (string->symbol
   (string-append "gn:investigator"
                  (string-replace-substring
                   (fix-email-id email) "@" "_"))))

(define (dump-investigators db)
  (sql-for-each (lambda (alist)
                  (let ((id (investigator-email->id (assoc-ref alist "Email"))))
                    (triple id 'rdf:type 'foaf:Person)
                    (scm->triples
                     (cons (cons 'foaf:name (string-append
                                             (assoc-ref alist "FirstName")
                                             " " (assoc-ref alist "LastName")))
                           (map (match-lambda
                                  (('gn:firstName . first-name)
                                   (cons 'foaf:givenName first-name))
                                  (('gn:lastName . last-name)
                                   (cons 'foaf:familyName last-name))
                                  (('gn:phone . phone)
                                   (cons 'foaf:phone phone))
                                  (('gn:email . email)
                                   (cons 'foaf:mbox (fix-email-id email)))
                                  (('gn:url . url)
                                   (cons 'foaf:homepage url))
                                  (x x))
                                (process-metadata-alist alist)))
                     id)))
                db
                ;; There are a few duplicate entries. We group by
                ;; email to deduplicate.
                ;; TODO: Find email ID for records with none. (This is
                ;; just one record corresponding to "Evan Williams")
                "SELECT FirstName, LastName, Address, City, State, ZipCode, Phone, Email, Country, Url FROM Investigators
WHERE Email != ''
GROUP BY Email"))

(define (avg-method-name->id name)
  (string->symbol
   (string-append "gn:avgmethod"
                  (string-replace-substring
                   (string-downcase name) " " "_"))))

(define (dump-avg-method db)
  (sql-for-each (match-lambda
                  (((_ . name))
                   (let ((id (avg-method-name->id name)))
                     (triple id 'rdf:type 'gn:avgMethod)
                     (triple id 'gn:name name))))
                db
                ;; The Name and Normalization fields seem to be the
                ;; same. Some records have Name and Normalization as
                ;; "N/A". Remove them.
                "SELECT Name FROM AvgMethod WHERE Name != \"N/A\""))

(define (gene-chip-name->id name)
  (string->symbol
   (string-replace-substring (string-append "gn:platform" name)
                             " " "_")))

(define (dump-gene-chip db)
  (sql-for-each (match-lambda
                  (((_ . gene-chip-name)
                    (_ . name))
                   (let ((id (gene-chip-name->id name)))
                     (triple id 'rdf:type 'gn:platform)
                     (triple id 'gn:name gene-chip-name))))
                db
                "SELECT GeneChipName, Name FROM GeneChip"))

(define (dump-info-files db)
  (sql-for-each (lambda (alist)
                  (let ((id (string-append "gn:dataset"
                                           (number->string
                                            (assoc-ref alist "GN_AccesionId")))))
                    (triple id 'rdf:type 'gn:dataset)
                    (scm->triples
                     (filter-map (match-lambda
                                   (('gn:gNAccesionId . accession-id)
                                    (cons 'gn:accessionId
                                          (string-append "GN" (number->string accession-id))))
                                   (('gn:datasetStatusName . status)
                                    (cons 'gn:datasetStatus
                                          (string-downcase status)))
                                   (('gn:binomialName . binomial-name)
                                    (cons 'gn:datasetOfSpecies
                                          (binomial-name->species-id binomial-name)))
                                   (('gn:inbredSetName . inbred-set-name)
                                    (cons 'gn:datasetOfInbredSet
                                          (inbred-set-name->id inbred-set-name)))
                                   (('gn:shortName . short-name)
                                    (cons 'gn:datasetOfTissue
                                          (tissue-short-name->id short-name)))
                                   (('gn:email . email)
                                    (cons 'gn:datasetOfInvestigator
                                          (investigator-email->id email)))
                                   (('gn:avgMethodName . (? (negate (cut string=? <> "N/A"))
                                                            avg-method-name))
                                    (cons 'gn:normalization
                                          (avg-method-name->id avg-method-name)))
                                   (('gn:geneChip . name)
                                    (cons 'gn:datasetOfPlatform
                                          (gene-chip-name->id name)))
                                   (('gn:summary . summary)
                                    ;; TODO: Why are there unprintable
                                    ;; characters in the summary?
                                    (cons 'gn:summary
                                          (delete-substrings summary "\x01" "\x03")))
                                   (('gn:aboutTissue . about-tissue)
                                    ;; TODO: Why are there unprintable
                                    ;; characters in the summary?
                                    (cons 'gn:aboutTissue
                                          (delete-substrings about-tissue "\x01" "\x03")))
                                   (x x))
                                 (process-metadata-alist alist))
                     id)))
                db
                ;; TODO: Find email ID for records with none. (This is
                ;; just one record corresponding to "Evan Williams")
                ;; TODO: Double check Platforms. It doesn't seem to
                ;; match up.
                "SELECT GN_AccesionId, InfoPageTitle AS Name, InfoFiles.Title,
Specifics, DatasetStatusName,
Datasets.Summary, Datasets.GeoSeries, Datasets.AboutCases,
Datasets.AboutPlatform, Datasets.AboutTissue, Datasets.AboutDataProcessing,
Datasets.Notes, Datasets.ExperimentDesign, Datasets.Contributors,
Datasets.Citation, Datasets.Acknowledgment,
Species.FullName AS BinomialName,
InbredSet.Name AS InbredSetName,
Tissue.Short_Name,
Investigators.Email,
AvgMethod.Name AS AvgMethodName,
GeneChip.Name AS GeneChip
FROM InfoFiles
LEFT JOIN Datasets USING (DatasetId)
LEFT JOIN DatasetStatus USING (DatasetStatusId)
LEFT JOIN Species USING (SpeciesId)
LEFT JOIN InbredSet USING (InbredSetId)
LEFT JOIN Tissue USING (TissueId)
LEFT JOIN Investigators USING (InvestigatorId)
LEFT JOIN AvgMethod USING (AvgMethodId)
LEFT JOIN GeneChip USING (GeneChipId)
WHERE Investigators.Email != ''"))

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
       (prefix "foaf:" "<http://xmlns.com/foaf/0.1/>")
       (prefix "gn:" "<https://genenetwork.org/>")
       (newline)
       (dump-species db)
       (dump-strain db)
       (dump-mapping-method db)
       (dump-inbred-set db)
       (dump-phenotype db)
       (dump-publication db)
       (dump-publish-xref db)
       (dump-tissue db)
       (dump-investigators db)
       (dump-avg-method db)
       (dump-gene-chip db)
       (dump-info-files db)))))
