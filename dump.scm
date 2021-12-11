#! /usr/bin/env guile
!#

(add-to-load-path (dirname (current-filename)))

(use-modules (rnrs io ports)
             (srfi srfi-1)
             (srfi srfi-9 gnu)
             (srfi srfi-26)
             (ice-9 match)
             (ice-9 string-fun)
             (dump sql)
             (dump utils))


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

(define %database-name
  (assq-ref (call-with-input-file "conn.scm" read)
            'database))

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
           (select-query ((TableComments TableName))
                         (TableComments))))

(define (dump-table-fields db table)
  (format #t "* ~a~%" table)
  (match (sql-find db
                   (select-query ((TableComments Comment))
                                 (TableComments)
                                 (format #f "WHERE TableName = '~a'" table)))
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
                (select-query ((TableFieldAnnotation TableField)
                               (TableFieldAnnotation Foreign_Key)
                               (TableFieldAnnotation Annotation))
                              (TableFieldAnnotation)
                              (format #f "WHERE TableField LIKE '~a.%'" table)))
  (newline))

(define (dump-schema-annotations db)
  (call-with-genenetwork-database
   (lambda (db)
     (for-each (cut dump-table-fields db <>)
               (get-tables-from-comments db)))))


;;; Dump tables

(define (delete-substrings str . substrings)
  "Delete SUBSTRINGS, a list of strings, from STR."
  (fold (lambda (substring result)
          (string-replace-substring result substring ""))
        str
        substrings))

(define (string->identifier prefix str)
  "Convert STR to a turtle identifier after replacing illegal
characters with an underscore and prefixing with gn:PREFIX."
  (string->symbol
   (string-append "gn:" prefix "_"
                  (string-map (lambda (c)
                                (case c
                                  ((#\/ #\< #\> #\+ #\( #\) #\space #\@) #\_)
                                  (else c)))
                              (string-downcase str)))))

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

(define (string-blank? str)
  "Return non-#f if STR consists only of whitespace characters."
  (string-every char-set:whitespace str))

(define (scm->triples alist id)
  (for-each (match-lambda
              ((predicate . object)
               (when (cond
                      ((string? object)
                       (not (string-blank? object)))
                      (else object))
                 (triple id predicate object))))
            alist))

(define default-metadata-proc
  (match-lambda
    ((key . value)
     (cons (string->symbol
            (string-append "gn:" (snake->lower-camel key)))
           value))
    (x (error "malformed alist element" x))))

(define (triple subject predicate object)
  (format #t "~a ~a ~s .~%" subject predicate object))

(define binomial-name->species-id
  (cut string->identifier "species" <>))

(define (dump-species db)
  (sql-for-each (lambda (row)
                  (scm->triples (map-alist row
                                  (set rdf:type 'gn:species)
                                  ;; Common name
                                  (set gn:name (key "SpeciesName"))
                                  ;; Menu name (TODO: Maybe, drop this field. It can
                                  ;; be inferred from the common name.)
                                  (set gn:menuName (key "MenuName"))
                                  (set gn:binomialname (key "FullName")))
                                (binomial-name->species-id (assoc-ref row "FullName"))))
                db
                (select-query ((Species SpeciesName)
                               (Species MenuName)
                               (Species FullName))
                              (Species))))

(define (dump-strain db)
  (sql-for-each (lambda (row)
                  (scm->triples (map-alist row
                                  (set rdf:type 'gn:strain)
                                  (set gn:strainOfSpecies
                                       (binomial-name->species-id (key "FullName")))
                                  ;; Name, and maybe a second name
                                  (set gn:name (key "Name"))
                                  (set gn:name (key "Name2"))
                                  (set gn:alias (key "Alias")))
                                (string->identifier "strain" (assoc-ref row "Name"))))
                db
                (select-query ((Species FullName)
                               (Strain Name)
                               (Strain Name2)
                               (Strain Symbol)
                               (Strain Alias))
                              (Strain
                               (join Species "ON Strain.SpeciesId = Species.SpeciesId")))))

;; TODO: This function is unused. Remove if not required.
(define mapping-method-name->id
  (cut string->identifier "mappingMethod" <>))

;; TODO: This function is unused. Remove if not required.
(define (dump-mapping-method db)
  (sql-for-each (lambda (row)
                  (scm->triples (map-alist row
                                  (set rdf:type 'gn:mappingMethod))
                                (string->identifier "mappingMethod" (assoc-ref row "Name"))))
                db
                (select-query ((MappingMethod Name))
                              (MappingMethod))))

(define inbred-set-name->id
  (cut string->identifier "inbredSet" <>))

(define (dump-inbred-set db)
  (sql-for-each (lambda (row)
                  (scm->triples (map-alist row
                                  (set rdf:type 'gn:phenotype)
                                  (set gn:inbredSetOfSpecies
                                       (binomial-name->species-id (key "BinomialName")))
                                  (else=> default-metadata-proc))
                                (inbred-set-name->id (assoc-ref row "Name"))))
                db
                (select-query ((InbredSet Name)
                               (InbredSet FullName)
                               (InbredSet GeneticType)
                               (InbredSet Family)
                               (Species FullName BinomialName))
                              (InbredSet
                               (inner-join Species "USING (SpeciesId)")))))

(define (phenotype-id->id id)
  (string->identifier "phenotype" (number->string id)))

(define (dump-phenotype db)
  (sql-for-each (lambda (row)
                  (scm->triples (map-alist row
                                  (delete "Id")
                                  (set rdf:type 'gn:phenotype)
                                  (set gn:units (and (string-ci=? (key "Units") "unknown")
                                                     (key "Units")))
                                  (else=> default-metadata-proc))
                                (phenotype-id->id (assoc-ref row "Id"))))
                db
                (select-query ((Phenotype Id)
                               (Phenotype Pre_publication_description)
                               (Phenotype Post_publication_description)
                               (Phenotype Original_description)
                               (Phenotype Units)
                               (Phenotype Pre_publication_abbreviation)
                               (Phenotype Post_publication_abbreviation)
                               (Phenotype Lab_code)
                               (Phenotype Submitter)
                               (Phenotype Owner)
                               (Phenotype Authorized_Users))
                              (Phenotype))))

(define (dump-publication db)
  (sql-for-each (lambda (row)
                  (scm->triples (map-alist row
                                  (delete "Id")
                                  (set rdf:type 'gn:publication)
                                  (multiset gn:authors
                                            ;; The authors field is a comma
                                            ;; separated list. Split it.
                                            (map string-trim (string-split (key "Authors") #\,)))
                                  (set gn:abstract
                                       ;; TODO: Why are there unprintable characters?
                                       (delete-substrings (key "Abstract") "\x01"))
                                  (else=> default-metadata-proc))
                                (string->identifier "publication"
                                                    (number->string (assoc-ref row "Id")))))
                db
                (select-query ((Publication Id)
                               (Publication PubMed_ID)
                               (Publication Abstract)
                               (Publication Authors)
                               (Publication Title)
                               (Publication Journal)
                               (Publication Volume)
                               (Publication Pages)
                               (Publication Month)
                               (Publication Year))
                              (Publication))))

(define (dump-publish-xref db)
  (sql-for-each (lambda (row)
                  (scm->triples (map-alist row
                                  (set gn:phenotypeOfSpecies (inbred-set-name->id (key "Name"))))
                                (phenotype-id->id (assoc-ref row "PhenotypeId"))))
                db
                (select-query ((InbredSet Name)
                               (PublishXRef PhenotypeId))
                              (PublishXRef
                               (inner-join InbredSet "USING (InbredSetId)")))))

(define tissue-short-name->id
  (cut string->identifier "tissue" <>))

(define (dump-tissue db)
  ;; The Name and TissueName fields seem to be identical. BIRN_lex_ID
  ;; and BIRN_lex_Name are mostly NULL.
  (sql-for-each (lambda (row)
                  (scm->triples (map-alist row
                                  (delete "Short_Name")
                                  (set rdf:type 'gn:tissue)
                                  (set gn:name (key "Name")))
                                ;; Hopefully the Short_Name field is
                                ;; distinct and can be used as an
                                ;; identifier.
                                (tissue-short-name->id (assoc-ref row "Short_Name"))))
                db
                (select-query ((Tissue Name)
                               (Tissue Short_Name))
                              (Tissue))))

;; One email ID in the Investigators table has spaces in it. This
;; function fixes that.
(define (fix-email-id email)
  (string-delete #\space email))

(define (investigator-attributes->id first-name last-name email)
  ;; There is just one record corresponding to "Evan Williams" which
  ;; does not have an email ID. To accommodate that record, we
  ;; construct the investigator ID from not just the email ID, but
  ;; also the first and the last names. It would be preferable to just
  ;; find Evan Williams' email ID and insert it into the database.
  (string->identifier "investigator"
                      (string-join (list first-name last-name (fix-email-id email))
                                   "_")))

(define (dump-investigators db)
  (sql-for-each (lambda (row)
                  (scm->triples (map-alist row
                                  (set rdf:type 'foaf:Person)
                                  (set foaf:name (string-append (key "FirstName") " " (key "LastName")))
                                  (set foaf:givenName (key "FirstName"))
                                  (set foaf:familyName (key "LastName"))
                                  (set foaf:phone (key "Phone"))
                                  (set foaf:mbox (fix-email-id (key "Email")))
                                  (set foaf:homepage (key "Url"))
                                  (else=> default-metadata-proc))
                                (investigator-attributes->id (assoc-ref row "FirstName")
                                                             (assoc-ref row "LastName")
                                                             (assoc-ref row "Email"))))
                db
                ;; There are a few duplicate entries. We group by
                ;; email to deduplicate.
                (select-query ((Investigators FirstName)
                               (Investigators LastName)
                               (Investigators Address)
                               (Investigators City)
                               (Investigators State)
                               (Investigators ZipCode)
                               (Investigators Phone)
                               (Investigators Email)
                               (Investigators Country)
                               (Investigators Url))
                              (Investigators)
                              "GROUP BY Email")))

(define avg-method-name->id
  (cut string->identifier "avgmethod" <>))

(define (dump-avg-method db)
  (sql-for-each (lambda (row)
                  (scm->triples (map-alist row
                                  (set rdf:type 'gn:avgMethod)
                                  (set gn:name (key "Name")))
                                (avg-method-name->id (assoc-ref row "Name"))))
                db
                ;; The Name and Normalization fields seem to be the
                ;; same. Dump only the Name field.
                ;;
                ;; There are two records with Name as
                ;; "N/A". Deduplicate.
                (select-query (distinct (AvgMethod Name))
                              (AvgMethod))))

(define gene-chip-name->id
  (cut string->identifier "platform" <>))

(define (dump-gene-chip db)
  (sql-for-each (lambda (row)
                  (scm->triples (map-alist row
                                  (delete "Name")
                                  (set rdf:type 'gn:platform)
                                  (set gn:name (key "GeneChipName")))
                                (gene-chip-name->id (assoc-ref row "Name"))))
                db
                (select-query ((GeneChip GeneChipName)
                               (GeneChip Name))
                              (GeneChip))))

(define (dump-info-files db)
  (sql-for-each (lambda (row)
                  (scm->triples
                   (map-alist row
                     (set rdf:type 'gn:dataset)
                     (set gn:datasetOfInvestigator
                          (investigator-attributes->id (key "FirstName")
                                                       (key "LastName")
                                                       (key "Email")))
                     (set gn:accessionId (string-append "GN" (number->string (key "GN_AccesionId"))))
                     (set gn:datasetStatusName (string-downcase (key "DatasetStatusName")))
                     (set gn:datasetOfSpecies (binomial-name->species-id (key "BinomialName")))
                     (set gn:datasetOfInbredSet (inbred-set-name->id (key "InbredSetName")))
                     (set gn:datasetOfTissue (tissue-short-name->id (key "Short_Name")))
                     (set gn:normalization
                          (avg-method-name->id
                           ;; If AvgMethodName is NULL, assume N/A.
                           (if (string-blank? (key "AvgMethodName"))
                               "N/A" (key "AvgMethodName"))))
                     (set gn:datasetOfPlatform (gene-chip-name->id (key "GeneChip")))
                     (set gn:summary
                          ;; TODO: Why are there unprintable characters?
                          (delete-substrings (key "Summary")
                                             "\x01" "\x03"))
                     (set gn:aboutTissue
                          ;; TODO: Why are there unprintable characters?
                          (delete-substrings (key "AboutTissue")
                                             "\x01" "\x03"))
                     (set gn:geoSeries
                          (and (not (string-prefix-ci? "no geo series"
                                                       (key "GeoSeries")))
                               (key "GeoSeries")))
                     (else=> default-metadata-proc))
                   (string->identifier "dataset"
                                       (number->string (assoc-ref row "GN_AccesionId")))))
                db
                ;; TODO: Double check Platforms. It doesn't seem to
                ;; match up.
                (select-query ((InfoFiles GN_AccesionId)
                               (InfoFiles InfoFileTitle Name)
                               (InfoFiles Title)
                               (InfoFiles Specifics)
                               (DatasetStatus DatasetStatusName)
                               (Datasets DatasetName DatasetGroup)
                               (Datasets Summary)
                               (Datasets GeoSeries)
                               (Datasets AboutCases)
                               (Datasets AboutPlatform)
                               (Datasets AboutTissue)
                               (Datasets AboutDataProcessing)
                               (Datasets Notes)
                               (Datasets ExperimentDesign)
                               (Datasets Contributors)
                               (Datasets Citation)
                               (Datasets Acknowledgment)
                               (Species FullName BinomialName)
                               (InbredSet Name InbredSetName)
                               (Tissue Short_Name)
                               (Investigators FirstName)
                               (Investigators LastName)
                               (Investigators Email)
                               (AvgMethod Name AvgMethodName)
                               (GeneChip Name GeneChip))
                              (InfoFiles
                               (left-join Datasets "USING (DatasetId)")
                               (left-join DatasetStatus "USING (DatasetStatusId)")
                               (left-join Species "USING (SpeciesId)")
                               (left-join InbredSet "USING (InbredSetId)")
                               (left-join Tissue "USING (TissueId)")
                               (left-join Investigators "USING (InvestigatorId)")
                               (left-join AvgMethod "USING (AvgMethodId)")
                               (left-join GeneChip "USING (GeneChipId)"))
                              "WHERE GN_AccesionId IS NOT NULL")))

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


;;; Visualize schema

(define-immutable-record-type <table>
  (make-table name size columns)
  table?
  (name table-name)
  (size table-size)
  (columns table-columns set-table-columns))

(define (tables db)
  "Return list of all tables in DB. Each element of the returned list
is a <table> object."
  (map (lambda (table)
         (set-table-columns table
           (sql-map (cut assoc-ref <> "Field")
                    db
                    (format #f "SHOW COLUMNS FROM ~a" (table-name table)))))
       (sql-map (lambda (row)
                  (make-table (assoc-ref row "table_name")
                              ;; FIXME: This is probably correct only for
                              ;; MyISAM tables.
                              (assoc-ref row "data_length")
                              #f))
                db
                (select-query ((information_schema.tables table_name)
                               (information_schema.tables data_length))
                              (information_schema.tables)
                              (format #f "WHERE table_schema = '~a'" %database-name)))))

(define (string-remove-suffix-ci suffix str)
  "Remove SUFFIX from STR if present. Suffix check is
case-insensitive."
  (if (string-suffix-ci? suffix str)
      (substring str 0 (- (string-length str)
                          (string-length suffix)))
      str))

(define (human-units bytes)
  "Return number of BYTES as a string with human-readable units."
  (cond
   ((< bytes 1024)
    (format #f "~a B" bytes))
   ((< bytes (expt 1024 2))
    (format #f "~a KiB" (round-quotient bytes 1024)))
   ((< bytes (expt 1024 3))
    (format #f "~a MiB" (round-quotient bytes (expt 1024 2))))
   (else
    (format #f "~a GiB" (round-quotient bytes (expt 1024 3))))))

;; This wrapper function is necessary to work around a bug in (ccwl
;; graphviz) whereby backslashes in node labels are escaped and
;; printed as \\.
(define (graph->dot graph)
  (put-string (current-output-port)
              (string-replace-substring
               (call-with-output-string
                 (cut (@@ (ccwl graphviz) graph->dot) graph <>))
               "\\\\" "\\")))

(define (dump-schema db)
  (let ((tables (tables db)))
    (graph->dot
     ((@@ (ccwl graphviz) graph) 'schema
      #:nodes (map (lambda (table)
                     ((@@ (ccwl graphviz) graph-node)
                      (table-name table)
                      `((shape . "record")
                        (label . ,(format #f "{~a (~a) | ~a}"
                                          (table-name table)
                                          (human-units (table-size table))
                                          (string-replace-substring
                                           (string-replace-substring
                                            (string-join (table-columns table) "\\l" 'suffix)
                                            "<" "\\<")
                                           ">" "\\>"))))))
                   tables)
      #:edges (append-map (lambda (table)
                            (filter-map (lambda (column)
                                          (and=> (cond
                                                  ((string-prefix-ci? "StrainId" column)
                                                   'Strain)
                                                  ((let ((target-table (string-remove-suffix-ci "id" column)))
                                                     ;; Column has an "id" suffix.
                                                     (and (string-suffix-ci? "id" column)
                                                          ;; Column is not the original key.
                                                          (not (string=? (table-name table) target-table))
                                                          ;; Prefix is a table name, at least approximately.
                                                          (or (find (lambda (table)
                                                                      (string=? target-table (table-name table)))
                                                                    tables)
                                                              ;; Try deleting underscores and ignoring case.
                                                              (find (lambda (table)
                                                                      (string-ci=? (string-delete #\_ target-table)
                                                                                   (string-delete #\_ (table-name table))))
                                                                    tables)
                                                              ;; Try pluralization.
                                                              (let ((target-table (string-append target-table "s")))
                                                                (and (not (string=? (table-name table) target-table))
                                                                     (find (lambda (table)
                                                                             (string-ci=? target-table
                                                                                          (table-name table)))
                                                                           tables))))))
                                                   => table-name)
                                                  (else #f))
                                                 (cut cons (table-name table) <>)))
                                        (table-columns table)))
                          tables)))))


;; Main function

(define (prefix prefix iri)
  (format #t "@prefix ~a ~a .~%" prefix iri))

(call-with-genenetwork-database
 (lambda (db)
   (with-output-to-file (string-append %dump-directory "/dump.ttl")
     (lambda ()
       (prefix "rdf:" "<http://www.w3.org/1999/02/22-rdf-syntax-ns#>")
       (prefix "foaf:" "<http://xmlns.com/foaf/0.1/>")
       (prefix "gn:" "<http://genenetwork.org/>")
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
       (dump-info-files db)))
   (with-output-to-file (string-append %dump-directory "/schema.dot")
     (cut dump-schema db))))
