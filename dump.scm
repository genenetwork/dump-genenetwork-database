#! /usr/bin/env guile
!#

(add-to-load-path (dirname (current-filename)))

(use-modules (rnrs io ports)
             (srfi srfi-1)
             (srfi srfi-26)
             (ice-9 match)
             (ice-9 string-fun)
             (dump sql)
             (dump table)
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

(define (replace-substrings str replacement-alist)
  "Replace substrings in STR according to REPLACEMENT-ALIST, an
association list mapping substrings to their replacements."
  (fold (match-lambda*
          (((substring . replacement) str)
           (string-replace-substring str substring replacement)))
        str
        replacement-alist))

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

(define (triple subject predicate object)
  (unless (or (string? subject)
              (symbol? subject))
    (error "Triple subject not a string or symbol:"
           (list subject predicate object)))
  (unless (or (string? predicate)
              (symbol? predicate))
    (error "Triple predicate not a string or symbol:"
           (list subject predicate object)))
  (unless (or (string? object)
              (symbol? object)
              (number? object))
    (error "Triple object not a string, symbol or number:"
           (list subject predicate object)))
  (format #t "~a ~a ~s .~%" subject predicate object))

(define (field->key x)
  (translate-forms 'field
                   (lambda (x)
                     #`(key #,(symbol->string
                               (syntax->datum
                                ((syntax-rules (field)
                                   ((field table column) column)
                                   ((field table column alias) alias))
                                 x)))))
                   x))

(define (field->assoc-ref alist x)
  "Recursively translate field references in source X to (assoc-ref
ALIST field-name) forms."
  (translate-forms 'field
                   (lambda (x)
                     #`(assoc-ref #,alist
                                  #,(symbol->string
                                     (syntax->datum
                                      ((syntax-rules (field)
                                         ((field table column) column)
                                         ((field table column alias) alias))
                                       x)))))
                   x))

(define (collect-fields x)
  (map (syntax-rules (field)
         ((field reference ...)
          (reference ...)))
       (collect-forms 'field x)))

(define (find-clause clauses key)
  "Find (KEY ...) clause among CLAUSES, a list of syntax forms."
  (find (lambda (x)
          (syntax-case x ()
            ((clause-key _ ...)
             (eq? (syntax->datum #'clause-key)
                  key))))
        clauses))

(define %dumped '())

(define-syntax define-dump
  (lambda (x)
    (syntax-case x (tables schema-triples triples)
      ((_ name clauses ...)
       (let ((tables-clause (find-clause #'(clauses ...) 'tables))
             (schema-triples-clause (or (find-clause #'(clauses ...) 'schema-triples)
                                        #'(schema-triples)))
             (triples-clause (find-clause #'(clauses ...) 'triples)))
         (syntax-case triples-clause (triples)
           ((triples subject predicates ...)
            (let ((fields (collect-fields #'(subject predicates ...))))
              #`(begin
                  (set! %dumped
                        (append (list #,@(filter-map (lambda (field)
                                                       (syntax-case field (distinct)
                                                         (distinct #f)
                                                         ((table column _ ...) #'(cons 'table 'column))
                                                         (field-spec (error "Invalid field specification" #'field-spec))))
                                                     fields))
                                %dumped))
                  (define (name db)
                    #,(syntax-case schema-triples-clause (schema-triples)
                        ((schema-triples (triple-subject triple-predicate triple-object) ...)
                         #`(for-each triple
                                     (list 'triple-subject ...)
                                     (list 'triple-predicate ...)
                                     (list 'triple-object ...)))
                        (_ (error "Invalid schema triples clause:" schema-triples-clause)))
                    (sql-for-each (lambda (row)
                                    (scm->triples
                                     (map-alist row #,@(field->key #'(predicates ...)))
                                     #,(field->assoc-ref #'row #'subject)))
                                  db
                                  #,(syntax-case tables-clause (tables)
                                      ((tables tables-spec raw ...)
                                       #`(select-query #,fields tables-spec raw ...))
                                      (_ (error "Invalid tables clause:" (syntax->datum tables-clause)))))))))
           (_ (error "Invalid triples clause:" triples-clause)))))
      (_ (error "Invalid define-dump syntax:" (syntax->datum x))))))

(define binomial-name->species-id
  (cut string->identifier "species" <>))

(define-dump dump-species
  (tables (Species))
  (triples (binomial-name->species-id (field Species FullName))
    (set rdf:type 'gn:species)
    (set gn:name (field Species SpeciesName))
    (set gn:menuName (field Species MenuName))
    (set gn:binomialName (field Species FullName))))

(define-dump dump-strain
  (tables (Strain
           (join Species "ON Strain.SpeciesId = Species.SpeciesId")))
  (schema-triples
   (gn:strainOfSpecies rdfs:domain gn:strain)
   (gn:strainOfSpecies rdfs:range gn:species))
  (triples (string->identifier "strain" (field Strain Name))
    (set rdf:type 'gn:strain)
    (set gn:strainOfSpecies
         (binomial-name->species-id (field Species FullName)))
    ;; Name, and maybe a second name
    (set gn:name (field Strain Name))
    (set gn:name (field Strain Name2))
    (set gn:alias (field Strain Alias))
    (set gn:symbol (field Strain Symbol))))

;; TODO: This function is unused. Remove if not required.
(define mapping-method-name->id
  (cut string->identifier "mappingMethod" <>))

;; TODO: This function is unused. Remove if not required.
(define-dump dump-mapping-method
  (tables (MappingMethod))
  (triples (string->identifier "mappingMethod" (field MappingMethod Name))
    (set rdf:type 'gn:mappingMethod)))

(define inbred-set-name->id
  (cut string->identifier "inbredSet" <>))

(define-dump dump-inbred-set
  (tables (InbredSet
           (inner-join Species "USING (SpeciesId)")))
  (triples (inbred-set-name->id (field InbredSet Name))
    (set rdf:type 'gn:inbredSet)
    (set gn:fullName (field InbredSet FullName))
    (set gn:geneticType (field InbredSet GeneticType))
    (set gn:family (field InbredSet Family))
    (set gn:inbredSetOfSpecies
         (binomial-name->species-id (field Species FullName BinomialName)))))

(define (phenotype-id->id id)
  (string->identifier "phenotype" (number->string id)))

(define-dump dump-phenotype
  (tables (Phenotype))
  (triples (phenotype-id->id (field Phenotype Id))
    (set rdf:type 'gn:phenotype)
    (set gn:prePublicationDescription (field Phenotype Pre_publication_description))
    (set gn:postPublicationDescription (field Phenotype Post_publication_description))
    (set gn:originalDescription (field Phenotype Original_description))
    (set gn:prePublicationDescription (field Phenotype Pre_publication_abbreviation))
    (set gn:postPublicationDescription (field Phenotype Post_publication_abbreviation))
    (set gn:labCode (field Phenotype Lab_code))
    (set gn:submitter (field Phenotype Submitter))
    (set gn:owner (field Phenotype Owner))
    (set gn:authorizedUsers (field Phenotype Authorized_Users))
    (set gn:units (and (string-ci=? (field Phenotype Units) "unknown")
                       (field Phenotype Units)))))

(define-dump dump-publication
  (tables (Publication))
  (triples (string->identifier "publication"
                               (number->string (field Publication Id)))
    (set rdf:type 'gn:publication)
    (set gn:pubMedId (field Publication PubMed_ID))
    (set gn:title (field Publication Title))
    (set gn:journal (field Publication Journal))
    (set gn:volume (field Publication Volume))
    (set gn:pages (field Publication Pages))
    (set gn:month (field Publication Month))
    (set gn:year (field Publication Year))
    (multiset gn:authors
              ;; The authors field is a comma
              ;; separated list. Split it.
              (map string-trim (string-split (field Publication Authors) #\,)))
    (set gn:abstract
         ;; TODO: Why are there unprintable characters?
         (delete-substrings (field Publication Abstract)
                            "\x01"))))

(define-dump dump-publish-xref
  (tables (PublishXRef
           (inner-join InbredSet "USING (InbredSetId)")))
  (schema-triples
   (gn:phenotypeOfSpecies rdfs:domain gn:phenotype)
   (gn:phenotypeOfSpecies rdfs:range gn:species))
  (triples (phenotype-id->id (field PublishXRef PhenotypeId))
    (set gn:phenotypeOfSpecies (inbred-set-name->id (field InbredSet Name)))))

(define tissue-short-name->id
  (cut string->identifier "tissue" <>))

(define-dump dump-tissue
  ;; The Name and TissueName fields seem to be identical. BIRN_lex_ID
  ;; and BIRN_lex_Name are mostly NULL.
  (tables (Tissue))
  ;; Hopefully the Short_Name field is distinct and can be used as an
  ;; identifier.
  (triples (tissue-short-name->id (field Tissue Short_Name))
    (set rdf:type 'gn:tissue)
    (set gn:name (field Tissue Name))))

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

(define-dump dump-investigators
  ;; There are a few duplicate entries. We group by email to
  ;; deduplicate.
  (tables (Investigators)
          "GROUP BY Email")
  (triples (investigator-attributes->id (field Investigators FirstName)
                                        (field Investigators LastName)
                                        (field Investigators Email))
    (set rdf:type 'foaf:Person)
    (set foaf:name (string-append (field Investigators FirstName) " " (field Investigators LastName)))
    (set foaf:givenName (field Investigators FirstName))
    (set foaf:familyName (field Investigators LastName))
    (set foaf:phone (field Investigators Phone))
    (set foaf:mbox (fix-email-id (field Investigators Email)))
    (set foaf:homepage (field Investigators Url))
    (set gn:address (field Investigators Address))
    (set gn:city (field Investigators City))
    (set gn:state (field Investigators State))
    (set gn:zipCode (field Investigators ZipCode))
    (set gn:country (field Investigators Country))))

(define avg-method-name->id
  (cut string->identifier "avgmethod" <>))

(define-dump dump-avg-method
  ;; The Name and Normalization fields seem to be the same. Dump only
  ;; the Name field.
  ;;
  ;; There are two records with Name as "N/A". Deduplicate.
  (tables (AvgMethod)
          "GROUP BY Name")
  (triples (avg-method-name->id (field AvgMethod Name))
    (set rdf:type 'gn:avgMethod)
    (set gn:name (field AvgMethod Name))))

(define gene-chip-name->id
  (cut string->identifier "platform" <>))

(define-dump dump-gene-chip
  (tables (GeneChip))
  (triples (gene-chip-name->id (field GeneChip Name))
    (set rdf:type 'gn:platform)
    (set gn:name (field GeneChip GeneChipName))))

;; TODO: Double check Platforms. It doesn't seem to match up.
(define-dump dump-info-files
  (tables (InfoFiles
           (left-join Datasets "USING (DatasetId)")
           (left-join DatasetStatus "USING (DatasetStatusId)")
           (left-join Species "USING (SpeciesId)")
           (left-join InbredSet "USING (InbredSetId)")
           (left-join Tissue "USING (TissueId)")
           (left-join Investigators "USING (InvestigatorId)")
           (left-join AvgMethod "USING (AvgMethodId)")
           (left-join GeneChip "USING (GeneChipId)"))
          "WHERE GN_AccesionId IS NOT NULL")
  (schema-triples
   (gn:datasetOfInvestigator rdfs:domain gn:dataset)
   (gn:datasetOfInvestigator rdfs:range gn:investigator)
   (gn:datasetOfSpecies rdfs:domain gn:dataset)
   (gn:datasetOfSpecies rdfs:range gn:species)
   (gn:datasetOfInbredSet rdfs:domain gn:dataset)
   (gn:datasetOfInbredSet rdfs:range gn:inbredSet)
   (gn:datasetOfTissue rdfs:domain gn:dataset)
   (gn:datasetOfTissue rdfs:range gn:tissue)
   (gn:normalization rdfs:domain gn:dataset)
   (gn:normalization rdfs:range gn:avgMethod)
   (gn:datasetOfPlatform rdfs:domain gn:dataset)
   (gn:datasetOfPlatform rdfs:range gn:geneChip))
  (triples (string->identifier "dataset"
                               (number->string (field InfoFiles GN_AccesionId)))
    (set rdf:type 'gn:dataset)
    (set gn:datasetOfInvestigator
         (investigator-attributes->id (field Investigators FirstName)
                                      (field Investigators LastName)
                                      (field Investigators Email)))
    (set gn:accessionId (string-append "GN" (number->string
                                             (field InfoFiles GN_AccesionId))))
    (set gn:datasetStatusName (string-downcase
                               (field DatasetStatus DatasetStatusName)))
    (set gn:datasetOfSpecies (binomial-name->species-id
                              (field Species FullName BinomialName)))
    (set gn:datasetOfInbredSet (inbred-set-name->id (field InbredSet Name InbredSetName)))
    (set gn:datasetOfTissue (tissue-short-name->id (field Tissue Short_Name)))
    (set gn:normalization
         (avg-method-name->id
          ;; If AvgMethodName is NULL, assume N/A.
          (if (string-blank? (field AvgMethod Name AvgMethodName))
              "N/A" (field AvgMethod Name AvgMethodName))))
    (set gn:datasetOfPlatform (gene-chip-name->id (field GeneChip Name GeneChip)))
    (set gn:summary
         ;; TODO: Why are there unprintable characters?
         (delete-substrings (field Datasets Summary)
                            "\x01" "\x03"))
    (set gn:aboutTissue
         ;; TODO: Why are there unprintable characters?
         (delete-substrings (field Datasets AboutTissue)
                            "\x01" "\x03"))
    (set gn:geoSeries
         (and (not (string-prefix-ci? "no geo series"
                                      (field Datasets GeoSeries)))
              (field Datasets GeoSeries)))
    (set gn:name (field InfoFiles InfoFileTitle Name))
    (set gn:title (field InfoFiles Title))
    (set gn:specifics (field InfoFiles Specifics))
    (set gn:datasetGroup (field Datasets DatasetName DatasetGroup))
    (set gn:aboutCases (field Datasets AboutCases))
    (set gn:aboutPlatform (field Datasets AboutPlatform))
    (set gn:aboutDataProcessing (field Datasets AboutDataProcessing))
    (set gn:notes (field Datasets Notes))
    (set gn:experimentDesign (field Datasets ExperimentDesign))
    (set gn:contributors (field Datasets Contributors))
    (set gn:citation (field Datasets Citation))
    (set gn:acknowledgment (field Datasets Acknowledgment))))

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


;;; Dump schema

(define (tables db)
  "Return list of all tables in DB. Each element of the returned list
is a <table> object."
  (map (lambda (table)
         (set-table-columns table
           (sql-map (lambda (row)
                      (make-column (assoc-ref row "Field")
                                   (assoc-ref row "Type")))
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

(define (dumped-table? table)
  "Return non-#f if TABLE has been dumped. Else, return #f."
  (any (match-lambda
         ((dumped-table . _)
          (string=? (symbol->string dumped-table)
                    (table-name table)))
         (x (error "Malformed entry in %dumped:" x)))
       %dumped))

(define (dump-schema db)
  (let ((tables (tables db)))
    (for-each (lambda (table)
                (let ((table-id (string->identifier "table" (table-name table))))
                  (triple table-id 'rdf:type 'gn:sqlTable)
                  (triple table-id 'gn:name (table-name table))
                  (triple table-id 'gn:hasSize (table-size table))
                  (for-each (lambda (column)
                              (let ((column-id (string->identifier
                                                "field" (string-append (table-name table)
                                                                       "__" (column-name column)))))
                                (triple column-id 'rdf:type 'gn:sqlTableField)
                                (triple column-id 'gn:name (column-name column))
                                (triple column-id 'gn:sqlFieldType (column-type column))
                                (triple table-id 'gn:hasField column-id)))
                            (table-columns table))))
              tables)))


;; Main function

(define (prefix prefix iri)
  (format #t "@prefix ~a ~a .~%" prefix iri))

(call-with-genenetwork-database
 (lambda (db)
   (with-output-to-file (string-append %dump-directory "/dump.ttl")
     (lambda ()
       (prefix "rdf:" "<http://www.w3.org/1999/02/22-rdf-syntax-ns#>")
       (prefix "rdfs:" "<http://www.w3.org/2000/01/rdf-schema#>")
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
       (dump-info-files db)
       (dump-schema db)))))
