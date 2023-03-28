#! /usr/bin/env guile
!#

(use-modules (rnrs programs)
             (rnrs io ports)
             (srfi srfi-1)
             (srfi srfi-26)
             (srfi srfi-171)
             (ice-9 match)
             (ice-9 regex)
             (ice-9 string-fun)
             (dump sql)
             (dump table)
             (dump triples)
             (dump utils)
             (zlib))


;;; GeneNetwork database connection parameters and dump path

(define %connection-settings
  (call-with-input-file (list-ref (command-line) 1)
    read))

(define (call-with-genenetwork-database proc)
  (call-with-database "mysql" (string-join
                               (list (assq-ref %connection-settings 'sql-username)
                                     (assq-ref %connection-settings 'sql-password)
                                     (assq-ref %connection-settings 'sql-database)
                                     "tcp"
                                     (assq-ref %connection-settings 'sql-host)
                                     (number->string
                                      (assq-ref %connection-settings 'sql-port)))
                               ":")
                      proc))

(define %dump-directory
  (list-ref (command-line) 2))

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

(define (annotate-field field schema)
  (let ([schema (cond ((symbol? schema)
                       (symbol->string schema))
                      ((string? schema) schema)
                      (else
                       (error "Use a string/symbol")))])
    (string->symbol
     (format #f "~s~a" field schema))))

(define (string-split-substring str substr)
  "Split the string @var{str} into a list of substrings delimited by the
substring @var{substr}."

  (define substrlen (string-length substr))
  (define strlen (string-length str))

  (define (loop index start)
    (cond
     ((>= start strlen) (list ""))
     ((not index) (list (substring str start)))
     (else
      (cons (substring str start index)
            (let ((new-start (+ index substrlen)))
              (loop (string-contains str substr new-start)
                    new-start))))))

  (cond
   ((string-contains str substr) => (lambda (idx) (loop idx 0)))
   (else (list str))))

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

(eval-when (expand load eval)
  (define (field->key x)
    (translate-forms 'field
                     (lambda (x)
                       #`(key #,(symbol->string
                                 (syntax->datum
                                  ((syntax-rules (field)
                                     ((field (query alias)) alias)
                                     ((field table column) column)
                                     ((field table column alias) alias)
                                     ((field table column operation alias) alias))
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
                                           ((field (query alias)) alias)
                                           ((field table column) column)
                                           ((field table column alias) alias)
                                           ((field table column operation alias) alias))
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

  (define (remove-namespace str)
    "Remove RDF namespace prefix from STR."
    (substring str (1+ (string-index str #\:))))

  (define (column-id table-name column-name)
    (string->identifier
     "field" (string-append
              ;; We downcase table names in identifiers. So, we
              ;; distinguish between the user and User tables.
              (if (string=? table-name "User")
                  "user2" table-name)
              "__" column-name)))

  (define (dump-id dump-table predicate)
    (symbol->string
     (string->identifier
      "dump"
      (string-append
       dump-table "_" (remove-namespace (symbol->string predicate)))))))

(define-syntax blank-node
  (syntax-rules ()
    "Allow having set and multiset within the context of a blank-node"
    [(_ (op predicate object) ...)
     (let [(node (string-join
                  (filter-map (match-lambda
                                ((pred . obj)
                                 (match obj
                                   ((and (?  string? obj)
                                         (?  string-null? obj))
                                    #f)
                                   ((?  symbol? obj)
                                    (format #f "~a ~a" pred (symbol->string obj)))
                                   (_
                                    (format #f "~a ~s" pred obj)))))
                              (map-alist '()
                                (op predicate object) ...))
                  " ; "))]
       (if (string-null? node)
           ""
           (format #f "[ ~a ]" node)))]))

(define-syntax syntax-let
  (syntax-rules ()
    "Like match-let, but for syntax.

(syntax-let ((pattern literals expression))
  body ...)
≡
(syntax-case expression ()
  (pattern literals
   body ...))

literals is optional. So,

(syntax-let ((pattern expression))
  body ...)
≡
(syntax-case expression ()
  (pattern
   body ...))
"
    ((_ () body ...)
     (begin body ...))
    ((_ ((pattern expression)
         bindings ...)
        body ...)
     (syntax-case expression ()
       (pattern
        (syntax-let (bindings ...)
          body ...))))
    ((_ ((pattern literals expression)
         bindings ...)
        body ...)
     (syntax-case expression literals
       (pattern
        (syntax-let (bindings ...)
          body ...))))))

(define-syntax define-dump
  (lambda (x)
    "Define FUNCTION-NAME, a function that dumps a view of the database.

define-dump consists of three order-agnostic clauses---tables,
schema-triples and triples---in the form shown below.

(define-dump function-name
  (tables (table ...) raw-forms ...)
  (schema-triples
   (subject predicate object) ...)
  (triples subject
    (verb predicate object) ...))

The tables clause specifies the database tables to be joined to
construct the view to be dumped. TABLE must be either of the form
TABLE-NAME or of the form (JOIN-OPERATOR TABLE-NAME
RAW-CONDITION). TABLE-NAME must, obviously, be the name of the
table. JOIN-OPERATOR must be one of join, left-join and
inner-join. RAW-CONDITION should be the join condition as a raw
string. This is usually something like
\"USING (SpeciesId)\". RAW-FORMS are expressions that must evaluate to
strings to be appended to the SQL query.

The schema-triples clause specifies the list of triples to be written
once when the dump starts.

The triples clause specifies the triples to be dumped once for each
row in the view. All triples have a common SUBJECT. The (verb
predicate object) clauses are described below.

VERB must either be set or multiset. For the set VERB, a single triple
(SUBJECT PREDICATE OBJECT-VALUE) is written where OBJECT-VALUE is the
result of evaluating OBJECT. For the multiset VERB, OBJECT must
evaluate to a list, and a triple (SUBJECT PREDICATE
OBJECT-VALUE-ELEMENT) is created for each element OBJECT-VALUE-ELEMENT
of that list.

The SUBJECT and OBJECT expressions in the triples clause must
reference database fields using a (field TABLE COLUMN) clause where
TABLE and COLUMN refer to the table and column of the field being
referenced. Database fields can also be referenced using (field TABLE
COLUMN ALIAS) where ALIAS is an alias for that column in the SQL
query. Specification of ALIAS is indeed a leak in the abstraction, and
must be remedied."
    (syntax-case x (tables schema-triples triples)
      ((_ name clauses ...)
       (syntax-let (((tables (primary-table other-tables ...) tables-raw ...) (tables)
                     (find-clause #'(clauses ...) 'tables))
                    (schema-triples-clause (or (find-clause #'(clauses ...) 'schema-triples)
                                               #'(schema-triples)))
                    ((triples subject predicate-clauses ...) (triples)
                     (find-clause #'(clauses ...) 'triples)))
         #`(define* (name db #:optional (table-metadata? #f))
             #,(syntax-case #'schema-triples-clause (schema-triples)
                 ((schema-triples (triple-subject triple-predicate triple-object) ...)
                  #`(for-each triple
                              (list 'triple-subject ...)
                              (list 'triple-predicate ...)
                              (list 'triple-object ...)))
                 (_ (error "Invalid schema triples clause:" #'schema-triples-clause)))
             (when table-metadata?
               #,@(let ((dump-table (symbol->string (syntax->datum #'primary-table)))
                        (subject-type (any (lambda (predicate)
                                             (syntax-case predicate (rdf:type)
                                               ((_ rdf:type type) #'type)
                                               (_ #f)))
                                           #'(predicate-clauses ...))))
                    (map (lambda (predicate-clause)
                           (syntax-case predicate-clause ()
                             ((_ predicate _)
                              ;; Dump metadata about the dump itself.
                              #`(begin
                                  (scm->triples
                                   (map-alist '()
                                     (set rdf:type 'gn:dump)
                                     (set gn:createsPredicate 'predicate)
                                     (filter-set gn:forSubjectType #,subject-type)
                                     (multiset gn:dependsOn
                                               '#,(map (lambda (field)
                                                         (match (syntax->datum field)
                                                           ((table-name column-name _ ...)
                                                            (datum->syntax
                                                             x (column-id (symbol->string table-name)
                                                                          (symbol->string column-name))))
                                                           (((query alias))
                                                            (datum->syntax
                                                             x (column-id query (symbol->string alias))))))
                                                       (collect-fields predicate-clause))))
                                   #,(dump-id dump-table (syntax->datum #'predicate)))
                                  ;; Automatically create domain triples
                                  ;; for predicates.
                                  (when #,subject-type
                                    (triple 'predicate 'rdfs:domain #,subject-type))))
                             (_ (error "Invalid predicate clause:" predicate-clause))))
                         #'(predicate-clauses ...))))
             (sql-for-each (lambda (row)
                             (scm->triples
                              (map-alist row #,@(field->key #'(predicate-clauses ...)))
                              #,(field->assoc-ref #'row #'subject)))
                           db
                           (select-query #,(collect-fields #'(subject predicate-clauses ...))
                                         (primary-table other-tables ...)
                                         tables-raw ...)))))
      (_ (error "Invalid define-dump syntax:" (syntax->datum x))))))

(define binomial-name->species-id
  (cut string->identifier "species" <>))

(define-dump dump-species
  (tables (Species))
  (schema-triples
   (gn:name rdfs:range rdfs:Literal)
   (gn:menuname rdfs:range rdfs:Literal)
   (gn:binomialName rdfs:range rdfs:Literal))
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
   (gn:strainOfSpecies rdfs:range gn:species)
   (gn:name rdfs:range rdfs:Literal)
   (gn:alias rdfs:range rdfs:Literal)
   (gn:symbol rdfs:range rdfs:Literal))
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
           (left-join Species "ON InbredSet.SpeciesId=Species.Id")
           (left-join MappingMethod
                       "ON InbredSet.MappingMethodId=MappingMethod.Id")))
  (schema-triples
   (gn:fullName rdfs:range rdfs:Literal)
   (gn:geneticType rdfs:range rdfs:Literal)
   (gn:family rdfs:range rdfs:Literal)
   (gn:inbredSetOfSpecies rdfs:range gn:species)
   (gn:inbredSetOfMappingMethod rdfs:range gn:mappingMethod))
  (triples (inbred-set-name->id (field InbredSet Name))
    (set rdf:type 'gn:inbredSet)
    (set gn:fullName (field InbredSet FullName))
    (set gn:geneticType (field InbredSet GeneticType))
    (set gn:family (field InbredSet Family))
    (set gn:inbredSetOfMappingMethod (field MappingMethod Name))
    (set gn:inbredSetOfSpecies
         (binomial-name->species-id (field Species FullName BinomialName)))))

;; Metadata for published datasets
(define-dump dump-publishfreeze
  (tables (PublishFreeze
           (left-join InbredSet "USING (InbredSetId)")))
  (schema-triples
   (gn:datasetOfInbredSet rdfs:range gn:inbredSet)
   (gn:name rdfs:range rdfs:Literal)
   (gn:fullName rdfs:range rdfs:Literal)
   (gn:shortName rdfs:range rdfs:Literal)
   (gn:createTime rdfs:range rdfs:Literal))
  (triples (string->identifier "dataset" (field PublishFreeze Name))
    (set rdf:type 'gn:dataset)
    (set gn:name (field PublishFreeze Name))
    (set gn:fullName (field PublishFreeze FullName))
    (set gn:shortName (field PublishFreeze ShortName))
    (set gn:createTime (field PublishFreeze CreateTime))
    (set gn:datasetOfInbredSet
         (inbred-set-name->id (field InbredSet Name InbredSetName)))))

;; Phenotype metadata
(define-dump dump-published-phenotypes
  (tables (PublishXRef
           (inner-join
            Phenotype
            "ON PublishXRef.PhenotypeId = Phenotype.Id")
           (inner-join
            Publication
            "ON PublishXRef.PublicationId =
Publication.Id")
           (inner-join PublishFreeze "USING (InbredSetId)"))
          "WHERE PublishFreeze.public > 0 AND PublishFreeze.confidentiality < 1")
  (schema-triples
   (gn:prePublicationDescription rdfs:range rdfs:Literal)
   (gn:postPublicationDescription rdfs:range rdfs:Literal)
   (gn:originalDescription rdfs:range rdfs:Literal)
   (gn:units rdfs:range rdfs:Literal)
   (gn:prePublicationAbbreviation rdfs:range rdfs:Literal)
   (gn:postPublicationAbbreviation rdfs:range rdfs:Literal)
   (gn:labCode rdfs:range rdfs:Literal)
   (gn:submitter rdfs:range rdfs:Literal)
   (gn:owner rdfs:range rdfs:Literal)
   (gn:phenotypeOfDataset rdfs:range gn:dataset)
   (gn:pubMedId rdfs:range rdfs:Literal)
   (gn:publicationId rdfs:range gn:publication)
   (gn:mean rdfs:range rdfs:Literal)
   (gn:locus rdfs:range rdfs:Literal)
   (gn:lrs rdfs:range rdfs:Literal)
   (gn:additive rdfs:range rdfs:Literal)
   (gn:sequence rdfs:range rdfs:Literal)
   (gn:comments rdfs:range rdfs:Literal))
  ;; In GN, a given trait is identified by the id of the PublishXRef!
  (triples (string->identifier "phenotype"
                               (number->string
                                (field PublishXRef Id)))
    (set rdf:type 'gn:phenotype)
    (set rdf:type 'gn:phenotype)
    (set gn:prePublicationDescription (field Phenotype Pre_publication_description))
    (set gn:prePublicationDescription (field Phenotype Pre_publication_description))
    (set gn:postPublicationDescription (field Phenotype Post_publication_description))
    (set gn:originalDescription (field Phenotype Original_description))
    (set gn:units (field Phenotype Units))
    (set gn:prePublicationAbbreviation (field Phenotype Pre_publication_description))
    (set gn:postPublicationAbbreviation (field Phenotype Post_publication_abbreviation))
    (set gn:labCode (field Phenotype Lab_code))
    (multiset gn:submitter
              (map string-trim (string-split
                                (field Phenotype Submitter) #\,)))
    (multiset gn:owner
              (map string-trim (string-split
                                (field Phenotype Owner) #\,)))
    (set gn:pubMedId (field Publication PubMed_ID))
    (set gn:publicationId
         (string->identifier
          "publication"
          (number->string (field Publication Id))))
    (set gn:mean (field PublishXRef mean))
    (set gn:locus (field PublishXRef Locus))
    (set gn:lrs (field PublishXRef LRS))
    (set gn:additive (field PublishXRef additive))
    (set gn:sequence (field PublishXRef Sequence))
    (set gn:comments (field PublishXRef comments))
    (set gn:phenotypeOfDataset
         (string->identifier "dataset"
                             (field PublishFreeze Name)))))


(define-dump dump-publication
  (tables (Publication))
  (schema-triples
   (gn:pubMedId rdfs:range rdfs:Literal)
   (gn:title rdfs:range rdfs:Literal)
   (gn:journal rdfs:range rdfs:Literal)
   (gn:volume rdfs:range rdfs:Literal)
   (gn:pages rdfs:range rdfs:Literal)
   (gn:month rdfs:range rdfs:Literal)
   (gn:year rdfs:range rdfs:Literal)
   (gn:author rdfs:range rdfs:Literal)
   (gn:abstract rdfs:range rdfs:Literal))
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
    (multiset gn:author
              ;; The authors field is a comma
              ;; separated list. Split it.
              (map string-trim (string-split (field Publication Authors) #\,)))
    (set gn:abstract
         ;; TODO: Why are there unprintable characters?
         (delete-substrings (field Publication Abstract)
                            "\x01"
                            ;; \v is a vertical tab
                            ;; character. Microsoft Word probably
                            ;; still uses this.
                            "\v"))))


(define tissue-short-name->id
  (cut string->identifier "tissue" <>))

(define-dump dump-tissue
  ;; The Name and TissueName fields seem to be identical. BIRN_lex_ID
  ;; and BIRN_lex_Name are mostly NULL.
  (tables (Tissue))
  (schema-triples
   (gn:name rdfs:range rdfs:Literal))
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
                      (string-join
                       ;; Add special case for Yohan Bossé whose name
                       ;; has unprintable characters.
                       ;; TODO: Fix Yohan Bossé's name in the database.
                       (let ((last-name (if (string=? first-name "Yohan")
                                            "Bosse"
                                            last-name)))
                         (list first-name last-name (fix-email-id email)))
                       "_")))

(define-dump dump-investigators
  ;; There are a few duplicate entries. We group by email to
  ;; deduplicate.
  (tables (Investigators)
          "GROUP BY Email")
  (schema-triples
   ;; TODO: Are ranges required for FOAF predicates? Can they not be
   ;; obtained from the FOAF spec?
   (foaf:name rdfs:range rdfs:Literal)
   (foaf:givenName rdfs:range rdfs:Literal)
   (foaf:familyName rdfs:range rdfs:Literal)
   (foaf:phone rdfs:range rdfs:Literal)
   (foaf:mbox rdfs:range rdfs:Literal)
   (foaf:homepage rdfs:range rdfs:Literal)
   (gn:address rdfs:range rdfs:Literal)
   (gn:city rdfs:range rdfs:Literal)
   (gn:state rdfs:range rdfs:Literal)
   (gn:zipCode rdfs:range rdfs:Literal)
   (gn:country rdfs:range rdfs:Literal))
  (triples (investigator-attributes->id (field Investigators FirstName)
                                        (field Investigators LastName)
                                        (field Investigators Email))
    (set rdf:type 'foaf:Person)
    ;; Special case Yohan Bossé's name since the last name has
    ;; unprintable characters.
    (set foaf:name (string-append (field Investigators FirstName) " "
                                  (if (string=? (field Investigators FirstName) "Yohan")
                                      "Bossé"
                                      (field Investigators LastName))))
    (set foaf:givenName (field Investigators FirstName))
    ;; Special case Yohan Bossé's name since the last name has
    ;; unprintable characters.
    (set foaf:familyName (if (string=? (field Investigators FirstName) "Yohan")
                             "Bossé"
                             (field Investigators LastName)))
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
  (tables (AvgMethod))
  (schema-triples
   (gn:name rdfs:range rdfs:Literal))
  (triples (avg-method-name->id (field AvgMethod Name))
    (set rdf:type 'gn:avgMethod)
    (set gn:name (field AvgMethod Name))))

(define gene-chip-name->id
  (cut string->identifier "platform" <>))

(define-dump dump-gene-chip
  (tables (GeneChip))
  (schema-triples
   (gn:name rdfs:range rdfs:Literal))
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
   (gn:datasetOfInvestigator rdfs:range foaf:Person)
   (gn:datasetOfSpecies rdfs:domain gn:dataset)
   (gn:datasetOfSpecies rdfs:range gn:species)
   (gn:datasetOfInbredSet rdfs:domain gn:dataset)
   (gn:datasetOfInbredSet rdfs:range gn:inbredSet)
   (gn:datasetOfTissue rdfs:domain gn:dataset)
   (gn:datasetOfTissue rdfs:range gn:tissue)
   (gn:normalization rdfs:domain gn:dataset)
   (gn:normalization rdfs:range gn:avgMethod)
   (gn:datasetOfPlatform rdfs:domain gn:dataset)
   (gn:datasetOfPlatform rdfs:range gn:geneChip)
   (gn:accessionId rdfs:range rdfs:Literal)
   (gn:datasetStatusName rdfs:range rdfs:Literal)
   (gn:summary rdfs:range rdfs:Literal)
   (gn:aboutTissue rdfs:range rdfs:Literal)
   (gn:geoSeries rdfs:range rdfs:Literal)
   (gn:name rdfs:range rdfs:Literal)
   (gn:title rdfs:range rdfs:Literal)
   (gn:specifics rdfs:range rdfs:Literal)
   (gn:datasetGroup rdfs:range rdfs:Literal)
   (gn:aboutCases rdfs:range rdfs:Literal)
   (gn:aboutPlatform rdfs:range rdfs:Literal)
   (gn:aboutDataProcessing rdfs:range rdfs:Literal)
   (gn:notes rdfs:range rdfs:Literal)
   (gn:experimentDesign rdfs:range rdfs:Literal)
   (gn:contributors rdfs:range rdfs:Literal)
   (gn:citation rdfs:range rdfs:Literal)
   (gn:acknowledgment rdfs:range rdfs:Literal))
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
    (set gn:name (field InfoFiles InfoPageName))
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
                              (format #f "WHERE table_schema = '~a'"
                                      (assq-ref %connection-settings 'sql-database))))))

(define (dump-schema db)
  (let ((tables (tables db)))
    (for-each (lambda (table)
                (let ((table-id (string->identifier
                                 "table"
                                 ;; We downcase table names in
                                 ;; identifiers. So, we distinguish
                                 ;; between the user and User tables.
                                 (if (string=? (table-name table) "User")
                                     "user2"
                                     (table-name table)))))
                  (triple table-id 'rdf:type 'gn:sqlTable)
                  (triple table-id 'gn:name (table-name table))
                  (triple table-id 'gn:hasSize (table-size table))
                  (for-each (lambda (column)
                              (let ((column-id (column-id (table-name table)
                                                          (column-name column))))
                                (triple column-id 'rdf:type 'gn:sqlTableField)
                                (triple column-id 'gn:name (column-name column))
                                (triple column-id 'gn:sqlFieldType (column-type column))
                                (triple table-id 'gn:hasField column-id)))
                            (table-columns table))))
              tables)))

(define-dump dump-groups
  (tables (InbredSet
           (left-join Species "USING (SpeciesId)"))
          (string-join
           '("WHERE Species.Name IN "
             "(SELECT Name FROM Species ORDER BY OrderId) "
             "GROUP BY InbredSet.Name "
             "ORDER BY "
             "IFNULL(InbredSet.FamilyOrder, InbredSet.FullName) "
             "ASC, IFNULL(InbredSet.Family, InbredSet.FullName) "
             "ASC, InbredSet.FullName ASC, InbredSet.MenuOrderId ASC")))
  (schema-triples
   (gn:name rdfs:range rdfs:Literal)
   (gn:binomialName rdfs:range rdfs:Literal)
   (gn:species rdfs:range gn:species))
  (triples (string->identifier "inbredSet" (field InbredSet Name))
    (set gn:name (field InbredSet Name))
    (set gn:binomialName (field InbredSet fullName))
    (set gn:species (field Species Name))))

;; GeneRIF metadata
(define-dump dump-gn-genewiki-entries
  (tables (GeneRIF_BASIC
           (left-join GeneRIF "USING (symbol)")
           (left-join GeneRIFXRef "ON GeneRIFXRef.GeneRIFId = GeneRIF.Id")
           (left-join GeneCategory "ON GeneRIFXRef.GeneCategoryId = GeneCategory.Id"))
          "WHERE GeneRIF.display > 0 and GeneRIF.VersionId = 0 GROUP BY GeneRIF.symbol")
  (schema-triples
   (gn:geneWikiEntry rdfs:domain gn:geneWiki)
   (gn:geneWikiEntryOfGN rdfs:domain gn:geneWiki)
   (gn:geneWikiEntryofNCBI rdfs:domain gn:geneWiki)
   (gn:weburl rdfs:domain gn:geneWiki)
   (gn:versionId rdfs:domain gn:geneWiki)
   (gn:category rdfs:domain gn:geneWiki)
   (gn:pubMedId rdfs:domain rdfs:Literal)
   (gn:createTime rdfs:range xsd:datetime))
  (triples (ontology 'generif:
                      (field GeneRIF_BASIC GeneId))
    (set rdf:type 'gn:geneWikiEntry)
    (set gn:symbol (field GeneRIF symbol))
    (multiset gn:geneWikiEntryOfGn
              (let* ([entries (replace-substrings (field
                                                   ("GROUP_CONCAT(DISTINCT CONCAT_WS('::::', IFNULL(GeneCategory.Name, ''), IFNULL(GeneRIF.PubMed_ID, ''), GeneRIF.email, GeneRIF.comment, GeneRIF.createtime, IFNULL(weburl, '')) SEPARATOR';;;;;')"
                                                    wikientry))
                                                  '(("\x28" . "")
                                                    ("\x29" . "")
                                                    ("\xa0" . " ")
                                                    ("â\x81„" . "/")
                                                    ("â€\x9d" . #\")
                                                    ("â€™" . #\')
                                                    ("\x02" . "")
                                                    ("\x01" . "")
                                                    ("Î²" . "β")
                                                    ("Î±-Â\xad" . "α")
                                                    ("Â\xad" . "")
                                                    ("Î±" . "α")
                                                    ("â€“" . "-")))]
                     [comments (string-split-substring entries ";;;;;")])
                (map
                 (match-lambda
                   ((genecategory pmid email text createtime weburl)
                    (blank-node
                     (set gn:category genecategory)
                     (multiset gn:pubMedId
                               (map (lambda (el) (if (string-null? el)
                                                     ""
                                                     (ontology 'pubmed: el)))
                                    (string-split pmid #\space)))
                     (set gn:author (regexp-substitute/global #f "@.*$"
                                               email
                                               'pre
                                               ""
                                               'post))
                     (set gn:geneWikiEntry
                          (annotate-field text '^^xsd:string))
                     (set gn:createTime (annotate-field
                                         createtime
                                         '^^xsd:datetime))
                     (set gn:weburl weburl))))
                 (map
                  (cut string-split-substring <> "::::")
                  comments))))
    (multiset gn:geneWikiEntryOfNCBI
              (let* ([entries (field
                               ("GROUP_CONCAT(DISTINCT CONCAT_WS('::::', IFNULL(GeneRIF_BASIC.PubMed_ID, ''), IFNULL(GeneRIF_BASIC.comment, '')) SEPARATOR'|||||')"
                                ncbientry))
                              ]
                     [ncbi-comments (string-split-substring entries "|||||")])
                (map
                 (match-lambda
                   ((pmid text)
                    (blank-node
                     (set gn:geneWikiEntry (annotate-field text '^^xsd:string))
                     (set gn:pubMedId (ontology 'pubmed: pmid))))
                   (_ (display (string-split-substring ncbi-comments "::::"))
                      (error "error")))
                 (map
                  (cut string-split-substring <> "::::")
                  ncbi-comments))))
    ))


;; Main function

(call-with-genenetwork-database
 (lambda (db)
   (with-output-to-file (string-append %dump-directory "/dump.ttl")
     (lambda ()
       (prefix "rdf:" "<http://www.w3.org/1999/02/22-rdf-syntax-ns#>")
       (prefix "rdfs:" "<http://www.w3.org/2000/01/rdf-schema#>")
       (prefix "foaf:" "<http://xmlns.com/foaf/0.1/>")
       (prefix "gn:" "<http://genenetwork.org/>")
       (prefix "pubmed:" "<http://rdf.ncbi.nlm.nih.gov/pubmed/>")
       (prefix "taxon:" "<http://purl.uniprot.org/taxonomy/>")
       (prefix "generif:" "<https://www.ncbi.nlm.nih.gov/gene?Db=gene&Cmd=DetailsSearch&Term=>")
       (prefix "xsd:" "<http://www.w3.org/2001/XMLSchema#>")
       (prefix "owl:" "<http://www.w3.org/2002/07/owl#>")
       (newline)
       (dump-gn-genewiki-entries db)
       (dump-species db)
       (dump-strain db)
       (dump-mapping-method db)
       (dump-inbred-set db)
       (dump-publishfreeze db)
       (dump-publication db)
       (dump-tissue db)
       (dump-investigators db)
       (dump-avg-method db)
       (dump-gene-chip db)
       (dump-info-files db)
       (dump-schema db)
       (dump-groups db)
       (dump-published-phenotypes db)))))

