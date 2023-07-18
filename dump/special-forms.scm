(define-module (dump special-forms)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-26)
  #:use-module (dump sql)
  #:use-module (dump table)
  #:use-module (dump triples)
  #:export (translate-forms
            collect-forms
            collect-keys
            field->key
            field->assoc-ref
            collect-fields
            find-clause
            remove-namespace
            column-id
            dump-id
            syntax-let
            blank-node
            map-alist
	    dump-with-documentation
            define-dump))

(define (key->assoc-ref alist x)
  "Recursively translate (key k) forms in source X to (assoc-ref ALIST
k) forms."
  (translate-forms 'key
                   ;; (syntax-rules (key)
                   ;;   ((key k) (assoc-ref alist k)))
                   (lambda (x)
                     (syntax-case x (key)
                       ((key k) #`(assoc-ref #,alist 'k))))
                   x))

(define (alist-delete* alist keys)
  "Delete entries from ALIST whose key is equal (in the sense of
equal?) to any in KEYS, a list."
  (remove (match-lambda
            ((key . value)
             (member key keys))
            (x (error "malformed alist element" x)))
          alist))

(define (translate-forms from translator x)
  "Recursively pass (FROM ...) forms in source X to TRANSLATOR, and
replace them with the return value."
  (syntax-case x ()
    ;; Handle translation base case.
    ((head tail ...) (eq? (syntax->datum #'head)
                          from)
     (translator x))
    ;; Recurse.
    ((head tail ...)
     (cons (translate-forms from translator #'head)
           (map (cut translate-forms from translator <>)
                #'(tail ...))))
    ;; Handle leaf base case.
    (leaf #'leaf)))

(define (collect-forms target x)
  "Recursively collect (TARGET ...) forms in source X and return them
as a list."
  (syntax-case x ()
    ;; Handle collection base case.
    ((head tail ...) (eq? (syntax->datum #'head)
                          target)
     (list x))
    ;; Recurse.
    ((head tail ...)
     (append (collect-forms target #'head)
             (append-map (cut collect-forms target <>) #'(tail ...))))
    ;; Handle leaf base case.
    (leaf (list))))

(define (collect-keys x)
  "Recursively collect (key k) forms from source X and return as a
list of all K."
  (map (syntax-rules (key)
         ((key k) 'k))
       (collect-forms 'key x)))

(define-syntax map-alist
  (lambda (x)
    "Transform (aka map) ALIST, an association list, into another
association list. The returned association list may contain multiple
associations for the same key. equal? is used in all association list
key comparisons.

Syntax:

(map-alist alist
  (verb key expression) ...
  (else=> proc))

VERB must be one of set, filter-set, multiset and remove.

For the set VERB, KEY is set to the result of evaluating
EXPRESSION. Multiple set verbs on the same key will result in multiple
associations for that key.

For the filter-set VERB, KEY is set to the result of evaluating
EXPRESSION only if that result is not #f.

For the multiset VERB, EXPRESSION must return a list and KEY is
associated multiple times once with each element of the returned list.

For the remove VERB, KEY is discarded.

EXPRESSIONs must reference elements of ALIST using (key k) forms where
K is the key to be referenced from ALIST. K must not be quoted. That
is, if K is the symbol 'bar, it must be referenced as (key bar), not
as (key 'bar).

The else=> clause is optional.

If the else=> clause is present, PROC is passed all pairs of ALIST
that are not set by an earlier (verb key expression) action. PROC must
return #f or a pair to replace its input pair. If PROC returns #f,
that pair is discarded.

If the else=> clause is absent, all unset pairs are discarded.

Example:

(map-alist '((\"foo\" . 1)
             (bar . 2)
             (foobar . 5)
             (fubar . 3))
  (set spam (1+ (key \"foo\")))
  (set ham (* 2 (key bar)))
  (set eggs (* 3 (key bar)))
  (set aal (+ (key \"foo\")
              (key bar)))
  (multiset vel (iota (* 2 (key bar))))
  (remove foobar)
  (else=> (match-lambda
            ((key . value)
             (cons key (expt 2 value))))))
=> ((spam . 2) (ham . 4) (eggs . 6) (aal . 3)
    (vel . 0) (vel . 1) (vel . 2) (vel . 3) (fubar . 8))"
    (syntax-case x ()
      ((_ alist actions ...)
       ;; TODO: Check that all actions are valid.
       #`(let ((evaluated-alist alist))
           (append (remove (match-lambda
                             ;; Filter out results of filter-set actions.
                             ((key . #f)
                              (member key '#,(filter-map (lambda (action)
                                                           (syntax-case action (filter-set)
                                                             ((filter-set key expression) #'key)
                                                             (_ #f)))
                                                         #'(actions ...))))
                             (_ #f))
                           ;; Do set and filter-set.
                           `#,(filter-map (lambda (action)
                                            (syntax-case action (set filter-set)
                                              ((set key expression)
                                               #`(key . ,#,(key->assoc-ref #'evaluated-alist #'expression)))
                                              ((filter-set key expression)
                                               #`(key . ,#,(key->assoc-ref #'evaluated-alist #'expression)))
                                              (_ #f)))
                                          #'(actions ...)))
                   ;; Do multiset.
                   #,@(filter-map (lambda (action)
                                    (syntax-case action (multiset)
                                      ((multiset key expression)
                                       #`(map (cut cons 'key <>)
                                              #,(key->assoc-ref #'evaluated-alist #'expression)))
                                      (_ #f)))
                                  #'(actions ...))
                   ;; Apply else=> procedure on unspecified keys. If
                   ;; no else=> procedure is specified, delete
                   ;; unspecified keys.
                   (filter-map #,(or (any (lambda (action)
                                            (syntax-case action (else=>)
                                              ((else=> proc) #'proc)
                                              (_ #f)))
                                          #'(actions ...))
                                     #'(const #f))
                               ;; The unspecified part of the input
                               ;; alist
                               (alist-delete* evaluated-alist
                                              (list
                                               ;; Keys that were referenced
                                               #,@(append-map (lambda (action)
                                                                (syntax-case action ()
                                                                  ((_ key expression)
                                                                   (collect-keys #'expression))
                                                                  (_ '())))
                                                              #'(actions ...))
                                               ;; Keys that were deleted
                                               #,@(filter-map (lambda (action)
                                                                (syntax-case action (remove)
                                                                  ((remove key) #''key)
                                                                  (_ #f)))
                                                              #'(actions ...))
                                               ;; Keys that were set
                                               #,@(filter-map (lambda (action)
                                                                (syntax-case action ()
                                                                  ((_ key expression) #''key)
                                                                  (_ #f)))
                                                              #'(actions ...)))))))))))



(eval-when (expand load )
  (define (field->datum x)
    (translate-forms
     'field
     (lambda (x)
       (syntax-case x (field)
         ((field (query alias))
          #`(format #f "~a" (syntax->datum #'alias)))
         ((field table column)
          #`(format #f "~a(~a)"
                    (syntax->datum #'table)
                    (syntax->datum #'column)))
         ((field table column alias)
          #`(format #f "~a(~a)"
                    (syntax->datum #'table)
                    (syntax->datum #'alias)))))
     x))

  (define (field->key x)
    (translate-forms 'field
                     (lambda (x)
                       #`(key #,(symbol->string
                                 (syntax->datum
                                  ((syntax-rules (field)
                                     ((field (query alias)) alias)
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
                                           ((field (query alias)) alias)
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
         #`(define* (name db
			  #:optional
                          (dump-metadata? #f)
                          (dump-data? #t)
                          (dump-documentation? #f))
             (when dump-data?
               #,(syntax-case #'schema-triples-clause (schema-triples)
                   ((schema-triples (triple-subject triple-predicate triple-object) ...)
                    #`(for-each triple
                                (list 'triple-subject ...)
                                (list 'triple-predicate ...)
                                (list 'triple-object ...)))
                   (_ (error "Invalid schema triples clause:" #'schema-triples-clause))))
             (when dump-metadata?
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
             (when dump-documentation?
               (format #t "~%## '~a'~%~%" (syntax->datum #'name))
               #,(syntax-case #'schema-triples-clause (schema-triples)
                   ((schema-triples (triple-subject triple-predicate triple-object) ...)
                    #`(begin
                        (format #t "## Schema Triples:~%~%```text~%")
                        (for-each (lambda (s p o)
                                    (format #t "~a -> ~a -> ~a~%" s p o))
                                  (list 'triple-subject ...)
                                  (list 'triple-predicate ...)
                                  (list 'triple-object ...))
                        (format #t "```")))
                   (_ (error "Invalid schema triples clause:" #'schema-triples-clause)))
               (format #t "
## Generated Triples:

The following SQL query was executed:

```sql
~a
```

The above query results to triples that have the form:

```text
"
                       (select-query #,(collect-fields #'(subject predicate-clauses ...))
                                     (primary-table other-tables ...)
                                     tables-raw ...))
               (for-each (match-lambda
                           ((predicate . object)
                            (format #t "~a -> ~a -> ~a ~%"
                                    #,(field->datum #'subject)
                                    predicate
                                    (if (symbol? object)
                                        (symbol->string object)
                                        object))))
                         (map-alist
                             '()
                           #,@(field->datum #'(predicate-clauses ...))))
               (format #t "```~%Here's an example query:~%~%```sparql~%")
               (dump-documentation?)
               (newline)
               (let* ((result
                       (map-alist (sql-find
                                   db
                                   (format #f "~a LIMIT 1"
                                           (select-query #,(collect-fields #'(subject predicate-clauses ...))
                                                         (primary-table other-tables ...)
                                                         tables-raw ...)))
        		 #,@(field->key #'(predicate-clauses ...))))
                      (first-n (list-head result
                                          (let ((n (truncate
                                                    (+ (max (exact-integer-sqrt (length result))) 1))))
                                            (if (< n 3)
                                                (length result)
                                                n)))))
                 (format #t "SELECT ?s ?p ?o WHERE { ~%")
                 (for-each (match-lambda
                             ((predicate . object)
                              (match object
                                        ((or (?  symbol? object)
                                             (?  (lambda (el) (string-match "^\\[ .* \\]$" el)) object))
                                         (format #t "    ?s ~a ~a .~%" predicate object))
                                        ((and (? string? object)
                                              (? (lambda (el) (not (string-null? el))) object))
                                         (format #t "    ?s ~a \"~a\" .~%" predicate object))
                                        (_ ""))))
                           first-n)
                 (format #t "    ?s ?p ?o .~%}~%```~%"))
               (format #t "~%Expected Result:~%~%```rdf~%")
               (sql-for-each (lambda (row)
                               (scm->triples
                                (map-alist row #,@(field->key #'(predicate-clauses ...)))
                                #,(field->assoc-ref #'row #'subject)
                                (lambda (s p o)
                                  (triple s p o))))
                             db
                             (format #f "~a LIMIT 1"
                                     (select-query #,(collect-fields #'(subject predicate-clauses ...))
                                                   (primary-table other-tables ...)
                                                   tables-raw ...)))
               (format #t "```~%~%"))
             (when dump-data?
	       (sql-for-each (lambda (row)
                               (scm->triples
                                (map-alist row #,@(field->key #'(predicate-clauses ...)))
                                #,(field->assoc-ref #'row #'subject)))
                             db
                             (select-query #,(collect-fields #'(subject predicate-clauses ...))
                                           (primary-table other-tables ...)
                                           tables-raw ...)))
             )))
      (_ (error "Invalid define-dump syntax:" (syntax->datum x))))))

(define (get-keyword-value args keyword default)
  (let ((kv (memq keyword args)))
    (if (and kv (>= (length kv) 2))
        (cadr kv)
        default)))

(define-syntax dump-with-documentation
  (syntax-rules ()
    ((_ (key value) ...)
     (let ((name "")
           (connection "")
           (table-metadata? "")
           (prefixes "")
           (inputs "")
           (outputs ""))
       (for-each
        (match-lambda
          (('name n)
           (set! name n))
          (('connection conn)
           (set! connection conn))
          (('table-metadata? t-metadata?)
           (set! table-metadata? t-metadata?))
          (('prefixes p)
           (set! prefixes p))
          (('inputs i)
           (set! inputs i))
          (('outputs o)
           (set! outputs o)))
        (list (list 'key value) ...))
       (let ((rdf-path (get-keyword-value outputs #:rdf ""))
             (doc-path (get-keyword-value outputs #:documentation ""))
             (prefix-thunk (lambda ()  (for-each
                                        (match-lambda
                                          ((k v)
                                           (begin
                                             (prefix k v))))
                                        prefixes))))
          ;; Dumping the documentation first
          (call-with-target-database
           connection
           (lambda (db)
             (with-output-to-file       ;
                 doc-path
               (lambda ()
                 (format #t "# ~a" name)
                 (for-each
                  (lambda (proc)
                    (proc db #f #f prefix-thunk))
                  inputs))
               #:encoding "utf8")

             ;; Dumping the actual data
             (with-output-to-file
                 rdf-path
               (lambda ()
                 ;; Add the prefixes
                 (prefix-thunk)
                 (newline)
                 (for-each
                  (lambda (proc)
                    (proc db #f #t #f))
                  inputs))
               #:encoding "utf8"))))))))

