#! /usr/bin/env guile
!#

(use-modules (rnrs io ports)
             (srfi srfi-1)
             (srfi srfi-26)
             (srfi srfi-28)
             (srfi srfi-71)
             (srfi srfi-171)
             (ice-9 match)
             (sxml simple)
             (sparql driver)
             (sparql lang)
             (sparql util)
             (dump string-similarity)
             (dump table))

(define rdfs
  (prefix "http://www.w3.org/2000/01/rdf-schema#"))

(define rdf
  (prefix "http://www.w3.org/1999/02/22-rdf-syntax-ns#"))

(define gn
  (prefix "http://genenetwork.org/"))

(define graph (@@ (ccwl graphviz) graph))
(define graph-node (@@ (ccwl graphviz) graph-node))
(define graph-edge (@@ (ccwl graphviz) graph-edge))
(define graph-port (@@ (ccwl graphviz) graph-port))
(define html-string (@@ (ccwl graphviz) html-string))
(define graph->dot (@@ (ccwl graphviz) graph->dot))

(define %sparql-host
  (make-parameter #f))

(define %sparql-port
  (make-parameter #f))

(define (sparql-query-records . args)
  ;; TODO: Use the JSON query results so that types can be converted
  ;; correctly.
  (query-results->list (apply sparql-query
                              (append args
                                      (list #:host (%sparql-host)
                                            #:port (%sparql-port))))
                       #t))

(define (floor-log1024 x)
  "Return the floor of the base 1024 logarithm of X."
  (if (< x 1024)
      0
      (1+ (floor-log1024 (/ x 1024)))))

(define (human-units bytes)
  "Return number of BYTES as a string with human-readable units."
  (let* ((integer-log (floor-log1024 bytes)))
    (format "~a ~a"
            (round-quotient bytes (expt 1024 (min integer-log 3)))
            (case integer-log
              ((0) "B")
              ((1) "KiB")
              ((2) "MiB")
              (else "GiB")))))

(define (human-units-color bytes)
  "Return the table header color coding for a table of size BYTES."
  (let* ((color-scheme "purd4"))
    (format "/~a/~a"
            color-scheme
            (1+ (min (floor-log1024 bytes) 3)))))

(define (sxml->xml-string tree)
  "Serialize sxml TREE to a string. Return the serialized string."
  (call-with-output-string
    (cut sxml->xml tree <>)))

(define (sxml->graphviz-html tree)
  "Convert sxml TREE to a graphviz <html-string>, and return it."
  (html-string (sxml->xml-string tree)))

(define (table-label table)
  "Return HTML string label for TABLE."
  (sxml->graphviz-html
   `(table (@ (cellborder 0)
              (bgcolor ,(if (any column-dumped?
                                 (table-columns table))
                            "lightgrey"
                            "white")))
           (tr (td (@ (border 1)
                      (bgcolor ,(human-units-color (table-size table))))
                   ,(format "~a (~a)"
                            (table-name table)
                            (human-units (table-size table)))))
           ,@(map (lambda (column)
                    `(tr (td (@ (port ,(column-name column))
                                ,@(if (column-dumped? column)
                                      `((bgcolor "green"))
                                      '()))
                             ,(column-name column))))
                  (table-columns table)))))

(define (string-remove-suffix-ci suffix str)
  "Remove SUFFIX from STR if present. Suffix check is
case-insensitive."
  (if (string-suffix-ci? suffix str)
      (substring str 0 (- (string-length str)
                          (string-length suffix)))
      str))

(define (column->foreign-table table column all-tables)
  "If COLUMN in TABLE is a foreign key, return the table it refers to. Else,
return #f. ALL-TABLES is a list of all tables in the database."
  (cond
   ((and (string=? (column-name column) "UserId")
         (string=? (table-name table) "UserPrivilege"))
    'User)
   ((string-ci=? (column-name column) "GenbankID")
    'Genbank)
   ((not (or (string-prefix? "int" (column-type column))
             (string-prefix? "smallint" (column-type column))))
    #f)
   ((let ((string-similarity-threshold 0.8)
          (target-table
           (or (and=> (find (lambda (suffix)
                              (string-suffix-ci? suffix (column-name column)))
                            (list "id1" "id2" "_id" "id"))
                      (cut string-remove-suffix-ci <> (column-name column)))
               (column-name column))))
      (and (not (jaccard-string-similar? target-table
                                         (table-name table)))
           (find (lambda (table)
                   (jaccard-string-similar?
                    target-table (table-name table)))
                 all-tables)))
    => table-name)
   (else #f)))

(define (tables)
  "Return list of all tables in DB. Each element of the returned list
is a <table> object."
  (map (match-lambda
         ((table size fields field-types field-dumped)
          (make-table table
                      (string->number size)
                      (map make-column
                           (string-split fields #\,)
                           (string-split field-types #\,)
                           (map (cut string=? <> "1")
                                (string-split field-dumped #\,))))))
       (sparql-query-records
        "PREFIX gn: <http://genenetwork.org/>
SELECT SAMPLE(?tablename) SAMPLE(?size) GROUP_CONCAT(?fieldname ; separator=\",\") GROUP_CONCAT(?fieldtype ; separator=\",\") GROUP_CONCAT(EXISTS{ ?dump rdf:type gn:dump . ?dump gn:dependsOn ?field .} ; separator=\",\")
WHERE
{
  ?table rdf:type gn:sqlTable ;
         gn:name ?tablename ;
         gn:hasSize ?size ;
         gn:hasField ?field .
  ?field rdf:type gn:sqlTableField ;
         gn:name ?fieldname ;
         gn:sqlFieldType ?fieldtype .
} GROUP BY ?table")))

(define (foreign-key-graphviz-edges tables)
  "Return the list of graphviz edges representing foreign key
relations in TABLES."
  (append-map (lambda (table)
                (filter-map (lambda (column)
                              (and=> (column->foreign-table table column tables)
                                     (cut cons
                                          (graph-port (table-name table)
                                                      (column-name column))
                                          <>)))
                            (table-columns table)))
              tables))

(define (write-sql-visualization port)
  "Write a visualization of the SQL schema in graphviz dot syntax to
PORT."
  (let ((all-tables (tables)))
    (graph->dot
     (graph 'schema
            #:nodes (map (lambda (table)
                           (graph-node
                            (table-name table)
                            `((shape . "none")
                              (label . ,(table-label table)))))
                         all-tables)
            #:edges (foreign-key-graphviz-edges all-tables))
     port)))

(define (literal-node-id domain predicate)
  "Return the graphviz node identifier for an RDF literal node which
is the range of PREDICATE. The domain of PREDICATE is DOMAIN."
  (string-append "literal_node__"
                 (basename domain)
                 "_"
                 (basename predicate)))

(define (rdf-type-nodes)
  "Return the list of all graphviz nodes representing types."
  (map (match-lambda
         ((type)
          (graph-node (basename type)
                      `((fillcolor . lightgreen)
                        (style . filled)))))
       (sparql-query-records
        "SELECT DISTINCT ?type
WHERE {
  { ?predicate rdfs:domain ?type }
  UNION
  { ?predicate rdfs:range ?type }
  MINUS
  { ?predicate rdfs:range rdfs:Literal }
}")))

(define (rdf-literal-nodes)
  "Return the list of all graphviz nodes representing literal
properties."
  (map (match-lambda
         ((type predicate tables fields)
          (graph-node
           (literal-node-id type predicate)
           `((shape . box)
             (style . filled)
             (fillcolor . gold)
             (label . ,(basename predicate))
             (tooltip . ,(string-join
                          (map (lambda (table field)
                                 (string-append table "." field))
                               (string-split tables #\,)
                               (string-split fields #\,))
                          ", "))))))
       (sparql-query-records
        "PREFIX gn: <http://genenetwork.org/>
SELECT ?type ?predicate GROUP_CONCAT(?tablename ; separator=\",\") GROUP_CONCAT(?fieldname ; separator=\",\")
WHERE
{
  ?predicate rdfs:domain ?type ;
             rdfs:range rdfs:Literal .
  ?dump rdf:type gn:dump ;
        gn:createsPredicate ?predicate ;
        gn:forSubjectType ?type ;
        gn:dependsOn ?field .
  ?field rdf:type gn:sqlTableField ;
         gn:name ?fieldname .
  ?table rdf:type gn:sqlTable ;
         gn:hasField ?field ;
         gn:name ?tablename .
} GROUP BY ?type ?predicate
")))

(define (rdf-edges)
  "Return the list of all graphviz edges in the RDF visualization."
  (map (match-lambda
         ((type predicate range)
          (if (string=? range (string-trim-both (rdfs "Literal")
                                                (char-set #\< #\>)))
              ;; Literal properties
              (graph-edge (basename type)
                          (literal-node-id type predicate))
              ;; Relations between classes
              (graph-edge (basename type)
                          (basename range)))))
       (sparql-query-records
        (select '(type predicate range)
                `((predicate ,(rdfs "domain") type)
                  (predicate ,(rdfs "range") range))))))

(define (write-rdf-visualization port)
  "Write a visualization of the RDF schema in graphviz dot syntax to
PORT."
  (graph->dot
   (graph 'rdf
          #:nodes (append (rdf-type-nodes)
                      (rdf-literal-nodes))
          #:edges (rdf-edges))
   port))

(define main
  (match-lambda*
    ((_ connection-settings-file)
     (let ((connection-settings (call-with-input-file connection-settings-file
                                  read)))
       (parameterize ((%sparql-host (assq-ref connection-settings 'sparql-host))
                      (%sparql-port (assq-ref connection-settings 'sparql-port)))
         (call-with-output-file "sql.dot"
           write-sql-visualization)
         (call-with-output-file "rdf.dot"
           write-rdf-visualization))))
    ((arg0 _ ...)
     (display (format "Usage: ~a CONNECTION-SETTINGS-FILE~%" arg0)
              (current-error-port))
     (exit #f))))

(apply main (command-line))
