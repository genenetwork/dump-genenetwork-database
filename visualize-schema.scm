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

(define (sparql-query-records . args)
  (query-results->list (apply sparql-query
                              (append args
                                  (list #:host "127.0.0.1"
                                        #:port 8891)))
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
  ((@@ (ccwl graphviz) html-string) (sxml->xml-string tree)))

(define (table-label table)
  "Return HTML string label for TABLE."
  (sxml->graphviz-html
   `(table (@ (cellborder 0)
              (bgcolor "white"))
           (tr (td (@ (border 1)
                      (bgcolor ,(human-units-color (table-size table))))
                   ,(format "~a (~a)"
                            (table-name table)
                            (human-units (table-size table)))))
           ,@(map (lambda (column)
                    `(tr (td (@ (port ,(column-name column)))
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
         ((table size)
          (make-table table
                      ;; FIXME: Why is size coming out as a string?
                      (string->number size)
                      (map (match-lambda
                             ((field type) (make-column field type)))
                           (sparql-query-records
                            ;; We use format to construct the query instead of
                            ;; select due to an outstanding bug in
                            ;; guile-sparql. See
                            ;; https://github.com/roelj/guile-sparql/issues/5
                            (format
                             "SELECT ?fieldname ?fieldtype
WHERE
{
  ?table <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://genenetwork.org/sqlTable> .
  ?table <http://genenetwork.org/name> ~s .
  ?field <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://genenetwork.org/sqlTableField> .
  ?table <http://genenetwork.org/hasField> ?field .
  ?field <http://genenetwork.org/name> ?fieldname .
  ?field <http://genenetwork.org/sqlFieldType> ?fieldtype .
}" table))))))
       (sparql-query-records
        (select '(tablename size)
                `((table ,(rdf "type") ,(gn "sqlTable"))
                  (table ,(gn "name") tablename)
                  (table ,(gn "hasSize") size))))))

(define (foreign-key-graphviz-edges tables)
  "Return the list of graphviz edges representing foreign key
relations in TABLES."
  (append-map (lambda (table)
                (filter-map (lambda (column)
                              (and=> (column->foreign-table table column tables)
                                     (cut cons
                                          ((@@ (ccwl graphviz) graph-port)
                                           (table-name table)
                                           (column-name column))
                                          <>)))
                            (table-columns table)))
              tables))

(let ((all-tables (tables)))
  ((@@ (ccwl graphviz) graph->dot)
   ((@@ (ccwl graphviz) graph) 'schema
    #:nodes (map (lambda (table)
                   ((@@ (ccwl graphviz) graph-node)
                    (table-name table)
                    `((shape . "none")
                      (label . ,(table-label table)))))
                 all-tables)
    #:edges (foreign-key-graphviz-edges all-tables))))