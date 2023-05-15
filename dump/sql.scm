;;; Database helpers
;;;
;;; Most of these functions should have been a part of
;;; guile-dbi. Never too late to contribute upstream!

(define-module (dump sql)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (dbi dbi)
  #:export (select-query
            call-with-database
            call-with-target-database
            sql-exec
            sql-fold
            sql-map
            sql-for-each
            sql-find))

;; A half-baked macro to make SQL SELECT queries a bit more
;; S-expression friendly
(define-syntax select-query
  (lambda (x)
    (syntax-case x ()
      ((_ fields tables raw-forms ...)
       #`(string-append "SELECT "
                        #,(syntax-case #'fields (distinct)
                            ((distinct _ ...)
                             "DISTINCT ")
                            (_ ""))
                        #,(string-join (filter-map (match-lambda
                                                     ('distinct #f)
                                                     (((query alias))
                                                      (format #f "~a AS ~a" query alias))
                                                     ((table column)
                                                      (format #f "~a.~a" table column))
                                                     ((table column alias)
                                                      (format #f "~a.~a AS ~a" table column alias))
                                                     (field-spec
                                                      (error "Invalid field specification" field-spec)))
                                                   (syntax->datum #'fields))
                                       ", ")
                        " FROM "
                        #,(string-join (map (match-lambda
                                              ((join table condition)
                                               (format #f "~a ~a ~a"
                                                       (case join
                                                         ((join) "JOIN")
                                                         ((left-join) "LEFT JOIN")
                                                         ((inner-join) "INNER JOIN")
                                                         (else (error "Invalid join operator" join)))
                                                       table condition))
                                              ((? symbol? table)
                                               (symbol->string table))
                                              (table-spec
                                               (error "Invalid table specification" table-spec)))
                                            (syntax->datum #'tables))
                                       " ")
                        #,(syntax-case #'(raw-forms ...) ()
                            (() "")
                            (_ " "))
                        raw-forms ...))
      (_ (error "Invalid SQL select query" (syntax->datum x))))))

(define (call-with-database backend connection-string proc)
  (let ((db #f))
    (dynamic-wind (lambda ()
                    (set! db (dbi-open backend connection-string)))
                  (cut proc db)
                  (cut dbi-close db))))

(define (database-check-status db)
  (match (dbi-get_status db)
    ((code . str)
     (unless (zero? code)
       (error str)))))

(define (sql-exec db statement)
  (dbi-query db statement)
  (database-check-status db))

(define (sql-fold proc init db statement)
  (sql-exec db statement)
  (let loop ((result init))
    (let ((row (dbi-get_row db)))
      (if row
          (loop (proc row result))
          result))))

(define (sql-map proc db statement)
  (sql-fold (lambda (row result)
              (cons (proc row) result))
            (list) db statement))

(define (sql-for-each proc db statement)
  (sql-fold (lambda (row _)
              (proc row))
            #f db statement))

(define (sql-find db statement)
  (sql-exec db statement)
  (dbi-get_row db))

(define (call-with-target-database connection-settings proc)
  (call-with-database "mysql" (string-join
                               (list (assq-ref connection-settings 'sql-username)
                                     (assq-ref connection-settings 'sql-password)
                                     (assq-ref connection-settings 'sql-database)
                                     "tcp"
                                     (assq-ref connection-settings 'sql-host)
                                     (number->string
                                      (assq-ref connection-settings 'sql-port)))
                               ":")
                      proc))
