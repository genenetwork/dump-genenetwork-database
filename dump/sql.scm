;;; Database helpers
;;;
;;; These functions should have been a part of guile-dbi. Never too
;;; late to contribute upstream!

(define-module (dump sql)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:use-module (dbi dbi)
  #:export (call-with-database
            sql-exec
            sql-fold
            sql-map
            sql-for-each
            sql-find))

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
