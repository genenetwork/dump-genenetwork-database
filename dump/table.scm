(define-module (dump table)
  #:use-module (srfi srfi-9 gnu)
  #:export (make-table
            table-name
            table-size
            table-columns
            set-table-columns
            make-column
            column-name
            column-int?))

(define-immutable-record-type <table>
  (make-table name size columns)
  table?
  (name table-name)
  (size table-size)
  (columns table-columns set-table-columns))

(define-immutable-record-type <column>
  (make-column name int?)
  column?
  (name column-name)
  (int? column-int?))
