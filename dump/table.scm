(define-module (dump table)
  #:use-module (srfi srfi-9 gnu)
  #:export (make-table
            table-name
            table-size
            table-columns
            set-table-columns
            make-column
            column-name
            column-type
            column-dumped?))

(define-immutable-record-type <table>
  (make-table name size columns)
  table?
  (name table-name)
  (size table-size)
  (columns table-columns set-table-columns))

(define-immutable-record-type <column>
  (column-constructor name type dumped?)
  column?
  (name column-name)
  (type column-type)
  (dumped? column-dumped?))

(define* (make-column name type #:optional dumped?)
  (column-constructor name type dumped?))
