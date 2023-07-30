(define-module (dump schema)
  #:use-module (ice-9 match)
  #:use-module (ice-9 srfi-26)
  #:use-module (dump sql)
  #:use-module (dump triples)
  #:use-module (dump strings)
  #:use-module (dump table))


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

(define (get-tables-from-comments db)
  (sql-map (match-lambda
             ((("TableName" . table)) table))
           db
           (select-query ((TableComments TableName))
                         (TableComments))))

(define (dump-schema-annotations db)
  (call-with-target-database
   (lambda (db)
     (for-each (cut dump-table-fields db <>)
               (get-tables-from-comments db)))))

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

(define* (dump-data-table db table-name data-field
                          #:optional (default-dump-directory ""))
  (let ((dump-directory (string-append default-dump-directory "/" table-name))
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
