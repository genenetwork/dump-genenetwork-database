(use-modules (srfi srfi-1)
             (ice-9 match)
             (ice-9 rdelim)
             (dsv))

(define (assoc-set alist key value)
  "Associate KEY with VALUE in ALIST, and return the new association
list. This is a functional setter and does not mutate ALIST. KEY is
compared with keys in ALIST using equal?."
  (acons key value
         (alist-delete key alist)))

(define (parse-aggregate-table table)
  "Convert TABLE from the provided aggregate file to an s-exp equivalent.
The GROUP forms the KEY of each alist."
  (match table
    ((header table-body ...)
     (fold (lambda (row results)
             (let ((alist (map cons header row)))
               (acons (string->symbol (assoc-ref alist "group"))
                      (filter-map (lambda (key value)
                                    (and (member key
                                                 (list "mean" "n" "sex " "SD" "day"))
                                         (cons (string->symbol (string-downcase key))
                                               value)))
                                  header row)
                      results)))
           '()
           table-body))))

(define (parse-raw-table table aggregate-table)
  "Given TABLE which represents the data from the raw file, and the
AGGREGATE-TABLE which is an ALIST containing aggregate data, add extra
metadata not present in the aggregate table and return this result."
  (match table
    ((header table-body ...)
     (fold (lambda (row results)
             (let* ((alist (map cons header row))
                    (key (string->symbol (string-join
                                          (list (assoc-ref alist "strain")
                                                (assoc-ref alist "sex")
                                                (assoc-ref alist "day"))
                                          "_")))
                    (aggregate-data (assq-ref aggregate-table key))
                    (folded-data (assq-ref results key)))
               ;; Return an updated version of the aggregate-table as
               ;; you loop through the raw table.  Do this while
               ;; collecting the values for 'bw and setting 'inf-dose
               (assoc-set results
                          key (assoc-set
                               ;; inf_dose is repetitive in raw table
                               ;; and it's common in the same vector
                               ;; of data.  If it isn't set, set it
                               ;; once.
                               (if (assq-ref folded-data 'aggregate-dose)
                                   aggregate-data
                                   (acons 'inf-dose (assoc-ref alist "inf_dose") aggregate-data))
                               'bw
                               ;; When looping in the raw table,
                               ;; collect all the 'bw values.
                               (if (assq-ref folded-data 'bw)
                                   (cons (assoc-ref alist "BW")
                                         (assq-ref folded-data 'bw))
                                   (list (assoc-ref alist "BW")))))))
           '()
           table-body))))
