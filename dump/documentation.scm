(define-module (dump documentation)
  #:use-module (srfi srfi-9 gnu)
  #:export (dump-configuration
            dump-configuration?
            dump-configuration-triples?
            dump-configuration-table-metadata?
            dump-configuration-path
            call-with-documentation))


(define-immutable-record-type <dump-configuration>
  (%dump-configuration triples? table-metadata? path)
  dump-configuration?
  (triples? dump-configuration-triples?)
  (table-metadata? dump-configuration-table-metadata?)
  (path dump-configuration-path))

(define* (dump-configuration
          #:optional
          (triples? #t)
          (table-metadata? #f)
          (path #f))
  "Return a new configuration."
  (%dump-configuration triples? table-metadata? path))


(define (call-with-documentation conf proc)
  (let ((port #f)
        (path (dump-configuration-path conf)))
    (when path
      (dynamic-wind
        (lambda ()
          (set! port (open-file path "w")))
        (cut proc port)
        (cut close port)))))
