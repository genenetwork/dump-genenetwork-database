(define-module (dump triples)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 match)
  #:use-module (dump strings)
  #:export (ontology
            string->identifier
            prefix
            triple
            scm->triples
            annotate-field
            string->binomial-name))

(define (annotate-field field schema)
  (let ([schema (cond ((symbol? schema)
                       (symbol->string schema))
                      ((string? schema) schema)
                      (else
                       (error "Use a string/symbol")))]
        [string-field (if (number? field) (number->string field) field)])
    (if (string-null? string-field)
        ""
        (string->symbol
         (format #f "~s~a" string-field schema)))))

(define* (string->identifier prefix str
                             #:optional #:key
                             (ontology "gn:")
                             (separator "_")
                             (proc string-downcase))
  "Convert STR to a turtle identifier after replacing illegal
characters with an underscore and prefixing with gn:PREFIX."
  (if (string-null? str)
      ""
      (string->symbol
       (string-append ontology prefix separator
                      (string-delete
                       (lambda (c)
                         (eq? c #\)))
                       (string-map (lambda (c)
                                     (case c
                                       ((#\/ #\< #\> #\+ #\( #\space #\@) #\_)
                                       (else c)))
                                   (proc
                                    (string-trim-right str #\.))))))))


(define* (prefix prefix iri #:optional (ttl? #t))
  (format #t
	  (if ttl?
	      "@prefix ~a ~a .~%"
	      "PREFIX ~a ~a ~%")
	  prefix iri))

(define (ontology prefix value)
  (if (and (string? value) (string-null? value))
      ""
      (string->symbol
       `,(format #f "~a~a" prefix value))))

(define (triple subject predicate object)
  (unless (or (string? subject)
              (symbol? subject))
    (error "Triple subject not a string or symbol:"
           (list subject predicate object)))
  (unless (or (string? predicate)
              (symbol? predicate))
    (error "Triple predicate not a string or symbol:"
           (list subject predicate object)))
  (unless (or (string? object)
              (symbol? object)
              (number? object))
    (error "Triple object not a string, symbol or number:"
           (list subject predicate object)))
  (let ([pattern (match object
                   ((or (?  symbol? object)
                        (? (lambda (el) (string-match "^\\[ .* \\]$" el)) object))
                    "~a ~a ~a .~%")
                   (_ "~a ~a \"~a\" .~%"))])
    (format #t pattern subject predicate
            (if (symbol? object) (symbol->string object) object))))

(define* (scm->triples alist id
                       #:optional
                       (fn triple))
  (for-each (match-lambda
              ((predicate . object)
               (when (cond
                      ((string? object)
                       (not (string-blank? object)))
                      (else object))
                 (fn id predicate object))))
            alist))

(define (string->binomial-name name)
  (let ((binomial?
         (string-match
          "\\\(.+\\)"
          name)))
    (string->identifier
     ""
     (if binomial?
         (regexp-substitute/global
          #f "[^[:space:]A-Za-z0-9:]"
          (match:substring binomial?)
          'pre "" 'post)
         name)
     #:separator ""
     #:proc string-capitalize-first)))
