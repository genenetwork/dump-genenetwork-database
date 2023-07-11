(define-module (dump triples)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 match)
  #:use-module (dump strings)
  #:export (ontology
            string->identifier
            prefix
            triple
            scm->triples
            annotate-field))

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

(define (string->identifier prefix str)
  "Convert STR to a turtle identifier after replacing illegal
characters with an underscore and prefixing with gn:PREFIX."
  (if (string-null? str)
      ""
      (string->symbol
       (string-append "gn:" prefix "_"
                      (string-map (lambda (c)
                                    (case c
                                      ((#\/ #\< #\> #\+ #\( #\) #\space #\@) #\_)
                                      (else c)))
                                  (string-downcase
                                   (string-trim-right str #\.)))))))

(define (prefix prefix iri)
  (format #t "@prefix ~a ~a .~%" prefix iri))

(define (ontology prefix value)
  (if (and (string? value) (string-null? value))
      ""
      (string->symbol
       `,(format #f "~a~a" prefix value))))

(define* (triple subject predicate object
                 #:optional
                 (port #t))
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
    (format port pattern subject predicate
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
