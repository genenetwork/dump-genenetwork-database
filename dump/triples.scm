(define-module (dump triples)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 match)
  #:use-module (dump utils)
  #:export (ontology
            string->identifier
            prefix
            triple
            scm->triples))

(define (string->identifier prefix str)
  "Convert STR to a turtle identifier after replacing illegal
characters with an underscore and prefixing with gn:PREFIX."
  (string->symbol
   (string-append "gn:" prefix "_"
                  (string-map (lambda (c)
                                (case c
                                  ((#\/ #\< #\> #\+ #\( #\) #\space #\@) #\_)
                                  (else c)))
                              (string-downcase
                               (string-trim-right str #\.))))))

(define (prefix prefix iri)
  (format #t "@prefix ~a ~a .~%" prefix iri))

(define (ontology prefix value)
  (string->symbol
   `,(format #f "~a~a" prefix value)))

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
  (match object
    ((and (?  string? object)
          (?  (lambda (el) (string-match "^\\[ .* \\]$" object))))
     (format #t "~a ~a ~a .~%" subject predicate object))
    ((?  symbol? object)
     (format #t "~a ~a ~a .~%" subject predicate object))
    ((or (?  string? object)
         (?  number? object))
     (format #t "~a ~a ~s .~%" subject predicate object))
    (_ (error "Trible object must be a string, symbol, number or blank node"))))

(define (scm->triples alist id)
  (for-each (match-lambda
              ((predicate . object)
               (when (cond
                      ((string? object)
                       (not (string-blank? object)))
                      (else object))
                 (triple id predicate object))))
            alist))
