(define-module (dump utils)
  ;; Rename delete to srfi:delete since it somehow interferes with the
  ;; delete verb of map-alist.
  #:use-module ((srfi srfi-1) #:renamer (lambda (sym)
                                          (if (eq? sym 'delete)
                                              'srfi:delete sym)))
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:export (translate-forms
            collect-forms
            map-alist))

(define (translate-forms from translator x)
  "Recursively pass (FROM ...) forms in source X to TRANSLATOR, and
replace them with the return value."
  (syntax-case x ()
    ;; Handle translation base case.
    ((head tail ...) (eq? (syntax->datum #'head)
                          from)
     (translator x))
    ;; Recurse.
    ((head tail ...)
     (cons (translate-forms from translator #'head)
           (map (cut translate-forms from translator <>)
                #'(tail ...))))
    ;; Handle leaf base case.
    (leaf #'leaf)))

(define (key->assoc-ref alist x)
  "Recursively translate (key k) forms in source X to (assoc-ref ALIST
k) forms."
  (translate-forms 'key
                   ;; (syntax-rules (key)
                   ;;   ((key k) (assoc-ref alist k)))
                   (lambda (x)
                     (syntax-case x (key)
                       ((key k) #`(assoc-ref #,alist 'k))))
                   x))

(define (collect-forms target x)
  "Recursively collect (TARGET ...) forms in source X and return them
as a list."
  (syntax-case x ()
    ;; Handle collection base case.
    ((head tail ...) (eq? (syntax->datum #'head)
                          target)
     (list x))
    ;; Recurse.
    ((head tail ...)
     (append (collect-forms target #'head)
             (append-map (cut collect-forms target <>) #'(tail ...))))
    ;; Handle leaf base case.
    (leaf (list))))

(define (collect-keys x)
  "Recursively collect (key k) forms from source X and return as a
list of all K."
  (map (syntax-rules (key)
         ((key k) 'k))
       (collect-forms 'key x)))

(define (alist-delete* alist keys)
  "Delete entries from ALIST whose key is equal (in the sense of
equal?) to any in KEYS, a list."
  (remove (match-lambda
            ((key . value)
             (member key keys))
            (x (error "malformed alist element" x)))
          alist))

(define-syntax map-alist
  (lambda (x)
    "Transform (aka map) ALIST, an association list, into another
association list. The returned association list may contain multiple
associations for the same key. equal? is used in all association list
key comparisons.

Syntax:

(map-alist alist
  (verb key expression) ...
  (else=> proc))

VERB must be one of set, filter-set, multiset and delete.

For the set VERB, KEY is set to the result of evaluating EXPRESSION.

For the filter-set VERB, KEY is set to the result of evaluating
EXPRESSION only if that result is not #f.

For the multiset VERB, EXPRESSION must return a list and KEY is
associated multiple times once with each element of the returned list.

For the delete VERB, KEY is discarded.

EXPRESSIONs must reference elements of ALIST using (key k) forms where
K is the key to be referenced from ALIST. K must not be quoted. That
is, if K is the symbol 'bar, it must be referenced as (key bar), not
as (key 'bar).

The else=> clause is optional.

If the else=> clause is present, PROC is passed all pairs of ALIST
that are not set by an earlier (verb key expression) action. PROC must
return #f or a pair to replace its input pair. If PROC returns #f,
that pair is discarded.

If the else=> clause is absent, all unset pairs are discarded.

Example:

(map-alist '((\"foo\" . 1)
             (bar . 2)
             (foobar . 5)
             (fubar . 3))
  (set spam (1+ (key \"foo\")))
  (set ham (* 2 (key bar)))
  (set eggs (* 3 (key bar)))
  (set aal (+ (key \"foo\")
              (key bar)))
  (multiset vel (iota (* 2 (key bar))))
  (delete foobar)
  (else=> (match-lambda
            ((key . value)
             (cons key (expt 2 value))))))
=> ((spam . 2) (ham . 4) (eggs . 6) (aal . 3)
    (vel . 0) (vel . 1) (vel . 2) (vel . 3) (fubar . 8))"
    (syntax-case x ()
      ((_ alist actions ...)
       ;; TODO: Check that all actions are valid.
       #`(let ((evaluated-alist alist))
           (append (remove (match-lambda
                             ;; Filter out results of filter-set actions.
                             ((key . #f)
                              (member key '#,(filter-map (lambda (action)
                                                           (syntax-case action (filter-set)
                                                             ((filter-set key expression) #'key)
                                                             (_ #f)))
                                                         #'(actions ...))))
                             (_ #f))
                           ;; Do set and filter-set.
                           `#,(filter-map (lambda (action)
                                            (syntax-case action (set filter-set)
                                              ((set key expression)
                                               #`(key . ,#,(key->assoc-ref #'evaluated-alist #'expression)))
                                              ((filter-set key expression)
                                               #`(key . ,#,(key->assoc-ref #'evaluated-alist #'expression)))
                                              (_ #f)))
                                          #'(actions ...)))
                   ;; Do multiset.
                   #,@(filter-map (lambda (action)
                                    (syntax-case action (multiset)
                                      ((multiset key expression)
                                       #`(map (cut cons 'key <>)
                                              #,(key->assoc-ref #'evaluated-alist #'expression)))
                                      (_ #f)))
                                  #'(actions ...))
                   ;; Apply else=> procedure on unspecified keys. If
                   ;; no else=> procedure is specified, delete
                   ;; unspecified keys.
                   (filter-map #,(or (any (lambda (action)
                                            (syntax-case action (else=>)
                                              ((else=> proc) #'proc)
                                              (_ #f)))
                                          #'(actions ...))
                                     #'(const #f))
                               ;; The unspecified part of the input
                               ;; alist
                               (alist-delete* evaluated-alist
                                              (list
                                               ;; Keys that were referenced
                                               #,@(append-map (lambda (action)
                                                                (syntax-case action ()
                                                                  ((_ key expression)
                                                                   (collect-keys #'expression))
                                                                  (_ '())))
                                                              #'(actions ...))
                                               ;; Keys that were deleted
                                               #,@(filter-map (lambda (action)
                                                                (syntax-case action (delete)
                                                                  ((delete key) #''key)
                                                                  (_ #f)))
                                                              #'(actions ...))
                                               ;; Keys that were set
                                               #,@(filter-map (lambda (action)
                                                                (syntax-case action ()
                                                                  ((_ key expression) #''key)
                                                                  (_ #f)))
                                                              #'(actions ...)))))))))))
