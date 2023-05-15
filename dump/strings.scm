(define-module (dump strings)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (ice-9 match)
  #:use-module (ice-9 string-fun)
  #:export (string-blank?
            time-unix->string
            string-blank?
            string-split-substring
            delete-substrings
            replace-substrings
            sanitize-rdf-string
            snake->lower-camel))

(define (time-unix->string seconds . maybe-format)
  "Given an integer saying the number of seconds since the Unix
epoch (1970-01-01 00:00:00), SECONDS, format it as a human readable
date and time-string, possible using the MAYBE-FORMAT."
  (letrec ([time-unix->time-utc
            (lambda (seconds)
              (add-duration
               (date->time-utc (make-date 0 0 0 0 1 1 1970 0))
               (make-time time-duration 0 seconds)))])
    (apply date->string
           (time-utc->date (time-unix->time-utc seconds))
           maybe-format)))

(define (string-blank? str)
  "Return non-#f if STR consists only of whitespace characters."
  (string-every char-set:whitespace str))

(define (string-split-substring str substr)
  "Split the string @var{str} into a list of substrings delimited by the
substring @var{substr}."

  (define substrlen (string-length substr))
  (define strlen (string-length str))

  (define (loop index start)
    (cond
     ((>= start strlen) (list ""))
     ((not index) (list (substring str start)))
     (else
      (cons (substring str start index)
            (let ((new-start (+ index substrlen)))
              (loop (string-contains str substr new-start)
                    new-start))))))

  (cond
   ((string-contains str substr) => (lambda (idx) (loop idx 0)))
   (else (list str))))

(define (delete-substrings str . substrings)
  "Delete SUBSTRINGS, a list of strings, from STR."
  (fold (lambda (substring result)
          (string-replace-substring result substring ""))
        str
        substrings))

(define (replace-substrings str replacement-alist)
  "Replace substrings in STR according to REPLACEMENT-ALIST, an
association list mapping substrings to their replacements."
  (fold (match-lambda*
          (((substring . replacement) str)
           (string-replace-substring str substring replacement)))
        str
        replacement-alist))

(define (sanitize-rdf-string str)
  (replace-substrings
   (string-trim-both str)
   '(("\r" . "\\r")
     ("\n" . "\\n")
     ("\"" . "'")
     ("\v" . ""))))

(define (snake->lower-camel str)
  (let ((char-list (string->list str)))
    (call-with-output-string
      (lambda (port)
        (put-char port (char-downcase (string-ref str 0)))
        (map (lambda (char previous-char)
               (unless (char=? char #\_)
                 (put-char port (if (char=? previous-char #\_)
                                    (char-upcase char)
                                    char))))
             (drop char-list 1)
             char-list)))))
