#! /usr/bin/env guile
!#

(use-modules (json)
             (ice-9 ftw)
             (ice-9 match)
             (dump triples))



(define %dump-directory
  (list-ref (command-line) 2))

(define %data-directory
  (list-ref (command-line) 1))



(define (json-metadata->rdf path)
  "Given a PATH that contains a json file, fetch the metadata embedded
inside it."
  (if (access? path F_OK)
      (let* ((data (assoc-ref (call-with-input-file
                                  path
                                (lambda (port)
                                  (json->scm port)))
                              "metadata"))
             (name (or (assoc-ref data "name")
                       (assoc-ref data "displayName"))))
        (match data
          (((key . value) ...)
           (map
            (lambda (a b)
              (format
               #f "gn:sampledata_~a gn:sampledata:~A ~a ."
               name a (format #f "~s"
                              (cond ((boolean? b)
                                     (if b "True" "False"))
                                    (else b)))))
            key value))))))

(define (run-proc-on-files path proc)
  (define (enter? name stat result)
    (not (member (basename name) '(".git" ".svn" "CVS"))))
  (define (leaf name stat result)
    (proc name))
  (define (down name stat result) result)
  (define (up name stat result) result)
  (define (skip name stat result) result)

  ;; Ignore unreadable files/directories but warn the user.
  (define (error name stat errno result)
    (format (current-error-port) "warning: ~a: ~a~%"
            name (strerror errno))
    result)
  (file-system-fold enter? leaf down up skip error 0 path))

(define (dump-rdf path)
  (with-output-to-file
      (string-append %dump-directory "/sampledata.ttl")
    (lambda ()
      (prefix "gn:" "<http://genenetwork.org/>")
      (newline)
      (run-proc-on-files
       %data-directory
       (lambda (file)
         (when (string-suffix? "json" file)
           (map (lambda (line)
                  (display line)
                  (newline))
                (json-metadata->rdf file))))))))

(dump-rdf %data-directory)
