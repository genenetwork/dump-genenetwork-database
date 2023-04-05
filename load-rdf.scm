#! /usr/bin/env guile
!#

(use-modules (rnrs io ports)
             (srfi srfi-1)
             (srfi srfi-26)
             (srfi srfi-71)
             (srfi srfi-171)
             (ice-9 match)
             (ice-9 popen)
             (hashing md5)
             ((web client) #:select (http-head open-socket-for-uri))
             (web request)
             (web response)
             (web uri))

(define %graph-uri
  "http://genenetwork.org")

(define (random-cnonce len)
  "Return a random hexadecimal string LEN characters long."
  (list->string (map (lambda _
                       (let ((i (random 16)))
                         (integer->char
                          (if (< i 10)
                              (+ i (char->integer #\0))
                              (+ (- i 10)
                                 (char->integer #\a))))))
                     (iota len))))

(define (md5-digest-authorization method uri username password)
  "Return a digest authorization header to access URI with METHOD,
USERNAME and PASSWORD."
  (let* ((response response-body (http-head uri))
         (challenge (or (find (match-lambda
                                (('digest . challenge)
                                 (let ((algorithm (assq-ref challenge 'algorithm)))
                                   (or (not algorithm)
                                       (string=? algorithm "MD5")))))
                              (assq-ref (response-headers response)
                                        'www-authenticate))
                        (error "No MD5 authentication challenges advertised")))
         (realm (assq-ref challenge 'realm))
         (nonce (assq-ref challenge 'nonce))
         (qop (assq-ref challenge 'qop))
         ;; Hard-code the request counter (nc) to "1".
         (nc "1")
         (cnonce (random-cnonce 8)))
    `(digest (username . ,username)
             (realm . ,realm)
             (nonce . ,nonce)
             (uri . ,(uri-path uri))
             (algorithm . "MD5")
             (qop . ,qop)
             (nc . ,nc)
             (cnonce . ,cnonce)
             (response . ,(md5->string
                           (md5 (string->bytevector
                                 (string-join
                                  (list (md5->string
                                         (md5 (string->bytevector (string-join
                                                                   (list username realm password)
                                                                   ":")
                                                                  (make-transcoder (utf-8-codec)))))
                                        nonce
                                        nc
                                        cnonce
                                        qop
                                        (md5->string
                                         (md5 (string->bytevector (string-join (list (symbol->string method)
                                                                                     (uri-path uri))
                                                                               ":")
                                                                  (make-transcoder (utf-8-codec))))))
                                  ":")
                                 (make-transcoder (utf-8-codec))))))
             (opaque . ,(assq-ref challenge 'opaque)))))

(define* (http-upload-file method uri file #:key username password)
  "Upload FILE to URI using METHOD. USERNAME and PASSWORD, if
provided, are the username and password to authenticate with."
  (let ((authorization-headers
         (if username
             `((authorization . ,(md5-digest-authorization method uri username password)))
             '())))
    (call-with-port (open-socket-for-uri uri)
      (lambda (port)
        (let ((request (write-request (build-request
                                       uri
                                       #:method method
                                       #:headers `((connection close)
                                                   (content-length . ,(stat:size (stat file)))
                                                   ,@authorization-headers)
                                       #:port port)
                                      port)))
          (when file
            (call-with-input-file file
              (lambda (in)
                (port-transduce (tmap (cut write-request-body request <>))
                                (const #t)
                                get-bytevector-some
                                in)
                (force-output (request-port request)))))
          (read-response port))))))

(define (call-with-pipe proc mode program . args)
  "Execute PROGRAM ARGS ... in a subprocess with a pipe of MODE to
it. Call PROC with a port to that pipe. Close the pipe once PROC
exits, even if it exits non-locally. Return the value returned by
PROC."
  (let ((port #f))
    (dynamic-wind (lambda () (set! port (apply open-pipe* mode program args)))
                  (cut proc port)
                  (lambda ()
                    (let ((return-value (status:exit-val (close-pipe port))))
                      (unless (and return-value
                                   (zero? return-value))
                        (error "Invocation of program failed" (cons program args))))))))

(define* (put-graph sparql-endpoint username password rdf-file graph #:optional retry?)
  "Load RDF-FILE into GRAPH at SPARQL-ENDPOINT, a SPARQL 1.1 Graph
Store HTTP Protocol endpoint, authenticating with USERNAME and
PASSWORD. The PUT method is used, and therefore, any existing data in
the graph is deleted."
  (let ((response (http-upload-file 'PUT
                                    (build-uri (uri-scheme sparql-endpoint)
                                               #:host (uri-host sparql-endpoint)
                                               #:port (uri-port sparql-endpoint)
                                               #:path (uri-path sparql-endpoint)
                                               #:query (string-append "graph-uri=" (uri-encode graph)))
                                    rdf-file
                                    #:username username
                                    #:password password)))
    ;; Fail if response code is not 2xx.
    (unless (= (quotient (response-code response)
                         100)
               2)
      ;; When uploading a large RDF file into a clean virtuoso, it
      ;; fails for the first time, and succeeds only the second
      ;; time. It is unclear why. So, perform this ugly hack of trying
      ;; twice.
      (if retry?
          (put-graph sparql-endpoint username password rdf-file graph #f)
          (error "Putting graph failed" response)))))

(define (delete-graph port password graph)
  "Delete GRAPH from virtuoso connecting to virtuoso on PORT
authenticating as the dba user with PASSWORD."
  ;; We do this with SQL because doing it with SPARQL is too
  ;; slow. Note that this does not delete free-text index data, if
  ;; any. See
  ;; http://vos.openlinksw.com/owiki/wiki/VOS/VirtTipsAndTricksGuideDeleteLargeGraphs
  (call-with-pipe
   (lambda (out)
     (format out
             "SET DSN=localhost:~a;
SET PWD=~s;
DELETE FROM rdf_quad WHERE g = iri_to_id ('~a');"
             port
             password
             graph))
   OPEN_WRITE
   "isql"))

(define (time-thunk thunk)
  "Run THUNK and return the time taken in seconds."
  (let ((start-time (current-time)))
    (thunk)
    (- (current-time) start-time)))

(define main
  (match-lambda*
    ((_ connection-settings-file rdf-file)
     (let ((connection-settings
            (call-with-input-file connection-settings-file
              read)))
       ;; Delete existing data. We do not rely on the implicit
       ;; deletion in the PUT method of the SPARQL 1.1 Graph Store
       ;; HTTP Protocol because that is too slow.
       (format (current-output-port)
               "Existing virtuoso data deleted in ~a seconds~%"
               (time-thunk
                (cut delete-graph
                     (assq-ref connection-settings 'virtuoso-port)
                     (assq-ref connection-settings 'virtuoso-password)
                     %graph-uri)))
       ;; Load data into virtuoso.
       (format (current-output-port)
               "~a loaded into virtuoso in ~a seconds~%"
               rdf-file
               (time-thunk
                (cut put-graph
                     (build-uri
                      (assq-ref connection-settings 'sparql-scheme)
                      #:host (assq-ref connection-settings 'sparql-host)
                      #:port (assq-ref connection-settings 'sparql-port)
                      #:path "/sparql-graph-crud-auth")
                     (assq-ref connection-settings 'virtuoso-username)
                     (assq-ref connection-settings 'virtuoso-password)
                     rdf-file
                     %graph-uri
                     #t)))))
    ((arg0 _ ...)
     (format (current-error-port) "Usage: ~a CONNECTION-SETTINGS-FILE RDF-FILE~%" arg0)
     (exit #f))))

(apply main (command-line))
