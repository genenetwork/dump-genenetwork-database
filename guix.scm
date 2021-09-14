;; Drop into a development environment using
;;
;; guix environment -m guix.scm
;;
;; Happy hacking!

(use-modules (gnu packages autotools)
             (gnu packages compression)
             (gnu packages databases)
             (gnu packages guile)
             ((gnu packages guile-xyz) #:prefix guix:)
             (gnu packages perl)
             (gnu packages python)
             (gnu packages python-web)
             (gnu packages rdf)
             (gnu packages tls)
             (gnu packages web)
             (guix build-system gnu)
             (guix git-download)
             ((guix licenses) #:prefix license:)
             (guix packages)
             (guix utils))

;; A better version of the guile-dbi and guile-dbd-* packages are
;; under review upstream. See https://issues.guix.gnu.org/50476
;; TODO: Remove these package definitions once merged upstream.
(define guile-dbi
  (package
    (inherit guix:guile-dbi)
    (name "guile-dbi")
    (version "2.1.8")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/opencog/guile-dbi")
                    (commit (string-append "guile-dbi-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "123m4j82bi60s1v95pjh4djb7bh6zdwmljbpyg7zq8ni2gyal7lw"))))
    (arguments
     (substitute-keyword-arguments (package-arguments guix:guile-dbi)
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after 'unpack 'chdir
             (lambda _
               ;; The upstream Git repository contains all the code, so change
               ;; to the directory specific to guile-dbi.
               (chdir "guile-dbi")))))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("perl" ,perl)
       ,@(package-native-inputs guix:guile-dbi)))
    (native-search-paths
     (list (search-path-specification
            (variable "LD_LIBRARY_PATH")
            (files '("lib")))))))

(define guile-dbi-bootstrap
  (package
    (inherit guile-dbi)
    (name "guile-dbi-bootstrap")
    (inputs '())
    (arguments
     (substitute-keyword-arguments (package-arguments guile-dbi)
       ((#:make-flags _) '(list))))))

(define guile-dbd-mysql
  (let ((commit "e97589b6b018b206c901e4cc24db463407a4036b")
        (revision "0"))
    (package
      (name "guile-dbd-mysql")
      (version (git-version "2.1.6" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/opencog/guile-dbi")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0n1gv9a0kdys10a4qmnrwvg5sydwb03880asri4gqdchcj3fimni"))))
      (build-system gnu-build-system)
      (arguments
       '(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'chdir
             (lambda _
               ;; The upstream Git repository contains all the code, so change
               ;; to the directory specific to guile-dbd-mysql.
               (chdir "guile-dbd-mysql"))))))
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ("guile-dbi-bootstrap" ,guile-dbi-bootstrap)
         ("libtool" ,libtool)
         ("openssl" ,openssl)
         ("perl" ,perl)))
      (inputs
       `(("mysql" ,mysql)
         ("zlib" ,zlib)))
      (synopsis "Guile DBI driver for MySQL")
      (home-page "https://github.com/opencog/guile-dbi/tree/master/guile-dbd-mysql")
      (description "@code{guile-dbi} is a library for Guile that provides a
convenient interface to SQL databases.  This package implements the interface
for MySQL.")
      (license license:gpl2+))))

(packages->manifest
 (list guile-3.0 guile-dbi guile-dbd-mysql
       python python-rdflib python-urllib3))
