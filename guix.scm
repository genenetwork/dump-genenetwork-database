;; Drop into a development environment using
;;
;; guix shell -m guix.scm
;;
;; Happy hacking!

(use-modules (gnu packages autotools)
             (gnu packages compression)
             (gnu packages databases)
             (gnu packages dbm)
             (gnu packages guile)
             (gnu packages guile-xyz)
             (gnu packages perl)
             (gnu packages python)
             (gnu packages python-web)
             (gnu packages rdf)
             (gnu packages tls)
             (gnu packages web)
             (guix build-system gnu)
             (guix build-system python)
             (guix download)
             (guix git-download)
             ((guix licenses) #:prefix license:)
             (guix packages)
             (guix utils))

;; A better version of the python-berkeleydb and python-rdflib
;; packages are under review upstream. See
;; https://issues.guix.gnu.org/50481
;; TODO: Remove these package definitions once merged upstream.
(define python-berkeleydb
  (package
    (name "python-berkeleydb")
    (version "18.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "berkeleydb" version))
       (sha256
        (base32
         "0m4ygc1b4wrdrhh2z43rhixcm3fm5bmylbqrvxyfrk97wxwsav9z"))))
    (build-system python-build-system)
    (arguments
     `(#:configure-flags (list (string-append "--berkeley-db="
                                              (assoc-ref %build-inputs "bdb")))
       #:phases
       (modify-phases %standard-phases
         (add-before 'unpack 'suppress-license-warning
           (lambda _
             (setenv "YES_I_HAVE_THE_RIGHT_TO_USE_THIS_BERKELEY_DB_VERSION" "1")))
         (replace 'build
           (lambda* (#:key inputs #:allow-other-keys)
             (invoke "python" "setup.py"
                     (string-append "--berkeley-db=" (assoc-ref inputs "bdb"))
                     "build")))
         (replace 'check
           (lambda* (#:key tests? inputs #:allow-other-keys)
             (when tests?
               (invoke "python" "setup.py"
                       (string-append "--berkeley-db=" (assoc-ref inputs "bdb"))
                       "check")))))))
    (inputs
     `(("bdb" ,bdb)))
    (home-page
     "https://www.jcea.es/programacion/pybsddb.htm")
    (synopsis
     "Python bindings for Oracle Berkeley DB")
    (description
     "Python bindings for Oracle Berkeley DB")
    (license #f)))

(define python-rdflib-6
  (package
   (inherit python-rdflib)
   (version "6.0.0")
   (source
    (origin
     (method url-fetch)
     (uri (pypi-uri "rdflib" version))
     (sha256
      (base32
       "0ycqczf33irq0ai6wpg4vxd9xwlpq3c41hsy411xvx16xdbxgr3w"))))
   (propagated-inputs
    `(("python-berkeleydb" ,python-berkeleydb)
      ,@(package-propagated-inputs python-rdflib)))))

(packages->manifest
 (list guile-3.0 guile-dbi guile-dbd-mysql
       python python-rdflib-6))
