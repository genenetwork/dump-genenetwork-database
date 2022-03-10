;; Drop into a development environment using
;;
;; guix shell
;;
;; Happy hacking!

(use-modules (gnu packages autotools)
             ((gnu packages bioinformatics) #:prefix guix:)
             (gnu packages graphviz)
             (gnu packages guile)
             ((gnu packages guile-xyz) #:select (guile-sparql) #:prefix guix:)
             ((gnu packages guile-xyz) #:select (guile-dbd-mysql guile-dbi guile-libyaml))
             (guix build-system gnu)
             (guix git-download)
             ((guix licenses) #:prefix license:)
             (guix packages))

(define ccwl
  (let ((commit "51c12b7e58685b70e7cfd9612dac403cf9ee845c"))
    (package
      (inherit guix:ccwl)
      (name "ccwl")
      (version (git-version (package-version guix:ccwl) "0" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/arunisaac/ccwl")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0r86bjph11y77iskcns494xcn526lbyrhfs3sfpnqv8gc0pbgnzj"))))
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ,@(package-native-inputs guix:ccwl))))))

;; Upstream guile-sparql tests are broken. Temporarily disable them.
(define guile-sparql
  (package
    (inherit guix:guile-sparql)
    (arguments
     `(#:tests? #f))))

(define run64
  (package
    (name "run64")
    (version "0.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://git.systemreboot.net/run64")
                    (commit "e07c1f90f5436559839dea7c0231dd3ee36678ce")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1la0x80b9s6am3hag0ijnvli3fzaa4iiiqm7dmwnyyhpd6n24jqn"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list (string-append "prefix=" %output))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (home-page "https://run64.systemreboot.net")
    (synopsis "SRFI-64 test runner for Scheme")
    (description "run64 is a SRFI-64 test runner for Scheme.")
    (license license:gpl3+)))

(packages->manifest
 (list guile-3.0 guile-dbi guile-dbd-mysql
       ;; We abuse (ccwl graphviz) as a library to visualize the database
       ;; schema. Hence we need ccwl and guile-libyaml.
       ccwl graphviz guile-libyaml guile-sparql run64))
