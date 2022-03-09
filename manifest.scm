;; Drop into a development environment using
;;
;; guix shell
;;
;; Happy hacking!

(use-modules (gnu packages autotools)
             ((gnu packages bioinformatics) #:prefix guix:)
             (gnu packages graphviz)
             (gnu packages guile)
             (gnu packages guile-xyz)
             (guix git-download)
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

(packages->manifest
 (list guile-3.0 guile-dbi guile-dbd-mysql
       ;; We abuse (ccwl graphviz) as a library to visualize the database
       ;; schema. Hence we need ccwl and guile-libyaml.
       ccwl graphviz guile-libyaml guile-sparql))
