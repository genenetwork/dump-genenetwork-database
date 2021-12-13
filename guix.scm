;; Drop into a development environment using
;;
;; guix shell -m guix.scm
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
  (let ((commit "1e36c8bdc8c22dee68a3aa292c1d318bd8e0b982"))
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
           "1fcgxjxf5c7329pdd16fgz6crvg4jz84czp7kkydj99cgg2f5rkx"))))
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ,@(package-native-inputs guix:ccwl))))))

(packages->manifest
 (list guile-3.0 guile-dbi guile-dbd-mysql
       ;; We abuse (ccwl graphviz) as a library to visualize the database
       ;; schema. Hence we need ccwl and guile-libyaml.
       ccwl graphviz guile-libyaml))
