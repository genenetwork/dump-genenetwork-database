;; Drop into a development environment using
;;
;; guix shell -m guix.scm
;;
;; Happy hacking!

(use-modules (gnu packages bioinformatics)
             (gnu packages graphviz)
             (gnu packages guile)
             (gnu packages guile-xyz))

(packages->manifest
 (list guile-3.0 guile-dbi guile-dbd-mysql
       ;; We abuse (ccwl graphviz) as a library to visualize the database
       ;; schema. Hence we need ccwl and guile-libyaml.
       ccwl graphviz guile-libyaml))
