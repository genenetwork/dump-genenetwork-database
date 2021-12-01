;; Drop into a development environment using
;;
;; guix shell -m guix.scm
;;
;; Happy hacking!

(use-modules (gnu packages guile)
             (gnu packages guile-xyz))

(packages->manifest
 (list guile-3.0 guile-dbi guile-dbd-mysql))
