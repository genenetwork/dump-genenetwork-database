;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((nil
  (indent-tabs-mode))
 (makefile-gmake-mode
  (indent-tabs-mode t))
 (scheme-mode
  (eval put 'map-alist 'scheme-indent-function 1)
  (eval put 'set-table-columns 'scheme-indent-function 1)
  (eval put 'triples 'scheme-indent-function 1)
  (eval put 'syntax-let 'scheme-indent-function 1)))
