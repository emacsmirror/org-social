;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((emacs-lisp-mode . ((eval . (progn
                               (add-to-list 'load-path
                                            (concat (locate-dominating-file
                                                     default-directory
                                                     ".dir-locals.el")
                                                    "ui"))
                               (add-to-list 'load-path
                                            (concat (locate-dominating-file
                                                     default-directory
                                                     ".dir-locals.el")
                                                    "ui/buffers")))))))
