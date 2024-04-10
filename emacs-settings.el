;;;; These are settings you can enable in Emacs if desired:
;;; add macro highlighting:
(font-lock-add-keywords
 'lisp-mode
 '(("tlambda" . font-lock-keyword-face)
   ("tlambda*" . font-lock-keyword-face)))

;; add tlambda & tlambda* pretty symbols
(add-hook 'lisp-mode-hook
          '(lambda ()
             (push (cons "tlambda"
                         (list ?t '(Br . Bl) ?λ))
                   prettify-symbols-alist)
             (push (cons "tlambda*"
                         (list ?t '(Br . Bl) ?λ '(Br . Bl) ?*))
                   prettify-symbols-alist)))

;; same for slime repl
(add-hook 'slime-repl-mode-hook
          '(lambda ()
             (prettify-symbols-mode 1)
             (push (cons "tlambda"
                         (list ?t '(Br . Bl) ?λ))
                   prettify-symbols-alist)
             (push (cons "tlambda*"
                         (list ?t '(Br . Bl) ?λ '(Br . Bl) ?*))
                   prettify-symbols-alist)))
