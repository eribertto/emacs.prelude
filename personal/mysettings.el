;;; This is my own customizations copied from different parts of the Emacs
;;; ecosystem
;;; Tue 29 Aug 2023 05:56:53 AM +03
;;;Make emacs always use its own browser for opening URLs
(setq browse-url-browser-function 'eww-browse-url)
(desktop-save-mode 1)

;; try out emacs transparency as per EmacsWiki guide
;; Fri 01 Sep 2023 04:38:27 AM +03
(set-frame-parameter nil 'alpha-background 75)
(add-to-list 'default-frame-alist '(alpha-background . 75))

;; auto save hooks
(add-hook 'diary-mode (lambda) (auto-save-visited-mode))
(add-hook 'todo-mode (lambda) (auto-save-visited-mode))

;; example snippet of determining major mode
;; you can check the buffers major mode by doing M-x: major-mode <enter>
;; (if (eq major-mode 'lisp-interaction-mode)
;;     (message "Yes you are!")
;;   (message "No you're not!"))
(blink-cursor-mode 1)

;; disable whitepace mode
(setq prelude-clean-whitespace-on-save nil)
(setq prelude-flyspell nil)

(if (string= (system-name) "TP460-eos")
    (add-to-list 'default-frame-alist '(cursor-color . "yellow")))

(turn-on-visual-line-mode)
(vertico-mode 1)

;; workaround???
;; error: Keyword argument (sp-in-string-p) not one of
;; (:trigger :trigger-wrap :actions :when :unless :pre-handlers :post-handlers :wrap :bind :insert :prefix :suffix :skip-match)
;; https://list.orgmode.org/87tugzinxz.fsf@gmail.com/T/#u
(defun org-native-comp-available-p ()
  (and (fboundp 'native-comp-available-p)
       (native-comp-available-p)))


(provide 'mysettings)
;;; mysettings.el ends here
