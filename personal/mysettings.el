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
    (add-to-list 'default-frame-alist '(cursor-color . "green")))

(turn-on-visual-line-mode) ;; aka wrap long lines in screen
;; already enabled in prelude-vertico.el
;; (vertico-mode 1)

;; set font globally?
;; http://xahlee.info/emacs/emacs/emacs_list_and_set_font.html

(when (member "FiraCode Nerd Font" (font-family-list))
  (set-frame-font "FiraCode Nerd Font-22" t t))

;; setup popper
;; https://github.com/karthink/popper
(require 'popper)
(setq popper-reference-buffers
      '("\\*Messages\\*"
        "Output\\*$"
        "\\*Async Shell Command\\*"
        help-mode
        compilation-mode))
(global-set-key (kbd "C-`") 'popper-toggle)
(global-set-key (kbd "M-`") 'popper-cycle)
(global-set-key (kbd "C-M-`") 'popper-toggle-type)
(popper-mode +1)

;; For echo-area hints
(require 'popper-echo)
(popper-echo-mode +1)

;; workaround for json readtable error 47
(when (memq window-system '(mac ns x)) (exec-path-from-shell-initialize))

;; setup own org-capture templates
;; note org-directory defaults to ~/.emacs.d/org
;; global shortcut of org-capture in M-F6
(setq org-default-notes-file (concat org-directory "/mynotes.org"))
(setq org-capture-templates
      '(("t" "Tasks" entry
         (file+headline "" "Inbox")
         "* TODO %?\n %U")
        ("n" "Note" entry
         (file+headline "" "Notes")
         "* %?\nEntered on %U\n %i\n %a")
        ("m" "Meeting" entry
         (file+headline "" "Meetings")
         "* %?\n %U")
        ("j" "Journal Entry" entry
         (file+datetree "journal.org")
         "* %U\n%?")))

;; function for line wrapping
(defun setup-textorg-mode ()
  (set-fill-column 80)
  (column-number-mode 1)
  (setq truncate-lines t))

(add-hook 'text-mode-hook 'setup-textorg-mode)
(add-hook 'org-mode-hook 'setup-textorg-mode)

(provide 'mysettings)
;;; mysettings.el ends here
