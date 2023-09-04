;;; This is my own customizations copied from different parts of the Emacs
;;; ecosystem
;;; Tue 29 Aug 2023 05:56:53 AM +03
;;;Make emacs always use its own browser for opening URLs
(setq browse-url-browser-function 'eww-browse-url)
(desktop-save-mode 1)
;; try out emacs transparency as per EmacsWiki guide
;; Fri 01 Sep 2023 04:38:27 AM +03
(set-frame-parameter nil 'alpha-background 50)
(add-to-list 'default-frame-alist '(alpha-background . 50))
;; auto save hooks
(add-hook 'diary-mode (lambda) (auto-save-visited-mode))
(add-hook 'todo-mode (lambda) (auto-save-visited-mode))

;; example snippet of determining major mode
;; you can check the buffers major mode by doing M-x: major-mode <enter>
;; (if (eq major-mode 'lisp-interaction-mode)
;;     (message "Yes you are!")
;;   (message "No you're not!"))


;; some tips from Protesilaos mark buffer tutorial video

;; https://protesilaos.com/codelog/2023-06-28-emacs-mark-register-basics/
;; Make Emacs repeat the C-u C-SPC command (`set-mark-command') by
;; following it up with another C-SPC.  It is faster to type
;; C-u C-SPC, C-SPC, C-SPC, than C-u C-SPC, C-u C-SPC, C-u C-SPC...
(setq set-mark-command-repeat-pop t)

;; By default, the built-in `savehist-mode' only keeps a record of
;; minibuffer histories.  This is helpful as it surfaces the most
;; recently selected items to the top, allowing you to access them again
;; very quickly.  With the variable `savehist-additional-variables' we
;; can make `savehist-mode' keep a record of any variable we want, so
;; that it persists between Emacs sessions.  I do this to store the
;; `kill-ring' and the `register-alist'.
(setq savehist-additional-variables '(register-alist kill-ring))

(savehist-mode 1)

;; I want Emacs to write the list of bookmarks to the `bookmark-file'
;; as soon as I set a new bookmark.  The default behaviour of Emacs is
;; to write to the disk as a final step before closing Emacs.  Though
;; this can lead to data loss, such as in the case of a power failure.
;; Storing the data outright mitigates this problem.
(defun prot/bookmark-save-no-prompt (&rest _)
  "Run `bookmark-save' without prompts.

The intent of this function is to be added as an :after advice to
`bookmark-set-internal'.  Concretely, this means that when
`bookmark-set-internal' is called, this function is called right
afterwards.  We set this up because there is no hook after
setting a bookmark and we want to automatically save bookmarks at
that point."
  (funcall 'bookmark-save))

(advice-add 'bookmark-set-internal :after 'prot/bookmark-save-no-prompt)

;; After publishing the above, I learnt about this variable, which has
;; the same effect as `prot/bookmark-save-no-prompt':
(setq bookmark-save-flag 1)

;; If you are using the wonderful `consult' package, set up the
;; register preview facility with its more informative presentation:
(setq register-preview-delay 0.8
      register-preview-function #'consult-register-format)
