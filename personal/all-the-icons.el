;; Configuration for all the icons
;; Wed 06 Sep 2023 08:26:29 PM +03

;; https://github.com/domtronn/all-the-icons.el
(when (display-graphic-p)
  (require 'all-the-icons))
;; or
;; (use-package all-the-icons
;;   :if (display-graphic-p))
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
(provide 'all-the-icons)
;;; all-the-icons.el ends here
