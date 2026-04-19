;;; daemon.el --- emacs as a daemon -*- lexical-binding: t; -*-

(unless (server-running-p)
  (server-start))


(provide 'daemon)
;;; daemon.el ends here
