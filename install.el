;;; install.el --- DragonRuby Mode Installer -*- lexical-binding: t; -*-

;; Usage: M-x load-file RET /path/to/dragonruby-mode/install.el

;;; Code:

(defun dragonruby-install ()
  "Install dragonruby-mode by adding configuration to init.el."
  (interactive)
  (let* ((this-dir (file-name-directory (or load-file-name buffer-file-name)))
         (init-file (expand-file-name "init.el" user-emacs-directory))
         (config-marker ";; >>> DragonRuby Mode <<<")
         (config-block (format "
%s
(add-to-list 'load-path %S)
(require 'dragonruby-mode)
(add-hook 'ruby-mode-hook #'dragonruby-mode)
;; >>> End DragonRuby Mode <<<
" config-marker this-dir)))
    
    ;; Check if already installed
    (with-temp-buffer
      (when (file-exists-p init-file)
        (insert-file-contents init-file))
      (if (search-forward config-marker nil t)
          (message "✅ DragonRuby Mode already installed in %s" init-file)
        ;; Not installed, add config
        (with-current-buffer (find-file-noselect init-file)
          (goto-char (point-max))
          (insert config-block)
          (save-buffer)
          (message "✅ DragonRuby Mode installed! Restart Emacs or run M-x eval-buffer"))))))

;; Auto-run on load
(dragonruby-install)

(provide 'install)
;;; install.el ends here
