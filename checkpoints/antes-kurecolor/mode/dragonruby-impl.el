;;; dragonruby-impl.el --- DragonRuby Minor Mode Implementation  -*- lexical-binding: t; -*-

(require 'dragonruby-eldoc)
(require 'dragonruby-inspector)
(require 'dragonruby-concept-hints)
(require 'dragonruby-color-preview)
(require 'dragonruby-sprite-preview)

;;;###autoload
(define-minor-mode dragonruby-mode
  "Minor mode for assisting DragonRuby development."
  :lighter " DR"
  :group 'dragonruby
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-d") 'dragonruby-inspect-concept-at-point)
            map)
  (if dragonruby-mode
      (progn
        (dragonruby-enable-eldoc)
        (dragonruby-enable-concept-hints)
        (dragonruby-enable-color-preview)
        (dragonruby-enable-sprite-preview))
    (kill-local-variable 'eldoc-documentation-function)))

;; ---------------------------------------------------------------------------
;; Ruby integration
;; ---------------------------------------------------------------------------

;;;###autoload
(defun dragonruby-mode-maybe-enable ()
  "Enable `dragonruby-mode` in DragonRuby Ruby files."
  (when (and (eq major-mode 'ruby-mode)
             (save-excursion
               (goto-char (point-min))
               (re-search-forward "def\\s-+tick\\s-*(args" nil t)))
    (dragonruby-mode 1)))

(add-hook 'ruby-mode-hook #'dragonruby-mode-maybe-enable)

(provide 'dragonruby-impl)
