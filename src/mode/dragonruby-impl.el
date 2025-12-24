;;; dragonruby-impl.el --- DragonRuby Minor Mode Implementation  -*- lexical-binding: t; -*-

(require 'dragonruby-eldoc)
(require 'dragonruby-inspector)
(require 'dragonruby-concept-hints)
(require 'dragonruby-color-preview)
(require 'dragonruby-sprite-preview)

;; Import UI Logic
(require 'dragonruby-colors-ui)
(require 'dragonruby-sprites-ui)
(require 'dragonruby-navigation-ui)

(defun dragonruby--after-change (start end _len)
  "Hook to run all scanners on text change."
  (when dragonruby-mode
    ;; 1. Clean old overlays
    (dragonruby--clear-color-overlays start end)
    (dragonruby--clear-sprite-overlays start end)
    (dragonruby--clear-require-overlays start end)
    
    ;; 2. Scan window (context)
    (let ((s (max (point-min) (- start 120)))
          (e (min (point-max) (+ end 120))))
      (dragonruby--scan-colors-region s e)
      (dragonruby--scan-sprites-region s e)
      (dragonruby--scan-requires-region s e))))

(defun dragonruby--scan-all ()
  "Run full buffer scan."
  (dragonruby--scan-colors-entire-buffer)
  (dragonruby--scan-sprites-region (point-min) (point-max))
  (dragonruby--scan-requires-region (point-min) (point-max)))

;;;###autoload
(define-minor-mode dragonruby-mode
  "Minor mode for assisting DragonRuby development."
  :lighter " DR"
  :group 'dragonruby
  (if dragonruby-mode
      (progn
        (dragonruby-enable-eldoc)
        (dragonruby--scan-all)
        (add-hook 'after-change-functions #'dragonruby--after-change nil t)
        (add-hook 'completion-at-point-functions #'dragonruby--completion-at-point nil t))
    ;; Disable
    (kill-local-variable 'eldoc-documentation-function)
    (remove-hook 'after-change-functions #'dragonruby--after-change t)
    (remove-hook 'completion-at-point-functions #'dragonruby--completion-at-point t)
    (remove-overlays (point-min) (point-max) 'dragonruby-color-overlay t)
    (remove-overlays (point-min) (point-max) 'dragonruby-sprite-overlay t)
    (remove-overlays (point-min) (point-max) 'dragonruby-require-overlay t)))

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
