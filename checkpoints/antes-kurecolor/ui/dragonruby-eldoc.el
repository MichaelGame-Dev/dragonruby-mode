;;; dragonruby-eldoc.el --- Eldoc support for DragonRuby concepts  -*- lexical-binding: t; -*-

(require 'eldoc)
(require 'dragonruby-registry)

(defun dragonruby--symbol-at-point ()
  "Return the symbol at point, handling Ruby method calls like 'args.state'.
Matches:
- word (e.g. 'args')
- object.method (e.g. 'args.state', 'args.outputs')
- object.method.submethod (e.g. 'args.inputs.keyboard')"
  (let ((symbol-at-point (thing-at-point 'symbol)))
    (when symbol-at-point
      (save-excursion
        ;; Try to look back for dotted parents
        (if (looking-back "\\(\\w+\\)\\." (line-beginning-position))
            (let ((parent (match-string 1)))
              (concat parent "." symbol-at-point))
          symbol-at-point)))))

(defun dragonruby-eldoc-function ()
  "Eldoc function for DragonRuby concepts.
Shows English definition in minibuffer. Use C-c C-d to see full bilingual info."
  (let* ((symbol (dragonruby--symbol-at-point))
         (concept (and symbol
                       (dragonruby-get-concept symbol))))
    (when concept
      (let ((definition (dragonruby-concept-definition concept))
            (mental-model (dragonruby-concept-mental-model concept)))
        (if mental-model
            ;; Show definition + mental-model (English only for brevity)
            (format "%s — %s" definition mental-model)
          ;; Fallback to just definition
          definition)))))

(defun dragonruby-enable-eldoc ()
  "Enable DragonRuby Eldoc support in the current buffer."
  (setq-local eldoc-documentation-function
              #'dragonruby-eldoc-function)
  (eldoc-mode 1))

(provide 'dragonruby-eldoc)
