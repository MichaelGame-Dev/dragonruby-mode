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

(defun dragonruby--put-hover-overlay (start end text)
  "Put a lightweight overlay with help-echo text."
  ;; Remove old overlay at this exact spot to avoid stacking
  (let ((overlays (overlays-at start)))
    (dolist (o overlays)
      (when (overlay-get o 'dragonruby-hover)
        (delete-overlay o))))
  
  (let ((ov (make-overlay start end)))
    (overlay-put ov 'dragonruby-hover t)
    (overlay-put ov 'help-echo text)
    ;; Auto-delete overlay when cursor moves away could be complex, 
    ;; so for now we rely on re-scans or let them persist lightly.
    ;; Better strategy: The 'help-echo' works on mouse hover naturally.
    ov))

(defun dragonruby-eldoc-function ()
  "Eldoc function for DragonRuby concepts.
Provides minibuffer info AND mouse hover tooltips."
  (let* ((bounds (bounds-of-thing-at-point 'symbol))
         (symbol (dragonruby--symbol-at-point))
         (concept (and symbol (dragonruby-get-concept symbol))))
    
    (when concept
      (let* ((definition (dragonruby-concept-definition concept))
             (mental-model (dragonruby-concept-mental-model concept))
             (summary (if mental-model
                          (format "%s\n\n💡 Mental Model:\n%s" definition mental-model)
                        definition))
             (message (if mental-model
                          (format "%s — %s" definition mental-model)
                        definition)))
        
        ;; 1. Add Mouse Hover Tooltip (Invisible Overlay)
        (when bounds
          (dragonruby--put-hover-overlay (car bounds) (cdr bounds) summary))
          
        ;; 2. Return Minibuffer Message
        message))))

(defun dragonruby-enable-eldoc ()
  "Enable DragonRuby Eldoc support in the current buffer."
  (setq-local eldoc-documentation-function
              #'dragonruby-eldoc-function)
  (eldoc-mode 1))

(provide 'dragonruby-eldoc)
