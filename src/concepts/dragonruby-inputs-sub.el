;;; dragonruby-inputs-sub.el --- Subconcepts of args.inputs -*- lexical-binding: t; -*-

(require 'dragonruby-registry)

;; --- args.inputs.keyboard ---
(dragonruby-register-concept
 (make-dragonruby-concept
  :id "args.inputs.keyboard"
  :name "Keyboard Input"
  :level 'basic
  :scope 'input
  :definition "Read-only hash providing current and historical keyboard state."
  :intention "To make keyboard input trivial while supporting both held keys and key events."
  :mental-model "Think of keyboard as a snapshot camera:
   - `keyboard.left` - Is the key currently pressed? (boolean)
   - `keyboard.key_down.left` - Was it just pressed THIS frame? (event)
   - `keyboard.key_held.left` - Has it been held for multiple frames? (duration)
   - `keyboard.key_up.left` - Was it just released THIS frame? (event)"
  :problems '("Key event vs key state confusion"
              "Frame-perfect input detection"
              "Key repeat handling")
  :limits '("Does not provide text input directly"
            "Does not handle IME (international keyboards)"
            "Does not detect modifier combinations automatically")
  :relations '(("contained-by" . "args.inputs")
               ("similar-to" . "args.inputs.mouse")
               ("used-in" . "player-movement"))
  :presentation '((eldoc . t))
  :evolution "May gain text input helpers, but state snapshot model is core."))

;; --- args.inputs.mouse ---
(dragonruby-register-concept
 (make-dragonruby-concept
  :id "args.inputs.mouse"
  :name "Mouse Input"
  :level 'basic
  :scope 'input
  :definition "Read-only hash providing mouse position, button state, and scroll wheel."
  :intention "To provide pixel-perfect mouse interaction without coordinate system confusion."
  :mental-model "Think of mouse as a laser pointer:
   - `mouse.x, mouse.y` - Current position in game coordinates
   - `mouse.click` - Left button just pressed THIS frame
   - `mouse.up` - Left button just released THIS frame
   - `mouse.wheel` - Scroll wheel delta (positive = up, negative = down)"
  :problems '("Screen vs game coordinate confusion"
              "Click detection timing"
              "Drag and drop implementation")
  :limits '("Does not provide hover state tracking"
            "Does not handle multi-touch"
            "Does not provide cursor customization")
  :relations '(("contained-by" . "args.inputs")
               ("provides" . "screen-coordinates")
               ("used-in" . "ui-interaction"))
  :presentation '((eldoc . t))
  :evolution "May gain gesture support, but coordinate model is stable."))

(provide 'dragonruby-inputs-sub)

;;; dragonruby-inputs-sub.el ends here
