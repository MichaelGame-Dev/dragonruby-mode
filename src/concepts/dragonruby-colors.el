;;; dragonruby-colors.el --- Concept definition for Colors -*- lexical-binding: t; -*-

(require 'dragonruby-registry)

(dragonruby-register-concept
 (make-dragonruby-concept
  :id "color-array"
  :name "Color Array"
  :level 'basic
  :scope 'render
  :definition "An array of integers [r, g, b, a, meta] used to define color and opacity."
  :mental-model "Paint Bucket. Red, Green, Blue, Alpha (0-255)."
  :presentation '((overlay . t))))

(provide 'dragonruby-colors)
