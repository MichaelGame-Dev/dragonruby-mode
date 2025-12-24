;;; dragonruby-outputs-sub.el --- Subconcepts of args.outputs -*- lexical-binding: t; -*-

(require 'dragonruby-registry)

;; --- args.outputs.sprites ---
(dragonruby-register-concept
 (make-dragonruby-concept
  :id "args.outputs.sprites"
  :name "Sprite Rendering"
  :level 'basic
  :scope 'render
  :definition "Array queue for textured rectangles (images) with optional transformations."
  :intention "To render 2D images efficiently with minimal boilerplate."
  :mental-model "Think of sprites as stickers:
   - You specify position (x, y), size (w, h), and which image (path)
   - The engine handles loading, caching, and GPU upload
   - Optional: rotation (angle), flip (flip_horizontally), tint (r, g, b, a)"
  :problems '("Manual texture loading complexity"
              "Sprite batching optimization"
              "Transform matrix calculations")
  :limits '("Does not provide sprite animation out of the box"
            "Does not handle sprite sheets automatically"
            "Does not provide particle systems")
  :relations '(("contained-by" . "args.outputs")
               ("uses" . "path-to-image")
               ("alternative-to" . "args.outputs.solids"))
  :presentation '((eldoc . t))
  :evolution "May gain sprite sheet support, but hash-based API is stable."))

;; --- args.outputs.labels ---
(dragonruby-register-concept
 (make-dragonruby-concept
  :id "args.outputs.labels"
  :name "Text Rendering"
  :level 'basic
  :scope 'render
  :definition "Array queue for rendering text with position, content, and styling."
  :intention "To display text without requiring font rendering knowledge."
  :mental-model "Think of labels as typewriter output:
   - You specify where (x, y), what (text), and how it looks (size, color, alignment)
   - The engine handles font rasterization and kerning
   - Text is always crisp, regardless of zoom level"
  :problems '("Font rendering complexity"
              "Text layout calculations"
              "Unicode support confusion")
  :limits '("Does not provide rich text formatting"
            "Does not handle text wrapping automatically"
            "Does not support custom fonts without setup")
  :relations '(("contained-by" . "args.outputs")
               ("uses" . "font-file")
               ("common-with" . "args.outputs.sprites"))
  :presentation '((eldoc . t))
  :evolution "May gain text layout helpers, but core API is stable."))

;; --- args.outputs.solids ---
(dragonruby-register-concept
 (make-dragonruby-concept
  :id "args.outputs.solids"
  :name "Solid Rectangles"
  :level 'basic
  :scope 'render
  :definition "Array queue for filled rectangles with position, size, and color."
  :intention "To provide the fastest possible way to render colored shapes."
  :mental-model "Think of solids as colored paper cutouts:
   - You specify position (x, y), size (w, h), and color (r, g, b, a)
   - No textures, no images - just pure colored rectangles
   - Perfect for UI, debug visualization, and simple graphics"
  :problems '("Primitive shape rendering verbosity"
              "Debug visualization clutter")
  :limits '("Only renders rectangles (no circles, polygons)"
            "Does not support gradients"
            "Does not provide borders/outlines")
  :relations '(("contained-by" . "args.outputs")
               ("simpler-than" . "args.outputs.sprites")
               ("faster-than" . "args.outputs.sprites"))
  :presentation '((eldoc . t))
  :evolution "Core primitive. Will not change."))

(provide 'dragonruby-outputs-sub)

;;; dragonruby-outputs-sub.el ends here
