;;; dragonruby-core-concepts.el --- Core DragonRuby concept definitions  -*- lexical-binding: t; -*-

(require 'cl-lib)

(cl-defstruct dragonruby-concept
  id          ;; symbol: args, args.inputs, etc.
  name        ;; human-readable name
  level       ;; core | basic | advanced
  scope       ;; frame | input | render | state
  definition  ;; one-line essential definition (English)
  definition-es ;; definición en español
  intention   ;; why this concept exists (English)
  intention-es ;; por qué existe este concepto (español)
  mental-model ;; analogy or metaphor (English)
  mental-model-es ;; analogía o metáfora (español)
  problems
  limits
  relations
  presentation
  evolution)

(provide 'dragonruby-core-concepts)
