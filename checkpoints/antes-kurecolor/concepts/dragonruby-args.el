;;; dragonruby-args.el --- Definition of the args concept -*- lexical-binding: t; -*-

(require 'dragonruby-registry)

;; ============================================================================
;; REFERENCE IMPLEMENTATION
;; ============================================================================
;; This is a REFERENCE IMPLEMENTATION of a rich, complete concept.
;; Use this as a template when creating new concepts.
;; Notice how ALL fields are filled to create a cognitive tool, not just docs.
;; ============================================================================

(dragonruby-register-concept
 (make-dragonruby-concept
  :id "args"
  :name "Frame Arguments"
  :level 'core
  :scope 'frame
  :definition
  "The specific universe of data for the current 1/60th second of simulation."
  :definition-es
  "El universo específico de datos para el 1/60 de segundo actual de simulación."
  :intention
  "To centralize inputs, state, and outputs for each execution frame."
  :intention-es
  "Centralizar entradas, estado y salidas para cada frame de ejecución."
  :mental-model
  "Think of args as the Console itself:
   - Inputs: The Controller (what you press)
   - Outputs: The TV Screen (what you see)
   - State: The Memory Card (what is saved)"
  :mental-model-es
  "Piensa en args como la Consola misma:
   - Inputs: El Control (lo que presionas)
   - Outputs: La Pantalla TV (lo que ves)
   - State: La Tarjeta de Memoria (lo que se guarda)"
  :problems
  '("Global state confusion"
    "Unclear input handling"
    "Unclear rendering pipeline")
  :limits
  '("Does not contain game logic"
    "Does not render by itself"
    "Does not persist data without state")
  :relations
  '(("contains" . "args.inputs")
    ("contains" . "args.state")
    ("contains" . "args.outputs"))
  :presentation
  '((eldoc . t)
    (tooltip . optional)
    (snippet . nil))
  :evolution
  "May gain sub-concepts, but its core definition must not change."))

(provide 'dragonruby-args)
