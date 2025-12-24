;;; dragonruby-args-sub.el --- Definitions for main subconcepts of args -*- lexical-binding: t; -*-

(require 'dragonruby-registry)

;; --- args.state ---
(dragonruby-register-concept
 (make-dragonruby-concept
  :id "args.state"
  :name "Game State (Memory)"
  :level 'core
  :scope 'state
  :definition "A dynamic OpenStruct where you store EVERYTHING that must persist between frames."
  :definition-es "Un OpenStruct dinámico donde guardas TODO lo que debe persistir entre frames."
  :intention "To provide persistent memory across the 60 frames per second execution cycle."
  :intention-es "Proveer memoria persistente a través del ciclo de 60 frames por segundo."
  :mental-model "The Memory Card. If it's not in .state, it is forgotten in the next frame."
  :mental-model-es "La Tarjeta de Memoria. Si no está en .state, se olvida en el siguiente frame."
  :problems '("Frame-to-frame data loss"
              "Global variable pollution"
              "Unclear state ownership")
  :limits '("Does not persist between game sessions"
            "Does not validate data types"
            "Does not provide undo/redo")
  :relations '(("contained-by" . "args")
               ("persists-across" . "tick"))
  :presentation '((eldoc . t))
  :evolution "May gain serialization support, but core behavior remains immutable."))

;; --- args.outputs ---
(dragonruby-register-concept
 (make-dragonruby-concept
  :id "args.outputs"
  :name "Render Pipeline"
  :level 'core
  :scope 'render
  :definition "An ordered queue of arrays. What you push here gets drawn."
  :definition-es "Una cola ordenada de arrays. Lo que empujas aquí se dibuja."
  :intention "To decouple game logic from rendering, allowing the engine to optimize draw calls."
  :intention-es "Desacoplar la lógica del juego del renderizado, permitiendo al motor optimizar las llamadas de dibujo."
  :mental-model "The TV Screen. You don't 'draw', you 'queue' instructions for the GPU."
  :mental-model-es "La Pantalla TV. No 'dibujas', 'encolas' instrucciones para la GPU."
  :problems '("Immediate mode rendering confusion"
              "Draw order ambiguity"
              "Performance optimization opacity")
  :limits '("Does not guarantee draw order across different output types"
            "Does not validate array structure"
            "Does not provide z-index beyond array order")
  :relations '(("contained-by" . "args")
               ("contains" . "args.outputs.solids")
               ("contains" . "args.outputs.sprites")
               ("contains" . "args.outputs.labels"))
  :presentation '((eldoc . t))
  :evolution "May gain render layers, but queue-based model is fundamental."))

;; --- args.inputs ---
(dragonruby-register-concept
 (make-dragonruby-concept
  :id "args.inputs"
  :name "Input Hardware"
  :level 'core
  :scope 'input
  :definition "Read-only snapshots of Keyboard, Mouse, and Controller state."
  :definition-es "Capturas de solo-lectura del estado de Teclado, Mouse y Control."
  :intention "To provide consistent input state for the entire frame, preventing race conditions."
  :intention-es "Proveer estado de entrada consistente para todo el frame, previniendo condiciones de carrera."
  :mental-model "The Controller. You check it, you don't write to it."
  :mental-model-es "El Control. Lo verificas, no escribes en él."
  :problems '("Input timing inconsistency"
              "Event-driven vs polling confusion"
              "Multi-device input complexity")
  :limits '("Does not buffer input history"
            "Does not provide input remapping"
            "Does not detect input device connection/disconnection")
  :relations '(("contained-by" . "args")
               ("contains" . "args.inputs.keyboard")
               ("contains" . "args.inputs.mouse")
               ("contains" . "args.inputs.controller_one"))
  :presentation '((eldoc . t))
  :evolution "May gain gesture recognition, but read-only snapshot model is core."))

(provide 'dragonruby-args-sub)
