;;; dragonruby-tick.el --- Definition of the tick concept -*- lexical-binding: t; -*-

(require 'dragonruby-registry)

;; ============================================================================
;; The tick function is THE MOST FUNDAMENTAL concept in DragonRuby.
;; Everything else exists to support this 60fps execution loop.
;; ============================================================================

(dragonruby-register-concept
 (make-dragonruby-concept
  :id "tick"
  :name "Game Loop (tick)"
  :level 'core
  :scope 'frame
  :definition
  "The heartbeat function called 60 times per second with fresh args."
  :definition-es
  "La función corazón llamada 60 veces por segundo con args frescos."
  :intention
  "To provide a simple, predictable execution model that eliminates timing complexity."
  :intention-es
  "Proveer un modelo de ejecución simple y predecible que elimina la complejidad de tiempo."
  :mental-model
  "Think of tick as a movie projector:
   - Each call is one frame of your game
   - 60 calls = 1 second of gameplay
   - Your entire game logic runs inside this function
   - No threading, no async - just pure sequential execution"
  :mental-model-es
  "Piensa en tick como un proyector de cine:
   - Cada llamada es un frame de tu juego
   - 60 llamadas = 1 segundo de juego
   - Toda la lógica de tu juego corre dentro de esta función
   - Sin hilos, sin async - solo ejecución secuencial pura"
  :problems
  '("Game loop complexity"
    "Frame timing inconsistency"
    "Update/render separation confusion"
    "Threading and concurrency overhead")
  :limits
  '("Does not provide variable timestep"
    "Does not run faster than 60fps"
    "Does not automatically handle slow frames"
    "Does not provide sub-frame timing")
  :relations
  '(("receives" . "args")
    ("calls-per-second" . "60")
    ("contains" . "game-logic"))
  :presentation
  '((eldoc . t)
    (tooltip . t))
  :evolution
  "Core execution model. Will never change - it's the foundation of DragonRuby."))

(provide 'dragonruby-tick)

;;; dragonruby-tick.el ends here
