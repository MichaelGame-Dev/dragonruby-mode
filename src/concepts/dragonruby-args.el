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
  "The central data structure passed to 'tick' every frame (60 times/second).
Contains everything needed to read input, manage state, and render output.
It is your game's complete universe for that 1/60th of a second."
  :definition-es
  "La estructura central pasada a 'tick' cada frame (60 veces/segundo).
Contiene todo lo necesario para leer entrada, manejar estado y renderizar salida.
Es el universo completo de tu juego para ese 1/60 de segundo."
  :intention
  "To centralize all game systems (input, state, output) in one predictable place.
No globals, no hidden state—everything flows through args."
  :intention-es
  "Centralizar todos los sistemas del juego (entrada, estado, salida) en un lugar predecible.
Sin globales, sin estado oculto—todo fluye a través de args."
  :mental-model
  "🎮 Think of args as THE GAME CONSOLE:

📥 args.inputs = The Controller
   What buttons you press, where you move the mouse
   
💾 args.state = The Memory Card  
   Your saved game data that persists between frames
   
📺 args.outputs = The TV Screen
   What gets drawn: sprites, text, shapes
   
⏱️ args.tick_count = Frame Counter
   How many frames have passed (useful for timing)

Every 1/60th second, tick(args) gets called with fresh input,
persistent state, and a clean canvas to draw on."
  :mental-model-es
  "🎮 Piensa en args como LA CONSOLA DE JUEGO:

📥 args.inputs = El Control
   Qué botones presionas, dónde mueves el mouse
   
💾 args.state = La Tarjeta de Memoria  
   Tus datos de juego guardados que persisten entre frames
   
📺 args.outputs = La Pantalla TV
   Lo que se dibuja: sprites, texto, formas
   
⏱️ args.tick_count = Contador de Frames
   Cuántos frames han pasado (útil para timing)

Cada 1/60 de segundo, tick(args) se llama con entrada fresca,
estado persistente, y un lienzo limpio para dibujar."
  :problems
  '("Global state confusion"
    "Unclear input handling"
    "Unclear rendering pipeline"
    "Forgetting args is per-frame (not persistent by default)")
  :limits
  '("Does not contain game logic (just data)"
    "Does not render by itself (you must populate args.outputs)"
    "Does not persist data without args.state"
    "Resets every frame (except args.state)")
  :relations
  '(("contains" . "args.inputs")
    ("contains" . "args.state")
    ("contains" . "args.outputs")
    ("contains" . "args.tick_count"))
  :presentation
  '((eldoc . t)
    (tooltip . optional)
    (snippet . nil))
  :evolution
  "May gain sub-concepts, but its core definition must not change."))

(provide 'dragonruby-args)
