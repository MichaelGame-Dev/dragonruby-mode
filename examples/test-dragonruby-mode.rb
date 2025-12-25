# test-dragonruby-mode.rb
# Archivo de prueba para verificar que todos los conceptos muestran sus definiciones

def tick(args)
  # Hover sobre "tick" debería mostrar: "The heartbeat function called 60 times per second..."
  
  # Hover sobre "args" debería mostrar: "The central data structure passed to 'tick'..."
  
  # ========== args.state ==========
  # Hover sobre "args.state" debería mostrar: "A dynamic OpenStruct..."
  args.state.player_x ||= 100
  args.state.player_y ||= 200
  
  # ========== args.inputs ==========
  # Hover sobre "args.inputs" debería mostrar: "Read-only snapshots of Keyboard, Mouse..."
  
  # args.inputs.keyboard
  if args.inputs.keyboard.key_down.left
    args.state.player_x -= 5
  end
  
  # args.inputs.mouse
  if args.inputs.mouse.click
    puts "Mouse clicked at #{args.inputs.mouse.x}, #{args.inputs.mouse.y}"
  end
  
  # ========== args.outputs ==========
  # Hover sobre "args.outputs" debería mostrar: "An ordered queue of arrays..."
  
  # args.outputs.solids
  args.outputs.solids << [100, 100, 50, 50, 255, 0, 0]
  
  # args.outputs.sprites
  args.outputs.sprites << {
    x: args.state.player_x,
    y: args.state.player_y,
    w: 64,
    h: 64,
    path: "sprites/player.png"
  }
  
  # args.outputs.labels
  args.outputs.labels << [10, 710, "DragonRuby Mode Test", 3, 255, 255, 255]
  
  # ========== Color Arrays ==========
  # Estos deberían mostrar un rectángulo de color
  red = [255, 0, 0]
  green = [0, 255, 0, 128]
  blue = [0, 0, 255, 255, 10]
  
  args.outputs.solids << [200, 200, 100, 100].merge(red)
end
