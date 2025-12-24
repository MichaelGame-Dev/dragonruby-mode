# Example 6: Combined Preview Demo
# concepts: args.outputs.sprites, args.outputs.solids, args.state

def tick args
  # Combined Color and Sprite Preview Demo
  # Este ejemplo muestra ambas características juntas
  
  # Inicializar paleta de colores
  args.state.colors ||= {
    bg: [20, 20, 30],           # Fondo oscuro
    ui: [100, 150, 200],        # Azul UI
    health: [255, 50, 50],      # Rojo salud
    mana: [50, 100, 255],       # Azul mana
    gold: [255, 215, 0]         # Oro
  }
  
  # Fondo con color
  args.outputs.solids << [0, 0, 1280, 720, *args.state.colors.bg]
  
  # UI con sprites y colores
  # Barra de salud
  health_bg = [50, 650, 200, 30, 50, 50, 50]  # Gris oscuro
  health_bar = [50, 650, 150, 30, *args.state.colors.health]  # Rojo
  args.outputs.solids << [health_bg, health_bar]
  
  # Barra de mana
  mana_bg = [50, 600, 200, 30, 50, 50, 50]
  mana_bar = [50, 600, 120, 30, *args.state.colors.mana]  # Azul
  args.outputs.solids << [mana_bg, mana_bar]
  
  # Icono de oro con sprite
  gold_icon = {
    x: 50,
    y: 550,
    w: 32,
    h: 32,
    path: 'sprites/circle/gold.png'  # Thumbnail visible
  }
  args.outputs.sprites << gold_icon
  
  # Texto de oro con color
  args.outputs.labels << [90, 570, "1,234", 0, 0, *args.state.colors.gold]
  
  # Player sprite con tint
  player = {
    x: 640,
    y: 360,
    w: 64,
    h: 64,
    path: 'sprites/square/blue.png',  # Thumbnail [64x64]
    r: 150,
    g: 200,
    b: 255  # Tint azul claro
  }
  args.outputs.sprites << player
  
  # Partículas de color
  10.times do |i|
    particle_color = [
      128 + rand(127),  # R
      128 + rand(127),  # G
      255               # B
    ]
    args.outputs.solids << [
      640 + i * 20,
      200,
      10,
      10,
      *particle_color
    ]
  end
end
