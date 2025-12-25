# test-sprites.rb
# Archivo de prueba para verificar el sistema de preview de sprites

def tick(args)
  # ========== SPRITES CON PATHS ==========
  # Hover sobre cada path debería mostrar:
  # - Miniatura de la imagen
  # - Dimensiones (ej: 64x64 px)
  # - Formato (PNG, JPG, etc.)
  # - Tamaño del archivo (KB)
  # - Ruta completa
  # - Click para abrir el archivo
  
  # Sprite como hash
  args.outputs.sprites << {
    x: 100,
    y: 100,
    w: 64,
    h: 64,
    path: "sprites/player.png"  # ← Hover aquí
  }
  
  # Sprite como array
  args.outputs.sprites << [200, 200, 64, 64, "sprites/enemy.png"]  # ← Hover aquí
  
  # Sprite con comillas simples (también debería funcionar)
  args.state.hero_sprite = 'sprites/hero.png'  # ← Hover aquí
  
  # Sprite en variable
  background_path = "sprites/background.png"  # ← Hover aquí
  logo = "sprites/logo.jpg"  # ← Hover aquí
  
  # Formatos soportados
  png_sprite = "sprites/character.png"   # ✅ PNG
  jpg_sprite = "sprites/photo.jpg"        # ✅ JPG
  bmp_sprite = "sprites/texture.bmp"      # ✅ BMP
  
  # Formatos NO soportados (deberían mostrar warning naranja)
  gif_anim = "sprites/animation.gif"      # ⚠️ GIF no soportado
  svg_icon = "sprites/icon.svg"           # ⚠️ SVG no soportado
  
  # Path que no existe (debería mostrar warning rojo)
  missing = "sprites/does_not_exist.png"  # ❌ Archivo no encontrado
  
  # ========== VISUAL FEEDBACK ==========
  # - Paths válidos: subrayado cyan, clickeable
  # - Paths inválidos: subrayado ondulado rojo
  # - Formatos no soportados: subrayado ondulado naranja
end
