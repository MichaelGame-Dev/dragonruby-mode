# Example 4: Color Preview Demo
# concepts: args.outputs.solids, args.outputs.labels

def tick args
  # Color Preview Demo
  # Los colores RGB se mostrarán con un cuadro de color al lado
  
  # Colores primarios RGB
  args.outputs.solids << [100, 500, 50, 50, 255, 0, 0]      # Rojo
  args.outputs.solids << [200, 500, 50, 50, 0, 255, 0]      # Verde
  args.outputs.solids << [300, 500, 50, 50, 0, 0, 255]      # Azul
  
  # Colores secundarios
  args.outputs.solids << [100, 400, 50, 50, 255, 255, 0]    # Amarillo
  args.outputs.solids << [200, 400, 50, 50, 255, 0, 255]    # Magenta
  args.outputs.solids << [300, 400, 50, 50, 0, 255, 255]    # Cyan
  
  # Escala de grises
  args.outputs.solids << [100, 300, 50, 50, 0, 0, 0]        # Negro
  args.outputs.solids << [200, 300, 50, 50, 128, 128, 128]  # Gris
  args.outputs.solids << [300, 300, 50, 50, 255, 255, 255]  # Blanco
  
  # Colores personalizados
  args.outputs.solids << [100, 200, 50, 50, 255, 128, 0]    # Naranja
  args.outputs.solids << [200, 200, 50, 50, 128, 0, 128]    # Púrpura
  args.outputs.solids << [300, 200, 50, 50, 0, 128, 64]     # Verde oscuro
  
  # También funciona con hexadecimales
  red_hex = "#FF0000"
  green_hex = "#00FF00"
  blue_hex = "#0000FF"
  
  # Colores con alpha (transparencia)
  args.outputs.solids << [100, 100, 50, 50, 255, 0, 0, 128]    # Rojo semi-transparente
  args.outputs.solids << [200, 100, 50, 50, 0, 255, 0, 128]    # Verde semi-transparente
  args.outputs.solids << [300, 100, 50, 50, 0, 0, 255, 128]    # Azul semi-transparente
  
  # Labels con colores
  args.outputs.labels << [100, 50, "Rojo", 0, 0, 255, 0, 0]
  args.outputs.labels << [200, 50, "Verde", 0, 0, 0, 255, 0]
  args.outputs.labels << [300, 50, "Azul", 0, 0, 0, 0, 255]
end
