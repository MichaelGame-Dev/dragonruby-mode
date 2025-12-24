# Sprite Preview en DragonRuby Mode

## ¿Qué es?

Una característica que muestra **thumbnails de sprites inline** con información de dimensiones cuando escribes rutas de imágenes en tu código DragonRuby.

---

## Cómo Funciona

Cuando escribes una ruta de sprite:

```ruby
sprite = {
  x: 100,
  y: 100,
  w: 64,
  h: 64,
  path: 'sprites/player.png'  # 🖼️ [64x64]
}
```

Verás:
- **Thumbnail** de la imagen al lado del path
- **Dimensiones** `[64x64]` después del path
- **⚠️ Advertencias** si hay problemas potenciales

---

## Advertencias Automáticas

### ⚠️ Imagen Muy Pequeña
```ruby
path: 'sprites/tiny.png'  # [8x8] ⚠ Very small - will be upscaled
```
**Problema**: Imágenes menores a 16x16 se verán pixeladas al escalar.

### ⚠️ Tamaño Impar
```ruby
path: 'sprites/odd.png'  # [23x47] ⚠ Odd size - may appear blurry
```
**Problema**: Tamaños que no son múltiplos de 8 pueden verse borrosos.

**Tamaños recomendados**: 16, 24, 32, 48, 64, 128, 256

---

## Ejemplos

### Ejemplo 1: Sprite Básico

```ruby
def tick args
  player = {
    x: 640,
    y: 360,
    w: 64,
    h: 64,
    path: 'sprites/square/blue.png'  # 🖼️ [64x64]
  }
  
  args.outputs.sprites << player
end
```

### Ejemplo 2: Sprite con Color Tint

```ruby
def tick args
  enemy = {
    x: 800,
    y: 360,
    w: 64,
    h: 64,
    path: 'sprites/square/red.png',  # 🖼️ [64x64]
    r: 255,  # ███ (color preview también visible)
    g: 0,
    b: 0
  }
  
  args.outputs.sprites << enemy
end
```

### Ejemplo 3: Múltiples Sprites

```ruby
def tick args
  sprites = [
    { x: 100, y: 100, w: 32, h: 32, path: 'sprites/coin.png' },     # 🖼️ [32x32]
    { x: 200, y: 100, w: 48, h: 48, path: 'sprites/gem.png' },      # 🖼️ [48x48]
    { x: 300, y: 100, w: 64, h: 64, path: 'sprites/chest.png' }     # 🖼️ [64x64]
  ]
  
  args.outputs.sprites << sprites
end
```

---

## Casos de Uso

### 1. **Verificar Imagen Correcta**
Asegúrate de que estás usando la imagen que quieres sin ejecutar el juego.

### 2. **Detectar Problemas de Escala**
Las advertencias te ayudan a identificar por qué una imagen se ve borrosa.

### 3. **Optimizar Assets**
Identifica imágenes con transparencia no usada o tamaños no óptimos.

### 4. **Desarrollo Rápido**
Ve tus sprites mientras programas, sin cambiar de ventana.

---

## Formatos Soportados

- ✅ `.png` (recomendado)
- ✅ `.jpg` / `.jpeg`

---

## Estructura de Proyecto

El sprite preview busca imágenes relativas a:
1. Directorio del archivo actual
2. Directorio padre (común en estructura `mygame/app/`)
3. Raíz del proyecto

**Estructura típica**:
```
mygame/
├── app/
│   └── main.rb          ← Tu código
└── sprites/
    ├── player.png
    └── square/
        └── blue.png
```

**En main.rb**:
```ruby
path: 'sprites/player.png'        # ✅ Encontrado
path: 'sprites/square/blue.png'   # ✅ Encontrado
```

---

## Ventajas

✅ **Visual** - Ve exactamente qué imagen estás usando  
✅ **Dimensiones** - Sabe el tamaño sin abrir el archivo  
✅ **Advertencias** - Detecta problemas antes de ejecutar  
✅ **Rápido** - No necesitas cambiar de ventana  
✅ **Automático** - Funciona mientras escribes  

---

## Actualizar Previsualizaciones

Las previsualizaciones se actualizan automáticamente cuando:
- Escribes nuevas rutas
- Modificas rutas existentes
- Guardas el archivo

Si necesitas forzar una actualización:
```
M-x dragonruby-update-sprite-previews
```

---

## Ejemplo Completo: UI de Juego

```ruby
def tick args
  # Barra de salud con sprite de corazón
  health_icon = {
    x: 50,
    y: 650,
    w: 32,
    h: 32,
    path: 'sprites/heart.png'  # 🖼️ [32x32]
  }
  args.outputs.sprites << health_icon
  
  # Barra de salud con color
  health_bar = [90, 650, 150, 30, 255, 50, 50]  # ███ Rojo
  args.outputs.solids << health_bar
  
  # Icono de mana
  mana_icon = {
    x: 50,
    y: 600,
    w: 32,
    h: 32,
    path: 'sprites/mana.png'  # 🖼️ [32x32]
  }
  args.outputs.sprites << mana_icon
  
  # Barra de mana con color
  mana_bar = [90, 600, 120, 30, 50, 100, 255]  # ███ Azul
  args.outputs.solids << mana_bar
end
```

---

## Troubleshooting

### Problema: No veo el thumbnail
**Solución**: Verifica que la ruta sea correcta y el archivo exista.

### Problema: Thumbnail muy grande
**Solución**: El tamaño máximo es 32px. Ajusta con:
```elisp
(setq dragonruby-sprite-preview-size 48)  ; Más grande
```

### Problema: Advertencia de tamaño impar
**Solución**: Redimensiona tu sprite a múltiplo de 8 (16, 24, 32, 48, 64, etc.)

---

¡Ahora puedes ver tus sprites mientras programas! 🖼️✨
