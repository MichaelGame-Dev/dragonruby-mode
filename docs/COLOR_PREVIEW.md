# Color Picker - Documentación Completa

## Estado Actual ✅

### Cómo Funciona

Cuando escribes valores RGB en tu código DragonRuby:

```ruby
args.outputs.solids << [100, 100, 50, 50, 255, 0, 0]
                                          ^^^^^^^^^
                                          Aquí aparece: ■ (cuadrito rojo)
```

### Características Actuales

1. **Cuadrito de Color (■)**
   - Se muestra ANTES de los valores RGB
   - El cuadrito está **relleno** con el color real
   - Tiene borde gris para que sea visible en fondos oscuros

2. **Formatos Detectados**
   - RGB: `255, 0, 0`
   - RGBA: `255, 0, 0, 128`
   - Hexadecimal: `#FF0000` o `#F00`

3. **Click para Cambiar Color**
   - Al hacer **clic** en el cuadrito → Abre selector de color
   - Al presionar **Enter** sobre el cuadrito → Abre selector de color
   - El selector muestra nombres de colores (red, blue, green, etc.)
   - Puedes escribir hexadecimal (#FF5500)
   - Al seleccionar → El código se actualiza automáticamente

---

## Cómo Usar

### Ver Preview de Color
Simplemente escribe valores RGB:
```ruby
# El cuadrito aparece automáticamente
args.outputs.solids << [100, 100, 50, 50, 255, 128, 0]
#                                          ■ naranja
```

### Cambiar Color con Click

1. Haz **clic** en el cuadrito ■
2. Escribe el nombre del color o hexadecimal:
   - `red`, `blue`, `green`, `orange`, etc.
   - `#FF5500`, `#00AAFF`, etc.
3. Presiona **Enter**
4. ¡El código se actualiza automáticamente!

### Cambiar Color con Teclado

1. Pon el cursor sobre el cuadrito ■
2. Presiona **Enter** o **RET**
3. Escribe el color
4. Presiona **Enter**

---

## Colores Disponibles (Nombres)

Puedes usar estos nombres de colores:

### Básicos
- `black`, `white`
- `red`, `green`, `blue`
- `yellow`, `cyan`, `magenta`

### Extendidos
- `orange`, `pink`, `purple`, `violet`
- `brown`, `gold`, `silver`
- `navy`, `teal`, `olive`
- `coral`, `salmon`, `turquoise`

### Grises
- `gray`, `grey`, `darkgray`, `lightgray`
- `dimgray`, `slategray`

### Tonos de Rojo
- `crimson`, `firebrick`, `indianred`
- `lightcoral`, `maroon`, `darkred`

### Tonos de Azul
- `navy`, `darkblue`, `royalblue`
- `steelblue`, `skyblue`, `lightblue`

### Tonos de Verde
- `darkgreen`, `forestgreen`, `limegreen`
- `springgreen`, `seagreen`, `mediumseagreen`

**Tip**: Presiona **Tab** en el minibuffer para ver todos los colores disponibles.

---

## Hexadecimales

También puedes escribir valores hexadecimales:

```
#FF0000  → Rojo
#00FF00  → Verde
#0000FF  → Azul
#FFFF00  → Amarillo
#FF00FF  → Magenta
#00FFFF  → Cyan
#FFFFFF  → Blanco
#000000  → Negro
#808080  → Gris
#FFA500  → Naranja
```

---

## Ejemplo Práctico

**Antes** (quieres cambiar el azul a naranja):
```ruby
args.outputs.solids << [100, 100, 50, 50, 0, 0, 255]
#                                          ■ azul
```

**Proceso**:
1. Click en ■
2. Escribes: `orange`
3. Enter

**Después**:
```ruby
args.outputs.solids << [100, 100, 50, 50, 255, 165, 0]
#                                          ■ naranja
```

---

## Limitaciones Actuales

- ❌ No hay ventana gráfica de color picker (como VS Code)
- ❌ No hay sliders RGB/HSL
- ❌ No hay previsualización antes de seleccionar

## Próximas Mejoras (Roadmap)

- [ ] Integrar Color Picker gráfico de Windows
- [ ] Agregar paquete `kurecolor` para ajuste con teclas

---

## Comandos Útiles

| Acción | Comando |
|--------|---------|
| Forzar actualización de colores | `M-x dragonruby-update-color-previews` |
| Ver todos los colores disponibles | En selector, presiona `Tab` |
