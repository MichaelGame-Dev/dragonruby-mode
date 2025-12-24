# Roadmap - Mejoras Futuras de DragonRuby Mode

## Prioridad Alta (Próximas)

### 🎨 Color Picker Gráfico
- [ ] **Opción 1**: Integrar Color Picker de Windows (mspaint style)
  - Al hacer clic en ■ → Abre ventana gráfica de Windows
  - Seleccionar color → Se inserta automáticamente en código
  
- [ ] **Opción 2**: Instalar paquete `kurecolor`
  - Ajustar colores con teclas (H, S, L, R, G, B)
  - Más visual que el picker por texto

### 🖼️ Sprite Preview Mejorado
- [ ] Mostrar imagen inline cuando existe el archivo
- [ ] Mostrar dimensiones (WxH) de la imagen
- [ ] Advertencias de optimización:
  - Imagen muy pequeña (upscaling)
  - Tamaño no múltiplo de 8 (borrosa)
  - Transparencia no usada (optimizar)
- [ ] Click para abrir imagen en visor externo

---

## Prioridad Media

### 📚 Más Conceptos DragonRuby
- [ ] `args.geometry` (collision detection)
- [ ] `args.outputs.borders`
- [ ] `args.outputs.lines`
- [ ] `args.grid`
- [ ] `args.easing` (animaciones)

### 🔧 Snippets desde Conceptos
- [ ] Generar templates de código desde conceptos
- [ ] Autocompletar patterns comunes

---

## Prioridad Baja

### 🌐 Integración Web
- [ ] Documentación en navegador con `C-c C-h`
- [ ] Búsqueda en docs oficiales de DragonRuby

### 🎮 Ejecución
- [ ] Comando para ejecutar DragonRuby Game Toolkit
- [ ] Mostrar logs de DragonRuby en buffer

---

## Notas del Usuario

- El usuario prefiere interfaz similar a VS Code cuando sea posible
- Priorizar visualización inline sobre popups
- Traducciones español/inglés son importantes

---

## Completado ✅

- [x] Soporte bilingüe (EN/ES) en conceptos
- [x] Color preview RGB y hexadecimal
- [x] Color picker clickeable (texto)
- [x] Cuadrito de color con el color real
- [x] Sprite preview básico (detectar paths)
- [x] Compilación automática en script
- [x] Instalación permanente en Emacs
