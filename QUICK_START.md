# Guía Rápida: Emacs con DragonRuby Mode

## Lanzar Emacs con Tema Oscuro y Plugin

### Opción 1: Con Archivo Específico (Recomendado)
```powershell
.\emacs-dragonruby.bat examples\01_hello_world.rb
```

### Opción 2: Solo Emacs con Plugin
```powershell
.\emacs-dragonruby.bat
```

---

## Qué Esperar al Abrir

✅ **Tema oscuro** (wombat) activado automáticamente  
✅ **DragonRuby mode** cargado (verás ` DR` en la mode line)  
✅ **Sin pantalla de bienvenida** (directo al código)  
✅ **Directorio de ejemplos** abierto en panel lateral  

---

## Cambiar de Tema Oscuro

Si quieres probar otros temas oscuros, edita [`.emacs-init.el`](file:///e:/ANTIGRAVITY/dragonruby-mode/.emacs-init.el#L9):

```elisp
;; Cambia 'wombat por otro tema:
(load-theme 'wombat t)        ; ← Actual (oscuro suave)
(load-theme 'tango-dark t)    ; ← Oscuro clásico
(load-theme 'misterioso t)    ; ← Azul oscuro
(load-theme 'deeper-blue t)   ; ← Azul profundo
(load-theme 'manoj-dark t)    ; ← Negro puro
```

Guarda el archivo y reinicia Emacs.

---

## Probar Temas Sin Reiniciar

Dentro de Emacs:
1. `M-x load-theme RET`
2. Escribe el nombre del tema
3. Presiona `Enter`

Temas disponibles:
- `wombat` (recomendado)
- `tango-dark`
- `misterioso`
- `deeper-blue`
- `manoj-dark`
- `wheatgrass`

---

## Verificar que Todo Funciona

Cuando Emacs se abra, verifica:

### 1. Tema Oscuro ✅
- El fondo debería ser oscuro (gris/negro)
- El texto debería ser claro (blanco/colores claros)

### 2. DragonRuby Mode Activo ✅
- Busca ` DR` en la barra inferior (mode line)
- Coloca cursor sobre `args` → eldoc muestra definición

### 3. Comandos Funcionan ✅
- `C-c C-d` → Abre inspector de conceptos
- `C-x C-f` → Abrir archivo
- `C-x C-s` → Guardar archivo

---

## Comandos Útiles

| Acción | Comando |
|--------|---------|
| **Inspeccionar concepto** | `C-c C-d` |
| **Abrir archivo** | `C-x C-f` |
| **Guardar** | `C-x C-s` |
| **Cambiar tema** | `M-x load-theme` |
| **Cerrar buffer** | `C-x k` |
| **Salir de Emacs** | `C-x C-c` |

---

## Solución de Problemas

### Problema: No veo tema oscuro
**Solución**: Verifica que `.emacs-init.el` tenga la línea:
```elisp
(load-theme 'wombat t)
```

### Problema: No veo ` DR` en mode line
**Solución**: Activa manualmente:
```
M-x dragonruby-mode
```

### Problema: Quiero tema claro
**Solución**: Edita `.emacs-init.el` línea 9:
```elisp
(load-theme 'leuven t)  ; Tema claro
```

---

## Archivos de Configuración

- **`.emacs-init.el`** - Configuración principal (tema, plugin, etc.)
- **`emacs-dragonruby.bat`** - Script de lanzamiento
- **`compile.bat`** - Compilar plugin a bytecode

---

¡Listo para programar DragonRuby con estilo! 🌙🚀
