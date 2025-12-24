# Troubleshooting DragonRuby Mode

## Problema: `M-x dragonruby-mode` no activa la extensión

### Síntomas
- Ejecutas `M-x dragonruby-mode` en Emacs
- No aparece ` DR` en la mode line
- No funciona ninguna característica

### Causas Comunes

#### 1. Error al Cargar Módulos

**Verificar errores**:
```
M-x view-echo-area-messages
```

Busca mensajes de error como:
```
Cannot open load file: No such file or directory, dragonruby-sprite-preview
```

**Solución**: Recompilar
```powershell
.\compile.bat
```

#### 2. Configuración No Cargada

**Verificar que .emacs existe**:
```powershell
Test-Path "$env:USERPROFILE\.emacs"
```

**Solución**: Reinstalar configuración
```powershell
.\install-config.bat
```

#### 3. Archivo No es Ruby

DragonRuby mode solo se activa automáticamente en archivos `.rb` que contengan `def tick args`.

**Solución**: Activar manualmente
```
M-x dragonruby-mode
```

O asegúrate de que tu archivo tenga:
```ruby
def tick args
  # ...
end
```

---

## Problema: Color Preview No Funciona

### Síntomas
- No ves cuadros de colores al lado de valores RGB
- Hexadecimales no muestran preview

### Soluciones

#### 1. Forzar Actualización
```
M-x dragonruby-update-color-previews
```

#### 2. Verificar Formato
Los valores RGB deben estar separados por comas:
```ruby
# ✅ Correcto
255, 0, 0

# ❌ Incorrecto
255 0 0
```

#### 3. Recargar Modo
```
M-x dragonruby-mode  (desactivar)
M-x dragonruby-mode  (activar)
```

---

## Problema: Sprite Preview No Funciona

### Síntomas
- No ves thumbnails de sprites
- No aparecen dimensiones

### Soluciones

#### 1. Verificar Ruta del Archivo
La imagen debe existir relativa al archivo actual:
```ruby
# Si tu archivo está en: mygame/app/main.rb
# La imagen debe estar en: mygame/sprites/player.png
path: 'sprites/player.png'  # ✅ Correcto
```

#### 2. Verificar Formato
Solo se soportan `.png`, `.jpg`, `.jpeg`:
```ruby
path: 'sprites/player.png'   # ✅
path: 'sprites/player.jpg'   # ✅
path: 'sprites/player.bmp'   # ❌
```

#### 3. Forzar Actualización
```
M-x dragonruby-update-sprite-previews
```

---

## Problema: Inspector No Abre

### Síntomas
- Presionas `C-c C-d` y no pasa nada
- Mensaje: "C-c C-d is undefined"

### Soluciones

#### 1. Verificar Modo Activo
Busca ` DR` en la mode line. Si no está:
```
M-x dragonruby-mode
```

#### 2. Usar Comando Completo
```
M-x dragonruby-inspect-concept-at-point
```

#### 3. Verificar Cursor
El cursor debe estar sobre un concepto válido:
```ruby
def tick args
           ^^^^
           (cursor aquí)
```

---

## Problema: Eldoc No Muestra Nada

### Síntomas
- Colocas cursor sobre concepto
- Minibuffer no muestra información

### Soluciones

#### 1. Activar Eldoc
```
M-x eldoc-mode
```

#### 2. Verificar Concepto Registrado
```
M-x dragonruby-inspect-concept
```
Escribe el nombre del concepto. Si no existe, no está registrado.

#### 3. Recargar Plugin
```
M-x load-file RET ~/.emacs RET
```

---

## Problema: Tema Oscuro No Se Carga

### Síntomas
- Emacs abre con tema claro
- No ves el tema wombat

### Soluciones

#### 1. Cargar Tema Manualmente
```
M-x load-theme RET wombat RET
```

#### 2. Verificar .emacs
```powershell
Get-Content "$env:USERPROFILE\.emacs" | Select-String "load-theme"
```

Debe contener:
```elisp
(load-theme 'wombat t)
```

#### 3. Reinstalar Configuración
```powershell
.\install-config.bat
```

---

## Problema: Cambios No Se Guardan

### Síntomas
- Haces cambios en archivos `.el`
- Al reiniciar Emacs, no se ven los cambios

### Soluciones

#### 1. Recompilar
Después de cualquier cambio en archivos `.el`:
```powershell
.\compile.bat
```

#### 2. Recargar Emacs
Cierra y abre Emacs, o:
```
M-x load-file RET ~/.emacs RET
```

#### 3. Limpiar Archivos Compilados
Si hay problemas persistentes:
```powershell
Get-ChildItem -Path src -Filter *.elc -Recurse | Remove-Item
.\compile.bat
```

---

## Comandos Útiles de Diagnóstico

### Ver Mensajes de Error
```
M-x view-echo-area-messages
```

### Ver Variables de Modo
```
M-x describe-mode
```

### Ver Valor de Variable
```
M-x describe-variable RET dragonruby-mode
```

### Recargar Archivo
```
M-x load-file RET ruta/al/archivo.el RET
```

### Evaluar Expresión
```
M-: (dragonruby-mode 1)
```

---

## Reinstalación Completa

Si nada funciona, reinstala desde cero:

```powershell
# 1. Limpiar compilados
Get-ChildItem -Path src -Filter *.elc -Recurse | Remove-Item

# 2. Recompilar
.\compile.bat

# 3. Reinstalar configuración
.\install-config.bat

# 4. Reiniciar Emacs
```

---

## Obtener Ayuda

Si el problema persiste:

1. Revisa [`docs/INSTALLATION.md`](file:///e:/ANTIGRAVITY/dragonruby-mode/docs/INSTALLATION.md)
2. Revisa [`docs/QUICK_START.md`](file:///e:/ANTIGRAVITY/dragonruby-mode/docs/QUICK_START.md)
3. Verifica que todos los archivos `.el` estén en su lugar
4. Comprueba que la compilación no tenga errores

---

¡La mayoría de problemas se resuelven recompilando! 🔧
