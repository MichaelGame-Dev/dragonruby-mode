# Instalación Permanente de DragonRuby Mode

## Problema

Cada vez que abres Emacs, necesitas cargar manualmente el plugin con `-l .emacs-init.el`. Queremos que se cargue **automáticamente** siempre.

---

## Solución: Configuración Permanente

Hay 2 formas de hacer que dragonruby-mode se cargue automáticamente:

### **Opción 1: Archivo de Configuración de Usuario (Recomendada)**

Emacs busca automáticamente un archivo de configuración en tu carpeta de usuario. Vamos a usarlo.

#### **Paso 1: Copiar Configuración**

Ejecuta en PowerShell:

```powershell
# Copiar configuración a archivo de usuario de Emacs
Copy-Item "e:\ANTIGRAVITY\dragonruby-mode\.emacs-init.el" "$env:USERPROFILE\.emacs" -Force
```

O manualmente:
1. Copia el archivo `.emacs-init.el`
2. Pégalo en `C:\Users\MACGYBER\`
3. Renómbralo a `.emacs` (sin `-init`)

#### **Paso 2: Verificar**

Abre Emacs normalmente (sin `-l`):
```powershell
& "C:\Program Files\Emacs\emacs-29.4\bin\emacs.exe"
```

¡Debería cargar dragonruby-mode automáticamente! ✅

---

### **Opción 2: Directorio .emacs.d (Más Organizado)**

Esta es la forma moderna y más organizada.

#### **Paso 1: Crear Estructura**

```powershell
# Crear directorio de configuración
New-Item -ItemType Directory -Path "$env:USERPROFILE\.emacs.d" -Force

# Copiar configuración
Copy-Item "e:\ANTIGRAVITY\dragonruby-mode\.emacs-init.el" "$env:USERPROFILE\.emacs.d\init.el" -Force
```

#### **Paso 2: Verificar**

Abre Emacs:
```powershell
emacs
```

¡Debería funcionar automáticamente! ✅

---

## ¿Qué Archivo Usa Emacs?

Emacs busca configuración en este orden:

1. `~/.emacs.d/init.el` ← **Recomendado** (moderno)
2. `~/.emacs` ← Clásico
3. `~/.emacs.el`

**Nota**: `~` en Windows es `C:\Users\MACGYBER\`

---

## Configuración Actual

Tu archivo `.emacs-init.el` ya tiene todo configurado:

```elisp
;; Desactivar pantalla de bienvenida
(setq inhibit-startup-screen t)

;; Activar tema oscuro (wombat)
(load-theme 'wombat t)

;; Cargar DragonRuby Mode
(add-to-list 'load-path "e:/ANTIGRAVITY/dragonruby-mode/src")
(add-to-list 'load-path "e:/ANTIGRAVITY/dragonruby-mode/src/core")
(add-to-list 'load-path "e:/ANTIGRAVITY/dragonruby-mode/src/ui")
(add-to-list 'load-path "e:/ANTIGRAVITY/dragonruby-mode/src/mode")
(add-to-list 'load-path "e:/ANTIGRAVITY/dragonruby-mode/src/concepts")

(require 'dragonruby)

;; Mensaje de bienvenida personalizado
(message "DragonRuby Mode cargado. Presiona C-c C-d para inspeccionar conceptos.")

;; Abrir directorio de ejemplos
(dired "e:/ANTIGRAVITY/dragonruby-mode/examples")
```

---

## Scripts Actualizados

### **Script de Instalación Rápida**

Crea este archivo: `install-config.bat`

```batch
@echo off
echo ========================================
echo Instalando DragonRuby Mode en Emacs
echo ========================================
echo.

REM Opción 1: Copiar a .emacs
copy /Y ".emacs-init.el" "%USERPROFILE%\.emacs"

echo.
echo ========================================
echo Instalacion completada!
echo ========================================
echo.
echo DragonRuby Mode se cargara automaticamente
echo la proxima vez que abras Emacs.
echo.
pause
```

### **Usar el Script**

```powershell
.\install-config.bat
```

---

## Verificar Instalación

### **1. Abrir Emacs sin argumentos**

```powershell
emacs
```

### **2. Verificar que se cargó**

Deberías ver:
- ✅ Tema oscuro activo
- ✅ Directorio de ejemplos abierto
- ✅ Mensaje: "DragonRuby Mode cargado..."

### **3. Probar funcionalidad**

Abre un archivo Ruby:
```
C-x C-f examples/01_hello_world.rb
```

Verifica:
- ✅ ` DR` en la mode line
- ✅ Eldoc funciona
- ✅ `C-c C-d` abre inspector
- ✅ Color preview visible
- ✅ Sprite preview visible

---

## Actualizar Configuración

Si haces cambios en dragonruby-mode:

### **1. Recompilar**
```powershell
.\compile.bat
```

### **2. Recargar Emacs**

Dentro de Emacs:
```
M-x load-file RET ~/.emacs RET
```

O simplemente reinicia Emacs.

---

## Troubleshooting

### Problema: Emacs no carga el plugin

**Solución 1**: Verifica que el archivo existe
```powershell
Test-Path "$env:USERPROFILE\.emacs"
```

**Solución 2**: Verifica el contenido
```powershell
Get-Content "$env:USERPROFILE\.emacs"
```

**Solución 3**: Revisa errores de Emacs
Abre Emacs y presiona:
```
M-x view-echo-area-messages
```

### Problema: Conflicto con configuración existente

Si ya tienes un `.emacs`, **no lo sobrescribas**. En su lugar:

1. Abre tu `.emacs` existente
2. Agrega al final:
```elisp
;; Cargar DragonRuby Mode
(load-file "e:/ANTIGRAVITY/dragonruby-mode/.emacs-init.el")
```

---

## Resumen

**Para instalación permanente**:

```powershell
# Copiar configuración
Copy-Item ".emacs-init.el" "$env:USERPROFILE\.emacs" -Force

# Abrir Emacs
emacs
```

**¡Listo!** DragonRuby Mode se cargará automáticamente cada vez. 🎉
