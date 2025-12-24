# Compilación de DragonRuby Mode

## ¿Por Qué Compilar?

Los archivos `.el` (Emacs Lisp) se pueden compilar a `.elc` (bytecode) para:
- ✅ **Cargar más rápido** (3-5x más rápido)
- ✅ **Usar menos memoria**
- ✅ **Detectar errores** en tiempo de compilación

---

## Cómo Compilar

### Opción 1: Usar el Script Batch (Más Fácil)

```powershell
.\compile.bat
```

Esto compilará todos los archivos automáticamente.

### Opción 2: Desde Emacs

1. Abrir Emacs
2. `M-x load-file RET compile-dragonruby.el RET`

---

## Verificar Compilación

Después de compilar, deberías ver archivos `.elc` en:
- `src/core/*.elc`
- `src/concepts/*.elc`
- `src/ui/*.elc`
- `src/mode/*.elc`
- `src/dragonruby.elc`

---

## Recompilar Después de Cambios

Si modificas algún archivo `.el`, debes recompilarlo:

```powershell
.\compile.bat
```

O desde Emacs:
```elisp
M-x byte-recompile-directory RET src RET
```

---

## Limpiar Archivos Compilados

Si quieres eliminar todos los `.elc`:

```powershell
Get-ChildItem -Path src -Filter *.elc -Recurse | Remove-Item
```

---

## Notas

- ✅ Los archivos `.elc` son **opcionales** - el plugin funciona sin ellos
- ✅ Emacs carga `.elc` automáticamente si existe
- ✅ Si hay un `.elc` más antiguo que el `.el`, Emacs usa el `.el`
