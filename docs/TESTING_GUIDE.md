# Guía de Prueba: DragonRuby Mode en Emacs

## Paso 1: Lanzar Emacs con el Plugin

Ejecuta en PowerShell:
```powershell
.\emacs-dragonruby.bat examples\01_hello_world.rb
```

---

## Paso 2: Verificar que el Modo Está Activo

Cuando Emacs se abra, busca en la **barra inferior** (mode line):
```
-UUU:----F1  01_hello_world.rb   (Ruby DR)
                                        ^^
                                        Esto indica que dragonruby-mode está activo
```

Si ves ` DR`, ¡el plugin está funcionando! ✅

---

## Paso 3: Probar Eldoc (Ayuda Contextual)

1. **Coloca el cursor** sobre la palabra `args` en la línea 4:
   ```ruby
   def tick args
              ^^^^
              (pon el cursor aquí)
   ```

2. **Mira la barra inferior** (minibuffer)
   
   Deberías ver algo como:
   ```
   The specific universe of data for the current 1/60th second of simulation. — Think of args as the Console itself...
   ```

   Esto es **eldoc mostrando la definición + mental-model** ✅

---

## Paso 4: Probar el Inspector Interactivo

1. **Con el cursor sobre `args`**, presiona: `C-c C-d`
   - Esto es: `Ctrl+c`, luego `Ctrl+d`

2. **Se abrirá un buffer nuevo** con el concepto completo:
   ```
   Frame Arguments (args)
   [core | frame]
   
   ═══════════════════════════════════════════════
   
   DEFINITION
     The specific universe of data for the current 1/60th second...
   
   MENTAL MODEL
     Think of args as the Console itself:
      - Inputs: The Controller (what you press)
      - Outputs: The TV Screen (what you see)
      - State: The Memory Card (what is saved)
   
   PROBLEMS IT SOLVES
     • Global state confusion
     • Unclear input handling
     ...
   ```

3. **Navegar entre conceptos relacionados**:
   - Presiona `n` para ir a la siguiente relación
   - Presiona `RET` (Enter) para inspeccionar ese concepto
   - Presiona `q` para cerrar el inspector

---

## Paso 5: Probar Concept Hints (Comentarios Interactivos)

1. **Abre el archivo** `examples/02_movement.rb`:
   - Presiona: `C-x C-f` (Ctrl+x, luego Ctrl+f)
   - Escribe: `examples/02_movement.rb`
   - Presiona: `RET` (Enter)

2. **Busca la línea 2**:
   ```ruby
   # concepts: args, args.state, args.inputs, tick
   ```

3. **Los conceptos deberían estar subrayados**

4. **Haz click** (o presiona `RET`) sobre `tick`
   - Se abrirá el inspector mostrando el concepto de `tick`

---

## Paso 6: Navegar la Red de Conceptos

1. **Inspecciona el concepto** `args.outputs`:
   - Presiona: `M-x` (Alt+x)
   - Escribe: `dragonruby-inspect-concept`
   - Presiona: `RET`
   - Escribe: `args.outputs`
   - Presiona: `RET`

2. **Verás la sección RELATIONS**:
   ```
   RELATIONS
     • contained-by → args
     • contains → args.outputs.solids
     • contains → args.outputs.sprites
     • contains → args.outputs.labels
   ```

3. **Navega**:
   - Presiona `n` hasta llegar a `args.outputs.sprites`
   - Presiona `RET` para inspeccionar ese concepto
   - Verás el mental-model: "Think of sprites as stickers..."

---

## Comandos Útiles de Emacs

| Acción | Comando |
|--------|---------|
| **Abrir archivo** | `C-x C-f` |
| **Guardar archivo** | `C-x C-s` |
| **Cerrar buffer** | `C-x k` |
| **Salir de Emacs** | `C-x C-c` |
| **Inspeccionar concepto** | `C-c C-d` |
| **Cancelar comando** | `C-g` |
| **Buscar** | `C-s` |
| **Ir a línea** | `M-g g` |

---

## Qué Esperar (Checklist)

- [ ] Emacs se abre sin errores
- [ ] El archivo `01_hello_world.rb` está visible
- [ ] La mode line muestra ` DR`
- [ ] Eldoc muestra información cuando el cursor está sobre `args`
- [ ] `C-c C-d` abre el inspector de conceptos
- [ ] Los conceptos en comentarios están subrayados
- [ ] Puedo navegar entre conceptos relacionados con `n` + `RET`

---

## Si Algo No Funciona

### Problema: No veo ` DR` en la mode line
**Solución**: El modo no se activó automáticamente. Ejecuta:
- `M-x dragonruby-mode` (Alt+x, luego escribe `dragonruby-mode`)

### Problema: Eldoc no muestra nada
**Solución**: Verifica que eldoc esté activo:
- `M-x eldoc-mode` (debería decir "Eldoc mode enabled")

### Problema: `C-c C-d` no funciona
**Solución**: Ejecuta manualmente:
- `M-x dragonruby-inspect-concept-at-point`

### Problema: Los conceptos en comentarios no están subrayados
**Solución**: Refresca el highlighting:
- `M-x font-lock-flush`

---

## Siguiente Nivel: Explorar Todos los Conceptos

Una vez que todo funcione, prueba inspeccionar estos conceptos:

1. **`tick`** - El game loop de 60fps (mental-model: proyector de cine)
2. **`args.state`** - Persistencia de datos (mental-model: memory card)
3. **`args.outputs.sprites`** - Rendering de imágenes (mental-model: stickers)
4. **`args.inputs.keyboard`** - Input de teclado (mental-model: snapshot camera)

---

**¡Listo para empezar!** 🚀
