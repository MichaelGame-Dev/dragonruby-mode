# Formato Bilingüe Mejorado

## Cambios Implementados ✅

### 1. **Eldoc (Minibuffer)** - Solo Inglés
El minibuffer ahora muestra **solo inglés** para que sea más legible:

```
The specific universe of data for the current 1/60th second of simulation. — Think of args as the Console itself...
```

**Por qué**: El minibuffer es pequeño y mostrar ambos idiomas lo hacía difícil de leer.

---

### 2. **Inspector (`C-c C-d`)** - Bloques Separados

El inspector ahora muestra **bloques completos** separados por idioma:

```
Frame Arguments (args)
[core | frame]

═══════════════════════════════════════════════════════════════
ENGLISH
═══════════════════════════════════════════════════════════════

DEFINITION
  The specific universe of data for the current 1/60th second of simulation.

INTENTION (Why it exists)
  To centralize inputs, state, and outputs for each execution frame.

MENTAL MODEL
  Think of args as the Console itself:
   - Inputs: The Controller (what you press)
   - Outputs: The TV Screen (what you see)
   - State: The Memory Card (what is saved)

PROBLEMS IT SOLVES
  • Global state confusion
  • Unclear input handling
  • Unclear rendering pipeline

LIMITS (What it does NOT do)
  • Does not contain game logic
  • Does not render by itself
  • Does not persist data without state

═══════════════════════════════════════════════════════════════
ESPAÑOL
═══════════════════════════════════════════════════════════════

DEFINICIÓN
  El universo específico de datos para el 1/60 de segundo actual de simulación.

INTENCIÓN (Por qué existe)
  Centralizar entradas, estado y salidas para cada frame de ejecución.

MODELO MENTAL
  Piensa en args como la Consola misma:
   - Inputs: El Control (lo que presionas)
   - Outputs: La Pantalla TV (lo que ves)
   - State: La Tarjeta de Memoria (lo que se guarda)

═══════════════════════════════════════════════════════════════

RELATIONS
  • contains → args.inputs
  • contains → args.state
  • contains → args.outputs

EVOLUTION
  May gain sub-concepts, but its core definition must not change.

═══════════════════════════════════════════════════════════════

Press 'q' to close | 'n'/'p' to navigate | RET to follow relation
```

---

## Ventajas del Nuevo Formato

✅ **Más legible** - Bloques separados son más fáciles de leer  
✅ **Elige tu idioma** - Lee el bloque que prefieras (EN o ES)  
✅ **Minibuffer limpio** - Solo inglés, no saturado  
✅ **Inspector completo** - Ambos idiomas disponibles cuando necesites profundizar  
✅ **Colores** - "ENGLISH" en cyan, "ESPAÑOL" en amarillo para fácil identificación  

---

## Cómo Usar

### Para Ayuda Rápida (Inglés)
1. Coloca cursor sobre concepto
2. Mira minibuffer (abajo)
3. Verás definición en inglés

### Para Ayuda Completa (Bilingüe)
1. Coloca cursor sobre concepto
2. Presiona `C-c C-d`
3. Lee el bloque que prefieras:
   - **ENGLISH** (arriba, cyan)
   - **ESPAÑOL** (abajo, amarillo)

---

## Sobre el Minibuffer

**Pregunta**: ¿Puedo mover el minibuffer?

**Respuesta**: No, el minibuffer en Emacs está fijo en la parte inferior. Pero puedes:
- Usar `C-c C-d` para ver todo en un buffer grande (que SÍ puedes mover/redimensionar)
- El inspector se abre en un panel lateral que puedes hacer más grande con `C-x ^` (más alto) o `C-x }` (más ancho)

---

## Navegar en el Inspector

| Tecla | Acción |
|-------|--------|
| `n` | Ir a siguiente relación |
| `p` | Ir a relación anterior |
| `RET` | Inspeccionar relación seleccionada |
| `q` | Cerrar inspector |
| `C-x ^` | Hacer ventana más alta |
| `C-x }` | Hacer ventana más ancha |

---

## Ejemplo Práctico

```ruby
def tick args
  args.state.player ||= { x: 640, y: 360 }
end
```

**Cursor sobre `args.state`**:

**Minibuffer** (rápido):
```
A dynamic OpenStruct where you store EVERYTHING that must persist between frames.
```

**Inspector** (`C-c C-d`) (completo):
```
═══════════════════════════════════════════════════════════════
ENGLISH
═══════════════════════════════════════════════════════════════

DEFINITION
  A dynamic OpenStruct where you store EVERYTHING that must persist...

MENTAL MODEL
  The Memory Card. If it's not in .state, it is forgotten...

═══════════════════════════════════════════════════════════════
ESPAÑOL
═══════════════════════════════════════════════════════════════

DEFINICIÓN
  Un OpenStruct dinámico donde guardas TODO lo que debe persistir...

MODELO MENTAL
  La Tarjeta de Memoria. Si no está en .state, se olvida...
```

---

¡Ahora puedes leer en el idioma que prefieras! 🇬🇧🇪🇸
