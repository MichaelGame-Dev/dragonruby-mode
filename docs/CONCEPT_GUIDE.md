# DragonRuby Concept System Guide

## Philosophy

The concept system is the heart of `dragonruby-mode`. It embodies the project's core philosophy:

> **"The goal is not speed of typing, but clarity of thought."**

This guide explains how to create, understand, and use DragonRuby concepts effectively.

---

## What is a Concept?

A **concept** is not just documentation—it is a **cognitive tool** designed to:
- Make implicit engine behavior explicit
- Provide mental models for understanding
- Show what problems the concept solves
- Clarify what the concept does NOT do
- Connect related concepts into a knowledge network

---

## The Concept Structure

Every concept is defined using the `dragonruby-concept` struct with these fields:

### Core Identity

#### `id` (symbol)
The unique identifier for the concept (e.g., `"args"`, `"args.state"`).

**Rule**: Use the exact DragonRuby API name.

#### `name` (string)
Human-readable name (e.g., `"Frame Arguments"`, `"Game State"`).

**Rule**: Should be clear to a beginner.

---

### Classification

#### `level` (symbol)
Complexity level: `'core`, `'basic`, or `'advanced`.

**Purpose**: Supports career-aware design (exploration vs exploitation).

#### `scope` (symbol)
Conceptual domain: `'frame`, `'input`, `'render`, `'state`, etc.

**Purpose**: Helps users understand where this fits in the engine.

---

### Understanding Fields (CRITICAL)

These fields are what make concepts **cognitive amplifiers** instead of just documentation.

#### `definition` (string)
**One-line essential truth** about the concept.

**Rules**:
- Must be understandable without context
- Should be memorable
- Avoid jargon when possible

**Example**:
```elisp
:definition "The specific universe of data for the current 1/60th second of simulation."
```

#### `intention` (string)
**Why this concept exists**—what problem it solves.

**Rules**:
- Focus on the "why", not the "what"
- Explain the design decision

**Example**:
```elisp
:intention "To centralize inputs, state, and outputs for each execution frame."
```

#### `mental-model` (string)
**Analogy or metaphor** to help understanding.

**Rules**:
- Use familiar concepts
- Make it visual/tangible
- Can be multi-line for rich analogies

**Example**:
```elisp
:mental-model "Think of args as the Console itself:
 - Inputs: The Controller (what you press)
 - Outputs: The TV Screen (what you see)
 - State: The Memory Card (what is saved)"
```

#### `problems` (list of strings)
**Confusions or difficulties** this concept prevents.

**Purpose**: Shows what the user would struggle with WITHOUT this concept.

**Example**:
```elisp
:problems '("Global state confusion"
            "Unclear input handling"
            "Unclear rendering pipeline")
```

#### `limits` (list of strings)
**What this concept does NOT do**.

**Purpose**: Prevents misconceptions and scope creep.

**Example**:
```elisp
:limits '("Does not contain game logic"
          "Does not render by itself"
          "Does not persist data without state")
```

#### `relations` (alist)
**Connections to other concepts**.

**Format**: `((relationship-type . "concept-id") ...)`

**Common relationship types**:
- `"contains"` - has as sub-concept
- `"uses"` - depends on
- `"extends"` - builds upon
- `"alternative-to"` - different approach

**Example**:
```elisp
:relations '(("contains" . "args.inputs")
             ("contains" . "args.state")
             ("contains" . "args.outputs"))
```

**Purpose**: Creates a navigable knowledge graph.

---

#### `presentation` (alist)
**How to display this concept**.

**Format**: `((ui-type . preference) ...)`

**Options**:
- `(eldoc . t)` - Show in eldoc
- `(tooltip . optional)` - Show in tooltip
- `(snippet . nil)` - Don't generate snippets

**Example**:
```elisp
:presentation '((eldoc . t)
                (tooltip . optional)
                (snippet . nil))
```

#### `evolution` (string)
**How this concept may change over time**.

**Purpose**: Manages expectations and backward compatibility.

**Example**:
```elisp
:evolution "May gain sub-concepts, but its core definition must not change."
```

---

## How to Create a Good Concept

### Step 1: Identify the Concept

Ask yourself:
- Is this a **concept** (understanding) or a **feature** (automation)?
- Does it help the user **think more clearly** about DragonRuby?
- Can it be explained simply?

If yes to all three, proceed.

### Step 2: Write the Definition

Start with the one-line definition. If you can't write it in one line, you don't understand it yet.

**Bad**:
```elisp
:definition "args is a thing that has inputs and outputs and state and stuff"
```

**Good**:
```elisp
:definition "The specific universe of data for the current 1/60th second of simulation."
```

### Step 3: Find the Mental Model

Think of an analogy from the real world or a familiar domain.

**Technique**: Complete this sentence:
> "Think of [concept] as [familiar thing]..."

### Step 4: Define the Boundaries

What does this concept NOT do? This is often more important than what it does.

### Step 5: Map the Relations

How does this concept connect to others? Draw a mental graph.

### Step 6: Test with a Beginner

Can someone unfamiliar with DragonRuby understand this? If not, revise.

---

## Example: Complete Concept Definition

Here's the `args` concept as a reference implementation:

```elisp
(dragonruby-register-concept
 (make-dragonruby-concept
  :id "args"
  :name "Frame Arguments"
  :level 'core
  :scope 'frame
  :definition
  "The specific universe of data for the current 1/60th second of simulation."
  :intention
  "To centralize inputs, state, and outputs for each execution frame."
  :mental-model
  "Think of args as the Console itself:
   - Inputs: The Controller (what you press)
   - Outputs: The TV Screen (what you see)
   - State: The Memory Card (what is saved)"
  :problems
  '("Global state confusion"
    "Unclear input handling"
    "Unclear rendering pipeline")
  :limits
  '("Does not contain game logic"
    "Does not render by itself"
    "Does not persist data without state")
  :relations
  '(("contains" . "args.inputs")
    ("contains" . "args.state")
    ("contains" . "args.outputs"))
  :presentation
  '((eldoc . t)
    (tooltip . optional)
    (snippet . nil))
  :evolution
  "May gain sub-concepts, but its core definition must not change."))
```

---

## Using Concepts in Your Workflow

### In Eldoc (Automatic)

When your cursor is over a concept (e.g., `args`), eldoc shows:
```
The specific universe of data for the current 1/60th second of simulation. — Think of args as the Console itself...
```

### In Inspector (Interactive)

Press `M-x dragonruby-inspect-concept-at-point` to see the **complete** concept:

```
Frame Arguments (args)
[core | frame]

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

RELATIONS
  • contains → args.inputs
  • contains → args.state
  • contains → args.outputs

EVOLUTION
  May gain sub-concepts, but its core definition must not change.

═══════════════════════════════════════════════════════════════

Press 'q' to close | Click on relations to navigate
```

### Navigation

In the inspector:
- Press `n` to jump to next relation
- Press `p` to jump to previous relation
- Press `RET` on a relation to inspect it
- Press `g` to inspect a different concept
- Press `q` to close

---

## Anti-Patterns to Avoid

### ❌ Don't: Create Feature Concepts

**Bad**:
```elisp
:id "auto-complete-args"
:definition "Automatically completes args when you type"
```

This is a **feature**, not a **concept**. It doesn't help understanding.

### ❌ Don't: Use Jargon in Definitions

**Bad**:
```elisp
:definition "Polymorphic accessor for frame-scoped mutable state container"
```

**Good**:
```elisp
:definition "A dynamic OpenStruct where you store EVERYTHING that must persist between frames."
```

### ❌ Don't: Skip Mental Models

Mental models are the **most important** field. If you skip it, the concept is just documentation.

### ❌ Don't: Create Orphan Concepts

Every concept should relate to at least one other concept. Isolated concepts don't build understanding.

---

## Contract Alignment

This concept system directly implements these contract principles:

- ✅ **"Understanding over automation"** - Concepts explain, they don't automate
- ✅ **"Everything must be traceable"** - Relations create knowledge graph
- ✅ **"Everything must be inspectable"** - Inspector shows complete concepts
- ✅ **"Cognitive continuity"** - Eldoc doesn't break flow, inspector is optional
- ✅ **"Career-aware design"** - Level field supports exploration/exploitation

---

## Contributing Concepts

When adding a new concept:

1. Create `src/concepts/your-concept.el`
2. Define the concept with ALL fields filled
3. Register it with `dragonruby-register-concept`
4. Require it in `src/dragonruby.el`
5. Add tests in `tests/test-concepts.el`
6. Update this guide if you discover new patterns

---

## Questions?

If you're unsure about any aspect of creating concepts, ask yourself:

> **"Does this help the user think more clearly about DragonRuby?"**

If yes, you're on the right track.

If no, it doesn't belong in this project.

---

**End of Guide**
