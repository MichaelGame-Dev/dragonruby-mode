# DragonRuby Mode for Emacs

This project follows a strict [Contract](docs/CONTRACT.md).

## Philosophy
"The goal is not speed of typing, but clarity of thought."

## Installation
See [Instructions](docs/INSTALL_INSTRUCTIONS.md) for setup details.

## Usage
The mode activates **automatically** when you open a Ruby file containing `def tick`. You will see `DR` in the modeline.

## Commands

| Command | Keybinding | Description |
|---------|------------|-------------|
| **`dragonruby-mode`** | - | **Toggle ON/OFF.** Manually activate or deactivate the mode in the current buffer. |
| **`dragonruby-inspect-concept-at-point`** | **`C-c C-d`** | **Inspect Concept.** View complete concept definition with all fields (mental-model, problems, limits, relations). |
| **`dragonruby-inspect-concept`** | - | **Inspect by Name.** Choose and inspect any registered concept. |
| **`load-file`** | - | **Reload Plugin.** Forces Emacs to reload the plugin code (useful for testing dev versions). |

## Features
- **Contextual explanations** of DragonRuby concepts via eldoc
- **Rich mental models** shown automatically while coding
- **Interactive concept inspector** for deep understanding (`C-c C-d`)
- **Live color preview** - Visual swatches for `[r, g, b, a, x]` arrays (click to edit)
- **Sprite preview** - Hover over image paths to see thumbnails, click to open
- **Auto-completion** - Type `/` in string to trigger path suggestions
- **Navigable requires** - Click `require "path"` to jump to file
- **10+ rich concepts** covering core DragonRuby APIs:
  - `tick` - The 60fps game loop
  - `args`, `args.state`, `args.outputs`, `args.inputs`
  - `args.outputs.sprites`, `args.outputs.labels`, `args.outputs.solids`
  - `args.inputs.keyboard`, `args.inputs.mouse`

## Configuration

Customize the mode via `M-x customize-group RET dragonruby RET`:

| Variable | Default | Description |
|----------|---------|-------------|
| `dragonruby-enable-color-preview` | `t` | Show colored rectangles for RGB arrays |
| `dragonruby-enable-sprite-preview` | `t` | Underline and preview image paths |
| `dragonruby-enable-require-linking` | `t` | Make `require` statements clickable |
| `dragonruby-enable-auto-completion` | `t` | Auto-trigger completion when typing `/` in strings |
| `dragonruby-max-overlays-per-type` | `50` | Max overlays per type (prevents visual overload) |

**Note on Auto-completion:** When enabled, typing `/` inside a string (e.g., `"sprites/`) will automatically suggest files from your project. This respects the **Cognitive Time Principle** (§7) by reducing micro-friction without breaking flow.


## Learning More
- See [**Concept Guide**](docs/CONCEPT_GUIDE.md) to understand the concept system
- Read the [**Contract**](docs/CONTRACT.md) to understand the project philosophy

## Contributing
Please read [contributing](docs/CONTRIBUTING.md) for architectural rules and guidelines.
