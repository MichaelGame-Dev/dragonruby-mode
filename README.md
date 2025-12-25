# DragonRuby Mode for Emacs

**Semantic Extension for DragonRuby.**

> "The goal is not speed of typing, but clarity of thought."

This mode enhances Emacs with semantic understanding of DragonRuby projects, focusing on visual clarity and navigation without adding UI clutter.

## Core Features

### 1. 🎨 Semantic Color Interaction
- **Detection**: Automatically highlights `[r, g, b]` arrays and `r: 255, g: 0...` hashes with their real color.
- **Support**: Works with Decimal (`0-255`) and Hexadecimal (`0xFF`).
- **Interaction**: **Click** on any color text to edit its values inline (no popups).

### 2. 👾 Asset Intelligence
- **Sprite Preview**: Hover over `"sprites/player.png"` to see the actual image thumbnail and dimensions.
- **Validation**:
  - **Cyan Underline**: File exists and is valid.
  - **Red Underline**: File missing (visual warning).
  - **Orange Underline**: Unsupported format (e.g., `.gif`).
- **Navigation**: Click on any asset path to open the file.

### 3. 🔗 Project Navigation
- **Clickable Requires**: `require "app/logic.rb"` becomes a clickable link.
- **Contextual Autocomplete**: Smart file completion for sprites, audio, and ruby files when typing strings.

## Installation

1. Clone this repository:
   ```bash
   git clone https://github.com/Macgyber/dragonruby-mode.git ~/.emacs.d/dragonruby-mode
   ```
2. Add to your `init.el`:
   ```elisp
   (add-to-list 'load-path "~/.emacs.d/dragonruby-mode/src")
   (require 'dragonruby)
   ```

## Usage

The mode activates **automatically** when you open a Ruby file containing `def tick`. You will see `DR` in the modeline.

## Configuration

Customize via `M-x customize-group RET dragonruby RET`.

| Variable | Default | Description |
|----------|---------|-------------|
| `dragonruby-enable-color-preview` | `t` | Show real color overlays. |
| `dragonruby-enable-sprite-preview` | `t` | Show image thumbnails on hover. |
| `dragonruby-enable-require-linking` | `t` | Make `require` paths clickable. |
| `dragonruby-max-overlays-per-type` | `50` | Performance limit for large files. |

## Architecture

See [docs/ARCHITECTURE.md](docs/ARCHITECTURE.md) for technical details on how the semantic engine works.

## Development

To verify that your changes compile correctly, you can use the included `Makefile`:

```bash
make compile
```

This will byte-compile all source files, identifying syntax errors or missing requirements immediately.
