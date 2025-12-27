# DragonRuby Mode Architecture

## 🏛️ Core Principles

1.  **Semantic Overlays**: We prioritize overlays over font-lock. Information is painted on top of the buffer (colors, images, links) without altering the text.
2.  **Zero-UI**: Information appears *in-place*. No sidebars.
    *   **Colors**: Inline Backgrounds.
    *   **Sprites**: Inline Tiny Thumbnails + Hover Tooltips.
    *   **Paths**: Clickable Hyperlinks.
3.  **Project-Aware**: All lookups (`app/main.rb`) are resolved relative to the DragonRuby project root.

## 🧩 Module Breakdown

### 1. `src/dragonruby-colors.el` (The Painter)
*   **Scanning**: Uses a Hybrid approach.
    *   **One-Liners**: Paints full block.
    *   **Multiline**: Paints fragments to respect indentation.
*   **Data Source**: Reads palettes from `src/data/palettes.json` via a recursive root lookup.

### 2. `src/dragonruby-sprites.el` (The Asset Manager)
*   **Dual Visualization**:
    1.  **Inline**: Inserts a tiny (20px) thumbnail using the `after-string` overlay property.
    2.  **Hover**: Injects a large (300px) image into the `help-echo` property.
*   **Autocomplete (CAPF)**: Implements `completion-at-point` to recursively scan `.png/.jpg` files in the project and offer them as candidates.
*   **Validation**: 
    *   Cyan = Valid.
    *   Red = Missing.
    *   Orange = Unsupported format.

### 3. `src/dragonruby-paths.el` (The Navigator)
*   **Universal Linker**: Scans for:
    *   Ruby `require` (adds `.rb` implicitly).
    *   Data files (`.json`, `.csv`, `.txt`, `.xml`).
*   **Interaction**: Creates clickable hyperlinks (`keymap` on overlay) that open the file.

### 4. `src/dragonruby-core.el` (Project Utilities)
*   Responsible for finding the project root (dominating file `app/main.rb` or `.dragonruby/`).

## 🔄 Execution Flow

1.  **User types** -> `after-change-functions` hook fires.
2.  **Scanners run** (`scan-colors`, `scan-sprites`, `scan-paths`).
3.  **Resolution**: Paths are resolved to absolute system paths.
4.  **Painting**: Overlays are created/updated.
5.  **Interaction**: User clicks an overlay -> `find-file` or Color Picker (future).

## 🎨 Extensibility
*   **Colors**: Add to `src/data/palettes.json`.
*   **Sprites**: Add extensions to `dragonruby-supported-sprites`.

