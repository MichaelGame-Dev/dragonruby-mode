def tick args
  # ------------------------------------------------------------------
  # FASE 1: COLORS (EDITABLE)
  # ------------------------------------------------------------------
  # Click these arrays to edit them inline.
  # The background should match the color values.
  
  args.outputs.solids << {
    x: 100, y: 100, w: 100, h: 100,
    r: 255, g: 0, b: 0,            # [255, 0, 0]   <- RED (Decimal)
    color: [0, 128, 0],            # [0, 128, 0]   <- GREEN (Decimal)
    hex: [0xFF, 0x00, 0xFF],       # [0xFF, 0, 255] <- PURPLE (Hex)
    alpha: [0, 0, 255, 128]        # [0, 0, 255, 128] <- BLUE (Semi-transparent)
  }

  # ------------------------------------------------------------------
  # FASE 2: SPRITES (PREVIEW + EXISTENCE)
  # ------------------------------------------------------------------
  # Hover to see preview. Click to open file.
  
  # 1. Valid Sprite (Should confirm existence + Preview)
  args.outputs.sprites << {
    x: 300, y: 100, w: 100, h: 100,
    path: "sprites/player.png" 
  }

  # 2. Missing Sprite (Should underline in RED)
  args.outputs.sprites << {
    x: 400, y: 100,
    path: "sprites/missing_file.png"
  }

  # 3. Bad Format (Should underline in ORANGE)
  args.outputs.sprites << {
    x: 500, y: 100,
    path: "sprites/animation.gif" 
  }

  # ------------------------------------------------------------------
  # FASE 3: PATH NAVIGATION
  # ------------------------------------------------------------------
  # Click to jump to file.
  
  require "app/game_logic.rb"
  require "app/utils/math.rb"
  
  # Context Autocomplete Test site:
  # Type: "sprites/" below...
  # path: "
  
end
