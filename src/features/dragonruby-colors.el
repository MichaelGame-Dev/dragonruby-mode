;;; dragonruby-colors.el --- Live Color Preview & Editor -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'dragonruby-config)

;; 1. Helper / Converter
(defun dragonruby--rgba-to-hex (r g b)
  "Convert R G B (0-255) to #RRGGBB."
  (format "#%02x%02x%02x" r g b))

(defun dragonruby--get-contrast-color (r g b)
  "Return black or white based on background brightness for readability."
  (let ((brightness (+ (* r 0.299) (* g 0.587) (* b 0.114))))
    (if (> brightness 128) "black" "white")))

(defun dragonruby--count-color-overlays ()
  "Count current color overlays in buffer."
  (let ((count 0))
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when (overlay-get ov 'dragonruby-color-overlay)
        (setq count (1+ count))))
    count))

;; 2. Overlay Logic
(defun dragonruby--make-color-overlay (start end r g b)
  "Create a rectangular highlight overlay covering the entire color text."
  (let* ((hex (dragonruby--rgba-to-hex r g b))
         (fg (dragonruby--get-contrast-color r g b))
         (ov (make-overlay start end)))
    (overlay-put ov 'dragonruby-color-overlay t)
    
    ;; Apply the 'Rectangle' style: Full background highlight
    (overlay-put ov 'face `(:background ,hex :foreground ,fg))
    
    ;; interaction
    (overlay-put ov 'help-echo (format "Color %s (Click to edit)" hex))
    (overlay-put ov 'mouse-face '(:box (:line-width (2 . 2) :color "gold" :style nil)))
    (overlay-put
     ov 'keymap
     (let ((map (make-sparse-keymap)))
       (define-key map [mouse-1]
         (lambda ()
           (interactive)
           (dragonruby--on-color-click ov)))
       map))
    ov))

(defun dragonruby--clear-color-overlays (start end)
  "Clear color overlays in region."
  (remove-overlays start end 'dragonruby-color-overlay t))

;; 3. Interaction Logic
(defun dragonruby--read-rgba (current-values)
  "Prompt user for new values based on CURRENT-VALUES list."
  (let* ((len (length current-values))
         (r (nth 0 current-values))
         (g (nth 1 current-values))
         (b (nth 2 current-values))
         (a (and (> len 3) (nth 3 current-values)))
         (x (and (> len 4) (nth 4 current-values))))
    (delq nil
          (list
           (read-number "Red (0-255): " r)
           (read-number "Green (0-255): " g)
           (read-number "Blue (0-255): " b)
           (when a (read-number "Alpha (0-255): " a))
           (when x (read-number "Meta/Sort: " x))))))

(defun dragonruby--replace-color-block (start end values)
  "Replace text in buffer with new values formatted as a Ruby array."
  (let ((text (concat "[" (mapconcat #'number-to-string values ", ") "]")))
    (save-excursion
      (goto-char start)
      (delete-region start end)
      (insert text))))

(defun dragonruby--on-color-click (overlay)
  "Handler for clicking a color overlay."
  (let* ((start (overlay-start overlay))
         (end   (overlay-end overlay))
         (text  (buffer-substring-no-properties start end)))
    ;; Flexible Regex for 3, 4, or 5 values with Hex support
    (when (string-match "\\[\\s-*\\(\\(?:0x[0-9a-fA-F]+\\|[0-9]+\\)\\)[, ]+\\(\\(?:0x[0-9a-fA-F]+\\|[0-9]+\\)\\)[, ]+\\(\\(?:0x[0-9a-fA-F]+\\|[0-9]+\\)\\)\\(?:[, ]+\\(\\(?:0x[0-9a-fA-F]+\\|[0-9]+\\)\\)\\)?\\(?:[, ]+\\(\\(?:0x[0-9a-fA-F]+\\|[0-9]+\\)\\)\\)?\\s-*\\]" text)
      (let* ((v1 (string-to-number (match-string 1 text)))
             (v2 (string-to-number (match-string 2 text)))
             (v3 (string-to-number (match-string 3 text)))
             (v4 (match-string 4 text))
             (v5 (match-string 5 text))
             (current (delq nil (list v1 v2 v3 
                                     (when v4 (string-to-number v4))
                                     (when v5 (string-to-number v5)))))
             (new (dragonruby--read-rgba current)))
        (dragonruby--replace-color-block start end new)
        (dragonruby--scan-colors-region (max (point-min) (- start 10))
                                        (min (point-max) (+ start 40)))))))

;; 4. Scanning Logic
(defun dragonruby--scan-colors-region (start end)
  "Scan and overlay colors in region."
  (when dragonruby-enable-color-preview
    (save-excursion
      (goto-char start)
      (while (and (re-search-forward "\\[\\s-*\\(\\(?:0x[0-9a-fA-F]+\\|[0-9]+\\)\\)[, ]+\\(\\(?:0x[0-9a-fA-F]+\\|[0-9]+\\)\\)[, ]+\\(\\(?:0x[0-9a-fA-F]+\\|[0-9]+\\)\\)\\(?:[, ]+\\(\\(?:0x[0-9a-fA-F]+\\|[0-9]+\\)\\)\\)?\\(?:[, ]+\\(\\(?:0x[0-9a-fA-F]+\\|[0-9]+\\)\\)\\)?\\s-*\\]" end t)
                  (< (dragonruby--count-color-overlays) dragonruby-max-overlays-per-type))
        (let ((s (match-beginning 0))
              (e (match-end 0))
              (r (string-to-number (match-string 1)))
              (g (string-to-number (match-string 2)))
              (b (string-to-number (match-string 3))))
          (dragonruby--make-color-overlay s e r g b))))))

(defun dragonruby--scan-colors-entire-buffer ()
  "Scan the whole buffer."
  (dragonruby--scan-colors-region (point-min) (point-max)))

(defun dragonruby-adjust-color-at-point ()
  "Interactively adjust the color array under cursor."
  (interactive)
  (let ((ovs (overlays-at (point))))
    (cl-loop for ov in ovs
             when (overlay-get ov 'dragonruby-color-overlay)
             return (dragonruby--on-color-click ov)
             finally (message "No color array found at point."))))

(provide 'dragonruby-colors)
