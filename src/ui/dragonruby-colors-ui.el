;;; dragonruby-colors-ui.el --- Live Color Preview & Editor -*- lexical-binding: t; -*-

(require 'cl-lib)

;; 1. Helper / Converter
(defun dragonruby--rgba-to-hex (r g b)
  "Convert R G B (0-255) to #RRGGBB."
  (format "#%02x%02x%02x" r g b))

;; 2. Overlay Logic
(defun dragonruby--make-color-overlay (start end r g b)
  "Create a clickable color preview overlay."
  (let* ((hex (dragonruby--rgba-to-hex r g b))
         (ov (make-overlay start end)))
    (overlay-put ov 'face `(:background ,hex :foreground "black")) ;; Contrast text
    (overlay-put ov 'dragonruby-color-overlay t) ;; Tag for cleanup
    (overlay-put ov 'mouse-face 'highlight)
    (overlay-put ov 'help-echo (format "Color %s (Click to edit)" hex))
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
(defun dragonruby--read-rgba (r g b a x)
  "Prompt user for new values."
  (list
   (read-number "Red (0-255): " r)
   (read-number "Green (0-255): " g)
   (read-number "Blue (0-255): " b)
   (read-number "Alpha (0-255): " a)
   (read-number "Meta/Sort: " x)))

(defun dragonruby--replace-color-block (start end values)
  "Replace text in buffer with new values."
  (let ((text (format "[%d, %d, %d, %d, %d]"
                      (nth 0 values)
                      (nth 1 values)
                      (nth 2 values)
                      (nth 3 values)
                      (nth 4 values))))
    (save-excursion
      (goto-char start)
      (delete-region start end)
      (insert text))))

(defun dragonruby--on-color-click (overlay)
  "Handler for clicking a color overlay."
  (let* ((start (overlay-start overlay))
         (end   (overlay-end overlay))
         (text  (buffer-substring-no-properties start end)))
    ;; Adjusted Regex to handle commas optionally for Ruby array style
    (when (string-match
           "\\[\\s-*\\([0-9]+\\)[, ]+\\([0-9]+\\)[, ]+\\([0-9]+\\)[, ]+\\([0-9]+\\)[, ]+\\([0-9]+\\)\\s-*\\]"
           text)
      (let* ((r (string-to-number (match-string 1 text)))
             (g (string-to-number (match-string 2 text)))
             (b (string-to-number (match-string 3 text)))
             (a (string-to-number (match-string 4 text)))
             (x (string-to-number (match-string 5 text)))
             (new (dragonruby--read-rgba r g b a x)))
        (dragonruby--replace-color-block start end new)
        ;; Rescan immediate area
        (dragonruby--scan-colors-region (max (point-min) (- start 10))
                                        (min (point-max) (+ start 40)))))))

;; 4. Scanning Logic
(defun dragonruby--scan-colors-region (start end)
  "Scan and overlay colors in region."
  (save-excursion
    (goto-char start)
    ;; Regex tolerates commas or spaces: [255, 0, 0, 255, 0] or [255 0 0 255 0]
    (while (re-search-forward
            "\\[\\s-*\\([0-9]+\\)[, ]+\\([0-9]+\\)[, ]+\\([0-9]+\\)[, ]+\\([0-9]+\\)[, ]+\\([0-9]+\\)\\s-*\\]"
            end t)
      (let ((s (match-beginning 0))
            (e (match-end 0))
            (r (string-to-number (match-string 1)))
            (g (string-to-number (match-string 2)))
            (b (string-to-number (match-string 3))))
        (dragonruby--make-color-overlay s e r g b)))))

(defun dragonruby--scan-colors-entire-buffer ()
  "Scan the whole buffer."
  (dragonruby--scan-colors-region (point-min) (point-max)))

(provide 'dragonruby-colors-ui)
