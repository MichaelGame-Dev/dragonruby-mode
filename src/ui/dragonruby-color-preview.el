;;; dragonruby-color-preview.el --- Color preview for RGB and hex values  -*- lexical-binding: t; -*-

;; This module provides inline color previews for RGB (0-255) and hexadecimal color values
;; in DragonRuby code, similar to VS Code color decorators.

(require 'color)

(defface dragonruby-color-preview-face
  '((t :box (:line-width 1 :color "gray50")))
  "Face for color preview boxes."
  :group 'dragonruby)

(defvar dragonruby-color-preview-overlays nil
  "List of color preview overlays in the current buffer.")

(defun dragonruby--rgb-to-hex (r g b)
  "Convert RGB values (0-255) to hex color string."
  (format "#%02x%02x%02x" r g b))

(defun dragonruby--normalize-rgb (r g b &optional a)
  "Normalize RGB values from 0-255 to 0.0-1.0 for Emacs color system.
Optional A (alpha) is ignored for now."
  (list (/ r 255.0) (/ g 255.0) (/ b 255.0)))

(defun dragonruby--create-color-overlay (start end color-hex)
  "Create a color preview overlay from START to END with COLOR-HEX.
Shows a clickable color checkbox that opens a color picker."
  (let ((ov (make-overlay start end)))
    (overlay-put ov 'dragonruby-color-preview t)
    (overlay-put ov 'dragonruby-color-start start)
    (overlay-put ov 'dragonruby-color-end end)
    ;; Cuadro de color clickeable (sin fondo amarillo)
    (overlay-put ov 'before-string
                 (propertize "■"
                             'face `(:foreground ,color-hex 
                                     :background ,color-hex
                                     :box (:line-width 1 :color "gray50"))
                             'mouse-face 'highlight
                             'help-echo "Click para abrir paleta de colores"
                             'keymap (let ((map (make-sparse-keymap)))
                                       (define-key map [mouse-1] 
                                         `(lambda () (interactive)
                                            (dragonruby--pick-color ,start ,end)))
                                       (define-key map (kbd "RET")
                                         `(lambda () (interactive)
                                            (dragonruby--pick-color ,start ,end)))
                                       map)
                             'pointer 'hand))
    (push ov dragonruby-color-preview-overlays)
    ov))

(defun dragonruby--pick-color (start end)
  "Open color picker and replace color at START to END with selected color."
  (let* ((current-text (buffer-substring-no-properties start end))
         ;; Usar read-color de Emacs para seleccionar color
         (new-color (read-color "Selecciona color: " t))
         ;; Convertir de hex a RGB (0-255)
         (rgb (color-name-to-rgb new-color))
         (r (round (* (nth 0 rgb) 255)))
         (g (round (* (nth 1 rgb) 255)))
         (b (round (* (nth 2 rgb) 255)))
         (new-text (format "%d, %d, %d" r g b)))
    ;; Reemplazar el texto con los nuevos valores
    (save-excursion
      (goto-char start)
      (delete-region start end)
      (insert new-text))
    ;; Actualizar previews
    (dragonruby-update-color-previews)
    (message "Color cambiado a: R=%d G=%d B=%d (%s)" r g b new-color)))

;; ---------------------------------------------------------------------------
;; Color Picker con Teclas (kurecolor-style)
;; ---------------------------------------------------------------------------

(defvar dragonruby-color-adjust-step 10
  "Cantidad a ajustar por cada presión de tecla (0-255).")

(defun dragonruby--get-color-at-point ()
  "Get RGB values at point. Returns (start end r g b) or nil."
  (save-excursion
    (let ((line-start (line-beginning-position))
          (line-end (line-end-position)))
      (goto-char line-start)
      (when (re-search-forward
             "\\b\\([0-9]\\{1,3\\}\\)[[:space:]]*,[[:space:]]*\\([0-9]\\{1,3\\}\\)[[:space:]]*,[[:space:]]*\\([0-9]\\{1,3\\}\\)"
             line-end t)
        (let ((r (string-to-number (match-string 1)))
              (g (string-to-number (match-string 2)))
              (b (string-to-number (match-string 3))))
          (when (and (<= 0 r 255) (<= 0 g 255) (<= 0 b 255))
            (list (match-beginning 1) (match-end 3) r g b)))))))

(defun dragonruby--update-color-at-point (r g b)
  "Update the RGB color at point with new R G B values."
  (let ((color-data (dragonruby--get-color-at-point)))
    (when color-data
      (let ((start (nth 0 color-data))
            (end (nth 1 color-data)))
        (save-excursion
          (goto-char start)
          (delete-region start end)
          (insert (format "%d, %d, %d" 
                          (max 0 (min 255 r))
                          (max 0 (min 255 g))
                          (max 0 (min 255 b)))))
        (dragonruby-update-color-previews)
        (message "Color: R=%d G=%d B=%d | Teclas: R/r G/g B/b H/h S/s L/l | q=salir" 
                 (max 0 (min 255 r))
                 (max 0 (min 255 g))
                 (max 0 (min 255 b)))))))

(defun dragonruby-color-increase-red ()
  "Increase red component."
  (interactive)
  (let ((data (dragonruby--get-color-at-point)))
    (when data
      (dragonruby--update-color-at-point 
       (+ (nth 2 data) dragonruby-color-adjust-step)
       (nth 3 data) (nth 4 data)))))

(defun dragonruby-color-decrease-red ()
  "Decrease red component."
  (interactive)
  (let ((data (dragonruby--get-color-at-point)))
    (when data
      (dragonruby--update-color-at-point 
       (- (nth 2 data) dragonruby-color-adjust-step)
       (nth 3 data) (nth 4 data)))))

(defun dragonruby-color-increase-green ()
  "Increase green component."
  (interactive)
  (let ((data (dragonruby--get-color-at-point)))
    (when data
      (dragonruby--update-color-at-point 
       (nth 2 data)
       (+ (nth 3 data) dragonruby-color-adjust-step)
       (nth 4 data)))))

(defun dragonruby-color-decrease-green ()
  "Decrease green component."
  (interactive)
  (let ((data (dragonruby--get-color-at-point)))
    (when data
      (dragonruby--update-color-at-point 
       (nth 2 data)
       (- (nth 3 data) dragonruby-color-adjust-step)
       (nth 4 data)))))

(defun dragonruby-color-increase-blue ()
  "Increase blue component."
  (interactive)
  (let ((data (dragonruby--get-color-at-point)))
    (when data
      (dragonruby--update-color-at-point 
       (nth 2 data) (nth 3 data)
       (+ (nth 4 data) dragonruby-color-adjust-step)))))

(defun dragonruby-color-decrease-blue ()
  "Decrease blue component."
  (interactive)
  (let ((data (dragonruby--get-color-at-point)))
    (when data
      (dragonruby--update-color-at-point 
       (nth 2 data) (nth 3 data)
       (- (nth 4 data) dragonruby-color-adjust-step)))))

(defvar dragonruby-color-adjust-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "R") 'dragonruby-color-increase-red)
    (define-key map (kbd "r") 'dragonruby-color-decrease-red)
    (define-key map (kbd "G") 'dragonruby-color-increase-green)
    (define-key map (kbd "g") 'dragonruby-color-decrease-green)
    (define-key map (kbd "B") 'dragonruby-color-increase-blue)
    (define-key map (kbd "b") 'dragonruby-color-decrease-blue)
    (define-key map (kbd "q") 'dragonruby-color-adjust-quit)
    (define-key map (kbd "RET") 'dragonruby-color-adjust-quit)
    (define-key map (kbd "<escape>") 'dragonruby-color-adjust-quit)
    map)
  "Keymap for color adjustment mode.")

(define-minor-mode dragonruby-color-adjust-mode
  "Minor mode for adjusting colors with keyboard."
  :lighter " ColorAdj"
  :keymap dragonruby-color-adjust-mode-map
  (if dragonruby-color-adjust-mode
      (message "Modo ajuste de color: R/r=Rojo G/g=Verde B/b=Azul | q=salir")
    (message "Saliendo del modo ajuste de color")))

(defun dragonruby-color-adjust-quit ()
  "Quit color adjustment mode."
  (interactive)
  (dragonruby-color-adjust-mode -1))

(defun dragonruby-adjust-color-at-point ()
  "Enter color adjustment mode for the color at point."
  (interactive)
  (if (dragonruby--get-color-at-point)
      (progn
        (dragonruby-color-adjust-mode 1)
        (message "Ajusta color: R/r=Rojo G/g=Verde B/b=Azul | q=salir"))
    (message "No hay color RGB en esta línea")))

(defun dragonruby--find-rgb-colors ()
  "Find RGB color patterns like (255, 0, 0) or [255, 0, 0, 255]."
  (save-excursion
    (goto-char (point-min))
    ;; Pattern: 3 or 4 numbers (0-255) separated by commas
    ;; Matches: 255, 0, 0 or 255, 0, 0, 255
    (while (re-search-forward
            "\\b\\([0-9]\\{1,3\\}\\)[[:space:]]*,[[:space:]]*\\([0-9]\\{1,3\\}\\)[[:space:]]*,[[:space:]]*\\([0-9]\\{1,3\\}\\)\\(?:[[:space:]]*,[[:space:]]*\\([0-9]\\{1,3\\}\\)\\)?"
            nil t)
      (let* ((r (string-to-number (match-string 1)))
             (g (string-to-number (match-string 2)))
             (b (string-to-number (match-string 3)))
             (a (when (match-string 4) (string-to-number (match-string 4)))))
        ;; Validate RGB values are in range 0-255
        (when (and (<= 0 r 255) (<= 0 g 255) (<= 0 b 255)
                   (or (null a) (<= 0 a 255)))
          (let ((color-hex (dragonruby--rgb-to-hex r g b))
                (start (match-beginning 1))
                (end (if a (match-end 4) (match-end 3))))
            (dragonruby--create-color-overlay start end color-hex)))))))

(defun dragonruby--find-hex-colors ()
  "Find hexadecimal color patterns like #FF0000 or #ff0000."
  (save-excursion
    (goto-char (point-min))
    ;; Pattern: #RRGGBB or #RGB
    (while (re-search-forward
            "#\\([0-9a-fA-F]\\{6\\}\\|[0-9a-fA-F]\\{3\\}\\)\\b"
            nil t)
      (let* ((hex-value (match-string 1))
             (color-hex (concat "#" hex-value))
             (start (match-beginning 0))
             (end (match-end 0)))
        ;; Expand #RGB to #RRGGBB if needed
        (when (= (length hex-value) 3)
          (setq color-hex
                (format "#%c%c%c%c%c%c"
                        (aref hex-value 0) (aref hex-value 0)
                        (aref hex-value 1) (aref hex-value 1)
                        (aref hex-value 2) (aref hex-value 2))))
        (dragonruby--create-color-overlay start end color-hex)))))

(defun dragonruby--clear-color-overlays ()
  "Remove all color preview overlays from the current buffer."
  (dolist (ov dragonruby-color-preview-overlays)
    (delete-overlay ov))
  (setq dragonruby-color-preview-overlays nil))

(defun dragonruby-update-color-previews ()
  "Update color previews in the current buffer."
  (interactive)
  (dragonruby--clear-color-overlays)
  (dragonruby--find-rgb-colors)
  (dragonruby--find-hex-colors))

(defun dragonruby-enable-color-preview ()
  "Enable color preview in the current buffer."
  (dragonruby-update-color-previews)
  ;; Update on buffer changes
  (add-hook 'after-change-functions #'dragonruby--color-preview-after-change nil t))

(defun dragonruby--color-preview-after-change (_beg _end _len)
  "Update color previews after buffer change."
  ;; Debounce updates to avoid performance issues
  (run-with-idle-timer 0.5 nil #'dragonruby-update-color-previews))

(provide 'dragonruby-color-preview)

;;; dragonruby-color-preview.el ends here
