;;; dragonruby-sprites.el --- Sprite previews with Toolbar -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'image-file)
(require 'image-mode)

(defvar dragonruby--sprite-overlays nil)
(defvar dragonruby-supported-sprites '("png" "jpg" "jpeg" "gif" "bmp"))
(defvar dragonruby-unsupported-sprites '("svg" "psd" "xcf" "ase" "aseprite"))

;; --- PATH RESOLUTION ---
(defun dragonruby--find-project-root ()
  (let ((dir (file-name-directory (or buffer-file-name default-directory))))
    (or (locate-dominating-file dir "app/main.rb")
        (locate-dominating-file dir "dragonruby")
        (locate-dominating-file dir ".dragonruby/")
        dir)))

(defun dragonruby--resolve-asset-path (path)
  (let ((root (dragonruby--find-project-root)))
    (expand-file-name path root)))

;; --- AUTOCOMPLETE (CAPF) ---
(defun dragonruby--get-all-sprites-in-project ()
  (let* ((root (dragonruby--find-project-root))
         (files (directory-files-recursively root "\\.\\(png\\|jpg\\|jpeg\\|gif\\|bmp\\|PNG\\|JPG\\)$")))
    (mapcar (lambda (f) (file-relative-name f root)) files)))

(defun dragonruby-sprite-completion-at-point ()
  (let ((state (syntax-ppss)))
    (when (nth 3 state)
      (save-excursion
        (let* ((start (nth 8 state))
               (end (point))
               (str-content (buffer-substring-no-properties (1+ start) end))
               (len (length str-content))) 
          (when (> len 0)
             (list (1+ start) (nth 1 (syntax-ppss))
                   (completion-table-dynamic (lambda (_) (dragonruby--get-all-sprites-in-project)))
                   :exclusive 'no)))))))

;; --- IMAGE TOOLBAR UI ---
(defun dragonruby-image-zoom-in () (interactive) (image-increase-size 1.2))
(defun dragonruby-image-zoom-out () (interactive) (image-decrease-size 1.2))
(defun dragonruby-image-rotate () (interactive) (image-rotate))
(defun dragonruby-image-optimize () 
  (interactive) 
  (message "✨ Optimization (Trim/Compress) coming in Phase 2.5! (Requires ImageMagick)"))

(defun dragonruby--insert-image-toolbar ()
  "Insert visual buttons at the top of the image buffer."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (unless (looking-at "DR-TOOLBAR") ;; Prevent double insertion
        (insert "\n")
        (insert-button "[ 🔍+ ]" 'action (lambda (_) (dragonruby-image-zoom-in)) 'help-echo "Zoom In")
        (insert " ")
        (insert-button "[ 🔍- ]" 'action (lambda (_) (dragonruby-image-zoom-out)) 'help-echo "Zoom Out")
        (insert " ")
        (insert-button "[ ↩️ Rotar ]" 'action (lambda (_) (dragonruby-image-rotate)) 'help-echo "Rotate 90°")
        (insert "   ")
        (insert-button "[ ✨ Editar/Optimizar ]" 
                       'action (lambda (_) (dragonruby-image-optimize)) 
                       'face '(:background "#4CAF50" :foreground "white" :box (:line-width 2 :color "#4CAF50"))
                       'help-echo "Click to Trim/Compress (Future Feature)")
        (insert "\n\nDR-TOOLBAR: Image Preview Mode\n")
        (add-text-properties (point-min) (point) '(read-only t rear-nonsticky t))))))

(defun dragonruby--image-mode-hook ()
  "Activate toolbar if inside a DragonRuby project."
  (when (and buffer-file-name (locate-dominating-file buffer-file-name "app/main.rb"))
    (dragonruby--insert-image-toolbar)))

;; --- VISUALS ---
(defun dragonruby--create-tooltip-string (path)
  (if (file-exists-p path)
      (let* ((image (create-image path nil nil :max-width 300 :max-height 300))
             (attrs (file-attributes path))
             (size (file-size-human-readable (file-attribute-size attrs)))
             (info-text (format " \n📦 %s\n📏 %s" (upcase (file-name-extension path)) size)))
        (concat (propertize " " 'display image) info-text))
    "❌ Text file not found"))

(defun dragonruby--create-inline-thumb (path)
  (when (file-exists-p path)
    (let ((image (create-image path nil nil :height 20 :ascent 'center)))
      (concat (propertize " " 'display '(space :width 1))
              (propertize " " 'display image)))))

(defun dragonruby--make-sprite-overlay (start end text path status)
  (let ((ov (make-overlay start end))
        (color (pcase status ('valid "cyan") ('missing "red") ('unsupported "orange")))
        (style (if (eq status 'valid) nil 'wave))
        (tooltip (if (eq status 'valid) (dragonruby--create-tooltip-string path) "Missing/Invalid")))
    
    (overlay-put ov 'face `(:underline (:color ,color :style ,style)))
    (overlay-put ov 'help-echo tooltip)
    (when (eq status 'valid)
      (let ((thumb (dragonruby--create-inline-thumb path)))
        (when thumb (overlay-put ov 'after-string thumb)))
      (overlay-put ov 'keymap 
                   (let ((map (make-sparse-keymap)))
                     (define-key map [mouse-1] (lambda () (interactive) (find-file path)))
                     map))
      (overlay-put ov 'mouse-face 'highlight))
    (push ov dragonruby--sprite-overlays)))

(defun dragonruby--scan-sprites ()
  (mapc #'delete-overlay dragonruby--sprite-overlays)
  (setq dragonruby--sprite-overlays nil)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\"\\([^\"\n]+\\)\"" nil t)
      (let* ((start (match-beginning 1))
             (end (match-end 1))
             (raw-path (match-string 1))
             (ext (file-name-extension raw-path)))
        (when ext
          (setq ext (downcase ext))
          (cond
           ((member ext dragonruby-supported-sprites)
            (let ((abs-path (dragonruby--resolve-asset-path raw-path)))
              (if (file-exists-p abs-path)
                  (dragonruby--make-sprite-overlay start end raw-path abs-path 'valid)
                (dragonruby--make-sprite-overlay start end raw-path abs-path 'missing))))
           ((member ext dragonruby-unsupported-sprites)
            (dragonruby--make-sprite-overlay start end raw-path nil 'unsupported))))))))

(defun dragonruby--after-sprite-change (_beg _end _len)
  (dragonruby--scan-sprites))

(defun dragonruby--setup-capf ()
  (add-hook 'completion-at-point-functions #'dragonruby-sprite-completion-at-point nil t))

(define-minor-mode dragonruby-sprite-mode
  "Sprite previews."
  :lighter " DR-Sprite"
  (if dragonruby-sprite-mode
      (progn
        (add-hook 'after-change-functions #'dragonruby--after-sprite-change nil t)
        (dragonruby--setup-capf)
        (add-hook 'image-mode-hook #'dragonruby--image-mode-hook) ;; NEW HOOK
        (dragonruby--scan-sprites))
    (remove-hook 'after-change-functions #'dragonruby--after-sprite-change t)
    (remove-hook 'completion-at-point-functions #'dragonruby-sprite-completion-at-point t)
    (remove-hook 'image-mode-hook #'dragonruby--image-mode-hook)
    (mapc #'delete-overlay dragonruby--sprite-overlays)))

(provide 'dragonruby-sprites)
