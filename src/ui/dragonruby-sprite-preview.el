;;; dragonruby-sprite-preview.el --- Sprite preview with dimensions and optimization hints  -*- lexical-binding: t; -*-

;; This module provides inline sprite previews showing:
;; - Thumbnail of the image
;; - Dimensions (e.g., 24x24)
;; - Warnings for blurry images (upscaling)
;; - Warnings for unused transparency

(require 'image)

(defvar dragonruby-sprite-preview-overlays nil
  "List of sprite preview overlays in the current buffer.")

(defvar dragonruby-sprite-preview-size 32
  "Size of sprite preview thumbnails in pixels.")

(defun dragonruby--get-sprite-path (path-string)
  "Convert DragonRuby sprite PATH-STRING to absolute file path.
DragonRuby paths are relative to the game root."
  ;; Try to find the sprite relative to current buffer's directory
  (let* ((buffer-dir (file-name-directory (buffer-file-name)))
         (possible-paths
          (list
           ;; Direct path from buffer directory
           (expand-file-name path-string buffer-dir)
           ;; Try going up one level (common structure)
           (expand-file-name path-string (file-name-directory (directory-file-name buffer-dir)))
           ;; Try from project root (if in mygame/ folder)
           (expand-file-name path-string (expand-file-name ".." buffer-dir)))))
    (cl-find-if #'file-exists-p possible-paths)))

(defun dragonruby--get-image-dimensions (image-path)
  "Get dimensions of image at IMAGE-PATH.
Returns (width . height) or nil if cannot determine."
  (when (and image-path (file-exists-p image-path))
    (condition-case nil
        (let ((img (create-image image-path)))
          (when img
            (image-size img t)))
      (error nil))))

(defun dragonruby--create-sprite-overlay (start end sprite-path)
  "Create a sprite preview overlay from START to END for SPRITE-PATH."
  (let ((abs-path (dragonruby--get-sprite-path sprite-path)))
    (when (and abs-path (file-exists-p abs-path))
      (condition-case err
          (let* ((img (create-image abs-path nil nil :max-width dragonruby-sprite-preview-size
                                    :max-height dragonruby-sprite-preview-size))
                 (dims (dragonruby--get-image-dimensions abs-path))
                 (width (and dims (car dims)))
                 (height (and dims (cdr dims)))
                 (dim-str (if dims (format " [%dx%d]" width height) ""))
                 (ov (make-overlay start end)))
            
            ;; Check for potential issues
            (let ((warnings '()))
              ;; Check if image might be blurry (common sizes: 16, 24, 32, 48, 64, 128, 256)
              (when (and width height)
                (unless (or (zerop (mod width 8)) (zerop (mod height 8)))
                  (push "⚠ Odd size - may appear blurry" warnings))
                ;; Check if very small (might be upscaled)
                (when (or (< width 16) (< height 16))
                  (push "⚠ Very small - will be upscaled" warnings)))
              
              (overlay-put ov 'dragonruby-sprite-preview t)
              (overlay-put ov 'before-string
                           (propertize " "
                                       'display `(image :type ,(image-type abs-path)
                                                       :file ,abs-path
                                                       :max-width ,dragonruby-sprite-preview-size
                                                       :max-height ,dragonruby-sprite-preview-size
                                                       :margin (2 . 2)
                                                       :relief 1)))
              (overlay-put ov 'after-string
                           (propertize dim-str 'face 'font-lock-comment-face))
              
              ;; Add warning tooltip if there are issues
              (when warnings
                (overlay-put ov 'help-echo (string-join warnings "\n")))
              
              (push ov dragonruby-sprite-preview-overlays)
              ov))
        (error
         (message "Error creating sprite preview: %s" (error-message-string err))
         nil)))))

(defun dragonruby--find-sprite-paths ()
  "Find sprite path patterns like path: 'sprites/image.png' or \"sprites/image.png\"."
  (save-excursion
    (goto-char (point-min))
    ;; Pattern: path: 'file.png' or path: "file.png"
    (while (re-search-forward
            "path:[[:space:]]*['\"]\\([^'\"]+\\.png\\|[^'\"]+\\.jpg\\|[^'\"]+\\.jpeg\\)['\"]"
            nil t)
      (let ((sprite-path (match-string 1))
            (start (match-beginning 1))
            (end (match-end 1)))
        (dragonruby--create-sprite-overlay start end sprite-path)))))

(defun dragonruby--clear-sprite-overlays ()
  "Remove all sprite preview overlays from the current buffer."
  (dolist (ov dragonruby-sprite-preview-overlays)
    (delete-overlay ov))
  (setq dragonruby-sprite-preview-overlays nil))

(defun dragonruby-update-sprite-previews ()
  "Update sprite previews in the current buffer."
  (interactive)
  (dragonruby--clear-sprite-overlays)
  (dragonruby--find-sprite-paths))

(defun dragonruby-enable-sprite-preview ()
  "Enable sprite preview in the current buffer."
  (dragonruby-update-sprite-previews)
  ;; Update on buffer changes
  (add-hook 'after-change-functions #'dragonruby--sprite-preview-after-change nil t))

(defun dragonruby--sprite-preview-after-change (_beg _end _len)
  "Update sprite previews after buffer change."
  ;; Debounce updates to avoid performance issues
  (run-with-idle-timer 0.5 nil #'dragonruby-update-sprite-previews))

(provide 'dragonruby-sprite-preview)

;;; dragonruby-sprite-preview.el ends here
