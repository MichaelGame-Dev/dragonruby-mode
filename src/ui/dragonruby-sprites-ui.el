;;; dragonruby-sprites-ui.el --- Visual previews for sprites -*- lexical-binding: t; -*-

(require 'dragonruby-project)
(require 'dragonruby-config)

(defconst dragonruby-supported-sprite-formats '("png" "bmp" "jpg" "jpeg"))
(defconst dragonruby-unsupported-sprite-formats '("gif" "webp" "svg" "psd" "tiff"))

;; Updated Regex: Supports "path" and 'path'
(defconst dragonruby--sprite-regex "[\"']\\([^\"]+\\.\\([a-zA-Z0-9]+\\)\\)[\"']")

(defun dragonruby--sprite-hover-image (path)
  "Return image spec for tooltip."
  (when (and path (file-exists-p path))
    (create-image path nil nil :max-height 128)))

(defun dragonruby--make-sprite-overlay (start end path full-path)
  "Create overlay with interactive sprite preview."
  (let ((ov (make-overlay start end)))
    (overlay-put ov 'dragonruby-sprite-overlay t)
    
    ;; 1. Visual Hint (It's clickable!)
    (overlay-put ov 'face '(:underline t)) 
    (overlay-put ov 'mouse-face 'highlight)
    
    ;; 2. Hover: Show Image
    (overlay-put ov 'help-echo
                 (lambda (_win _obj _pos)
                   (dragonruby--sprite-hover-image full-path)))
    
    ;; 3. Click: Open File
    (overlay-put ov 'keymap
                 (let ((map (make-sparse-keymap)))
                   (define-key map [mouse-1]
                     (lambda () (interactive) (find-file full-path)))
                   map))
    ov))

(defun dragonruby--make-sprite-warning (start end msg color)
  "Create warning overlay for bad sprites."
  (let ((ov (make-overlay start end)))
    (overlay-put ov 'dragonruby-sprite-overlay t)
    (overlay-put ov 'face `(:underline (:color ,color :style wave)))
    (overlay-put ov 'help-echo msg)
    (overlay-put ov 'mouse-face 'highlight)
    ov))

(defun dragonruby--scan-sprites-region (start end)
  "Scan region for sprite strings."
  (when dragonruby-enable-sprite-preview
    (save-excursion
      (goto-char start)
      (while (re-search-forward dragonruby--sprite-regex end t)
        (let* ((s (match-beginning 1))
               (e (match-end 1))
               (path (match-string 1))
               (ext (downcase (match-string 2)))
               (real-path (dragonruby--resolve-asset path)))
          
          (cond
           ((member ext dragonruby-supported-sprite-formats)
            (if real-path
                (dragonruby--make-sprite-overlay s e path real-path)
              (dragonruby--make-sprite-warning s e (format "Path not found: %s" path) "red")))
           
           ((member ext dragonruby-unsupported-sprite-formats)
            (dragonruby--make-sprite-warning s e (format "Format .%s not supported" ext) "orange"))))))))

(defun dragonruby--clear-sprite-overlays (start end)
  (remove-overlays start end 'dragonruby-sprite-overlay t))

(provide 'dragonruby-sprites-ui)
