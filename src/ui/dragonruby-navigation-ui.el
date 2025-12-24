;;; dragonruby-navigation-ui.el --- Clickable requires and completion -*- lexical-binding: t; -*-

(require 'dragonruby-project)

;; --- REQUIRE LINKS ---

(defconst dragonruby--require-regex "require[[:space:]]+\"\\([^\"]+\\)\"")

(defun dragonruby--make-require-overlay (start end path)
  "Make require statements clickable."
  (let* ((file (dragonruby--resolve-require path))
         (exists (and file (file-exists-p file)))
         (ov (make-overlay start end)))
    (overlay-put ov 'dragonruby-require-overlay t)
    (overlay-put ov 'face (if exists '(:underline t) '(:underline (:color "red" :style wave))))
    (overlay-put ov 'help-echo (if exists file "Required file not found"))
    (overlay-put ov 'keymap
                 (let ((map (make-sparse-keymap)))
                   (define-key map [mouse-1]
                     (lambda () (interactive)
                       (if exists (find-file file) (message "Missing file: %s" path))))
                   map))
    ov))

(defun dragonruby--scan-requires-region (start end)
  (save-excursion
    (goto-char start)
    (while (re-search-forward dragonruby--require-regex end t)
      (let ((s (match-beginning 1))
            (e (match-end 1))
            (path (match-string 1)))
        (dragonruby--make-require-overlay s e path)))))

(defun dragonruby--clear-require-overlays (start end)
  (remove-overlays start end 'dragonruby-require-overlay t))

;; --- AUTOCOMPLETION ---

(defun dragonruby--completion-at-point ()
  "Smart completion for DragonRuby assets and code."
  (let* ((root (dragonruby--project-root))
         (bounds (bounds-of-thing-at-point 'filename)))
    (when (and root bounds)
      (let* ((start (car bounds))
             (end   (cdr bounds))
             (candidates
              (append
               (dragonruby--files-in (expand-file-name "sprites" root) dragonruby-image-exts)
               (dragonruby--files-in (expand-file-name "audio" root) dragonruby-audio-exts)
               (dragonruby--files-in (expand-file-name "app" root) dragonruby-code-exts))))
        (list start end
              (mapcar (lambda (f) (file-relative-name f root)) candidates)
              :exclusive 'no)))))

(provide 'dragonruby-navigation-ui)
