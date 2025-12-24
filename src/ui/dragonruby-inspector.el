;;; dragonruby-inspector.el --- Bilingual Concept Inspector -*- lexical-binding: t; -*-

(require 'dragonruby-registry)
(require 'dragonruby-eldoc)

(defun dragonruby-inspect-concept (concept)
  "Display full bilingual details for CONCEPT in a separate buffer."
  (let ((buf (get-buffer-create "*DragonRuby Inspector*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "CONCEPT: %s\n" (dragonruby-concept-name concept)))
        (insert "========================================================\n\n")
        
        ;; English
        (insert "🇬🇧 ENGLISH\n")
        (insert (format "Definition: %s\n" (dragonruby-concept-definition concept)))
        (when (dragonruby-concept-mental-model concept)
          (insert (format "Mental Model: %s\n" (dragonruby-concept-mental-model concept))))
        (when (dragonruby-concept-intention concept)
          (insert (format "Intention: %s\n" (dragonruby-concept-intention concept))))
        
        (insert "\n/ ---------------------------------------------------- /\n\n")
        
        ;; Spanish
        (insert "🇪🇸 ESPAÑOL\n")
        (insert (format "Definición: %s\n" (or (dragonruby-concept-definition-es concept) "Sin traducir")))
        (when (dragonruby-concept-mental-model-es concept)
          (insert (format "Modelo Mental: %s\n" (dragonruby-concept-mental-model-es concept))))
        (when (dragonruby-concept-intention-es concept)
          (insert (format "Intención: %s\n" (dragonruby-concept-intention-es concept))))
        
        (insert "\n========================================================\n")
        (insert "Press 'q' to close this window.\n")
        (view-mode 1)
        (local-set-key (kbd "q") 'quit-window)))
    (display-buffer buf)))

(defun dragonruby-inspect-at-point ()
  "Inspect the DragonRuby concept under cursor."
  (interactive)
  (let* ((id (dragonruby--symbol-at-point))
         (concept (and id (dragonruby-get-concept id))))
    (if concept
        (dragonruby-inspect-concept concept)
      (message "No core DragonRuby concept detected at point: %s" id))))

(provide 'dragonruby-inspector)
