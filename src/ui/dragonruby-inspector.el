;;; dragonruby-inspector.el --- Interactive concept inspector  -*- lexical-binding: t; -*-

;; This module implements the "Everything must be inspectable" principle
;; from the project contract. It allows users to view complete concept
;; definitions without breaking their flow.

(require 'dragonruby-registry)

(defvar dragonruby-inspector-buffer-name "*DragonRuby Concept Inspector*"
  "Name of the buffer used for concept inspection.")

(defun dragonruby--format-concept-field (label value)
  "Format a concept field with LABEL and VALUE for display.
Returns nil if VALUE is nil or empty."
  (when (and value
             (not (and (stringp value) (string-empty-p value)))
             (not (and (listp value) (null value))))
    (concat (propertize label 'face 'bold)
            "\n"
            (if (listp value)
                (mapconcat (lambda (item)
                             (if (consp item)
                                 (format "  • %s → %s" (car item) (cdr item))
                               (format "  • %s" item)))
                           value
                           "\n")
              (format "  %s" value))
            "\n\n")))

(defun dragonruby--format-concept (concept)
  "Format CONCEPT for display in inspector buffer with bilingual support.
Shows English block first, then Spanish block for easy reading."
  (let ((id (dragonruby-concept-id concept))
        (name (dragonruby-concept-name concept))
        (level (dragonruby-concept-level concept))
        (scope (dragonruby-concept-scope concept))
        (definition (dragonruby-concept-definition concept))
        (definition-es (dragonruby-concept-definition-es concept))
        (intention (dragonruby-concept-intention concept))
        (intention-es (dragonruby-concept-intention-es concept))
        (mental-model (dragonruby-concept-mental-model concept))
        (mental-model-es (dragonruby-concept-mental-model-es concept))
        (problems (dragonruby-concept-problems concept))
        (limits (dragonruby-concept-limits concept))
        (relations (dragonruby-concept-relations concept))
        (evolution (dragonruby-concept-evolution concept)))
    (concat
     (propertize (format "%s (%s)" name id) 'face '(:height 1.3 :weight bold))
     "\n"
     (propertize (format "[%s | %s]" level scope) 'face 'font-lock-comment-face)
     "\n\n"
     (propertize "═══════════════════════════════════════════════════════════════\n" 'face 'shadow)
     (propertize "ENGLISH\n" 'face '(:height 1.2 :weight bold :foreground "cyan"))
     (propertize "═══════════════════════════════════════════════════════════════\n\n" 'face 'shadow)
     ;; English block
     (when definition
       (concat (propertize "DEFINITION\n" 'face 'bold)
               (format "  %s\n\n" definition)))
     (dragonruby--format-concept-field "INTENTION (Why it exists)" intention)
     (when mental-model
       (concat (propertize "MENTAL MODEL\n" 'face 'bold)
               (format "  %s\n\n" mental-model)))
     (dragonruby--format-concept-field "PROBLEMS IT SOLVES" problems)
     (dragonruby--format-concept-field "LIMITS (What it does NOT do)" limits)
     ;; Spanish block (if available)
     (when (or definition-es intention-es mental-model-es)
       (concat
        (propertize "═══════════════════════════════════════════════════════════════\n" 'face 'shadow)
        (propertize "ESPAÑOL\n" 'face '(:height 1.2 :weight bold :foreground "yellow"))
        (propertize "═══════════════════════════════════════════════════════════════\n\n" 'face 'shadow)
        (when definition-es
          (concat (propertize "DEFINICIÓN\n" 'face 'bold)
                  (format "  %s\n\n" definition-es)))
        (when intention-es
          (dragonruby--format-concept-field "INTENCIÓN (Por qué existe)" intention-es))
        (when mental-model-es
          (concat (propertize "MODELO MENTAL\n" 'face 'bold)
                  (format "  %s\n\n" mental-model-es)))))
     ;; Common sections (language-neutral)
     (propertize "═══════════════════════════════════════════════════════════════\n\n" 'face 'shadow)
     (dragonruby--format-concept-field "RELATIONS" relations)
     (dragonruby--format-concept-field "EVOLUTION" evolution)
     (propertize "═══════════════════════════════════════════════════════════════\n" 'face 'shadow)
     "\n"
     (propertize "Press 'q' to close | 'n'/'p' to navigate | RET to follow relation" 'face 'font-lock-comment-face))))

;;;###autoload
(defun dragonruby-inspect-concept-at-point ()
  "Inspect the DragonRuby concept at point in a dedicated buffer."
  (interactive)
  (let* ((symbol (dragonruby--symbol-at-point))
         (concept (and symbol (dragonruby-get-concept symbol))))
    (if concept
        (dragonruby--show-concept-inspector concept)
      (message "No DragonRuby concept found at point"))))

;;;###autoload
(defun dragonruby-inspect-concept (concept-id)
  "Inspect a DragonRuby concept by CONCEPT-ID.
Prompts for concept ID with completion."
  (interactive
   (list (completing-read "Inspect concept: "
                          (mapcar #'dragonruby-concept-id
                                  (dragonruby-all-concepts))
                          nil t)))
  (let ((concept (dragonruby-get-concept concept-id)))
    (if concept
        (dragonruby--show-concept-inspector concept)
      (message "Concept '%s' not found" concept-id))))

(defun dragonruby--show-concept-inspector (concept)
  "Display CONCEPT in the inspector buffer."
  (let ((buffer (get-buffer-create dragonruby-inspector-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (dragonruby--format-concept concept))
        (goto-char (point-min))
        (dragonruby-inspector-mode)))
    (pop-to-buffer buffer)))

(defvar dragonruby-inspector-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "g") 'dragonruby-inspect-concept)
    (define-key map (kbd "n") 'dragonruby-inspector-next-relation)
    (define-key map (kbd "p") 'dragonruby-inspector-prev-relation)
    (define-key map (kbd "RET") 'dragonruby-inspector-follow-relation)
    map)
  "Keymap for DragonRuby Inspector mode.")

(define-derived-mode dragonruby-inspector-mode special-mode "DR-Inspector"
  "Major mode for inspecting DragonRuby concepts.

\\{dragonruby-inspector-mode-map}"
  (setq buffer-read-only t)
  (setq truncate-lines nil))

(defun dragonruby-inspector-next-relation ()
  "Move to next relation in inspector buffer."
  (interactive)
  (when (re-search-forward "→ \\([^ \n]+\\)" nil t)
    (goto-char (match-beginning 1))))

(defun dragonruby-inspector-prev-relation ()
  "Move to previous relation in inspector buffer."
  (interactive)
  (when (re-search-backward "→ \\([^ \n]+\\)" nil t)
    (goto-char (match-beginning 1))))

(defun dragonruby-inspector-follow-relation ()
  "Follow the relation at point to inspect related concept."
  (interactive)
  (let ((relation-id (thing-at-point 'symbol)))
    (when relation-id
      (let ((concept (dragonruby-get-concept (symbol-name relation-id))))
        (if concept
            (dragonruby--show-concept-inspector concept)
          (message "Related concept '%s' not found" relation-id))))))

;; Helper function for eldoc (defined in dragonruby-eldoc.el)
;; We need to declare it here to avoid compilation warnings
(declare-function dragonruby--symbol-at-point "dragonruby-eldoc")

(provide 'dragonruby-inspector)

;;; dragonruby-inspector.el ends here
