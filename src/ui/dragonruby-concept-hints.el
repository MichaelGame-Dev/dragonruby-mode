;;; dragonruby-concept-hints.el --- Concept hints in comments  -*- lexical-binding: t; -*-

;; This module implements interactive concept detection in code comments.
;; When a comment contains "concepts: foo, bar", those become clickable links
;; to inspect the concepts.

(require 'dragonruby-registry)
(require 'dragonruby-inspector)

(defface dragonruby-concept-hint-face
  '((t :inherit font-lock-comment-face :underline t))
  "Face for concept hints in comments."
  :group 'dragonruby)

(defvar dragonruby-concept-hint-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'dragonruby-inspect-concept-from-hint)
    (define-key map (kbd "<mouse-1>") 'dragonruby-inspect-concept-from-hint)
    map)
  "Keymap for concept hints.")

(defun dragonruby-inspect-concept-from-hint ()
  "Inspect the concept at point in a hint."
  (interactive)
  (let ((concept-id (get-text-property (point) 'dragonruby-concept-id)))
    (when concept-id
      (let ((concept (dragonruby-get-concept concept-id)))
        (if concept
            (dragonruby--show-concept-inspector concept)
          (message "Concept '%s' not found" concept-id))))))

(defun dragonruby--fontify-concept-hints (limit)
  "Fontify concept hints in comments up to LIMIT."
  (while (re-search-forward
          "^[[:space:]]*#[[:space:]]*concepts?:[[:space:]]*\\([^\n]+\\)"
          limit t)
    (let* ((concepts-str (match-string 1))
           (concepts (split-string concepts-str "[[:space:]]*,[[:space:]]*" t))
           (start (match-beginning 1)))
      (dolist (concept-id concepts)
        (let* ((trimmed-id (string-trim concept-id))
               (concept (dragonruby-get-concept trimmed-id)))
          (when concept
            ;; Find the position of this concept in the matched string
            (when (string-match (regexp-quote trimmed-id) concepts-str)
              (let ((concept-start (+ start (match-beginning 0)))
                    (concept-end (+ start (match-end 0))))
                ;; Add text properties for interactivity
                (add-text-properties
                 concept-start concept-end
                 `(face dragonruby-concept-hint-face
                   mouse-face highlight
                   help-echo ,(format "Click to inspect concept: %s" trimmed-id)
                   dragonruby-concept-id ,trimmed-id
                   keymap ,dragonruby-concept-hint-keymap))))))))))

(defun dragonruby-enable-concept-hints ()
  "Enable concept hint detection in the current buffer."
  (font-lock-add-keywords
   nil
   '((dragonruby--fontify-concept-hints 0 nil t)))
  (font-lock-flush))

(provide 'dragonruby-concept-hints)

;;; dragonruby-concept-hints.el ends here
