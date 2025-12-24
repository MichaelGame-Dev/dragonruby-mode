;;; test-concepts.el --- Tests for DragonRuby concept system  -*- lexical-binding: t; -*-

(require 'ert)
(require 'dragonruby-registry)
(require 'dragonruby-args)
(require 'dragonruby-args-sub)

;; ============================================================================
;; Contract Compliance Tests
;; ============================================================================
;; These tests verify that the project adheres to the principles in CONTRACT.md
;; Specifically: "Everything must be traceable" and "Everything must be inspectable"

(ert-deftest test-all-concepts-have-definition ()
  "Verify all registered concepts have a definition.
This ensures compliance with the contract principle:
'Makes implicit engine behavior explicit and visible'."
  (dolist (concept (dragonruby-all-concepts))
    (should (dragonruby-concept-definition concept))
    (should (stringp (dragonruby-concept-definition concept)))
    (should (not (string-empty-p (dragonruby-concept-definition concept))))))

(ert-deftest test-all-concepts-have-intention ()
  "Verify all registered concepts have an intention field.
This ensures users can understand WHY a concept exists, not just WHAT it is."
  (dolist (concept (dragonruby-all-concepts))
    (should (dragonruby-concept-intention concept))
    (should (stringp (dragonruby-concept-intention concept)))
    (should (not (string-empty-p (dragonruby-concept-intention concept))))))

(ert-deftest test-all-concepts-have-mental-model ()
  "Verify all registered concepts have a mental-model.
Mental models are the MOST IMPORTANT field for understanding.
This is a critical contract requirement."
  (dolist (concept (dragonruby-all-concepts))
    (should (dragonruby-concept-mental-model concept))
    (should (stringp (dragonruby-concept-mental-model concept)))
    (should (not (string-empty-p (dragonruby-concept-mental-model concept))))))

(ert-deftest test-core-concepts-have-problems-and-limits ()
  "Verify core concepts have problems and limits defined.
Core concepts must be especially rich since they're foundational."
  (dolist (concept (dragonruby-all-concepts))
    (when (eq (dragonruby-concept-level concept) 'core)
      (should (dragonruby-concept-problems concept))
      (should (listp (dragonruby-concept-problems concept)))
      (should (> (length (dragonruby-concept-problems concept)) 0))
      (should (dragonruby-concept-limits concept))
      (should (listp (dragonruby-concept-limits concept)))
      (should (> (length (dragonruby-concept-limits concept)) 0)))))

(ert-deftest test-concepts-are-traceable ()
  "Verify concepts with relations point to valid concepts.
This ensures the knowledge graph is navigable (contract: 'Everything must be traceable')."
  (dolist (concept (dragonruby-all-concepts))
    (let ((relations (dragonruby-concept-relations concept)))
      (when relations
        (dolist (relation relations)
          (let ((related-id (cdr relation)))
            ;; Related concept should exist in registry
            (should (dragonruby-get-concept related-id))))))))

(ert-deftest test-concept-registry-basic ()
  "Test basic concept registry operations."
  (let ((args-concept (dragonruby-get-concept "args")))
    (should args-concept)
    (should (equal (dragonruby-concept-id args-concept) "args"))
    (should (equal (dragonruby-concept-name args-concept) "Frame Arguments"))))

(ert-deftest test-args-subconcepts-exist ()
  "Verify that args sub-concepts are registered."
  (should (dragonruby-get-concept "args.state"))
  (should (dragonruby-get-concept "args.outputs"))
  (should (dragonruby-get-concept "args.inputs")))

(ert-deftest test-concept-presentation-field ()
  "Verify concepts have presentation preferences defined."
  (dolist (concept (dragonruby-all-concepts))
    (should (dragonruby-concept-presentation concept))
    (should (listp (dragonruby-concept-presentation concept)))))

;; ============================================================================
;; Quality Tests
;; ============================================================================

(ert-deftest test-definitions-are-concise ()
  "Verify definitions are concise (ideally one line, max 200 chars).
Long definitions indicate unclear thinking."
  (dolist (concept (dragonruby-all-concepts))
    (let ((definition (dragonruby-concept-definition concept)))
      (should (< (length definition) 200)))))

(ert-deftest test-no-orphan-concepts ()
  "Verify no concepts are isolated (all should have relations or be referenced).
Isolated concepts don't build understanding."
  (let ((all-concepts (dragonruby-all-concepts))
        (referenced-ids '()))
    ;; Collect all referenced concept IDs
    (dolist (concept all-concepts)
      (dolist (relation (dragonruby-concept-relations concept))
        (push (cdr relation) referenced-ids)))
    ;; Every concept should either have relations OR be referenced
    (dolist (concept all-concepts)
      (let ((id (dragonruby-concept-id concept))
            (relations (dragonruby-concept-relations concept)))
        (should (or relations
                    (member id referenced-ids)))))))

;;; test-concepts.el ends here
