;;; dragonruby.el --- DragonRuby Cognitive Mode  -*- lexical-binding: t; -*-

;; Author: Macgyber <esteban3261g@gmail.com>
;; Version: 0.1.1
;; Package-Requires: ((emacs "26.1"))
;; Keywords: games, dragonruby, tools, help
;; URL: https://github.com/Macgyber/dragonruby-mode

;;; Commentary:
;; Cognitive Amplifier for DragonRuby.

;;; Code:

;; 1. Setup Load Path
(eval-and-compile
  (let ((current-dir (file-name-directory 
                      (cond
                       (load-in-progress load-file-name)
                       ((and (boundp 'byte-compile-current-file)
                             byte-compile-current-file)
                        byte-compile-current-file)
                       (t (buffer-file-name))))))
    (add-to-list 'load-path (expand-file-name "core" current-dir))
    (add-to-list 'load-path (expand-file-name "modules" current-dir))
    (add-to-list 'load-path (expand-file-name "ui" current-dir))
    (add-to-list 'load-path (expand-file-name "mode" current-dir))
    (add-to-list 'load-path (expand-file-name "concepts" current-dir))))

;; 2. Infrastructure
(require 'dragonruby-core-concepts)
(require 'dragonruby-registry)
(require 'dragonruby-config)
(require 'dragonruby-project)

;; 3. Knowledge Base
(require 'dragonruby-args)
(require 'dragonruby-args-sub)
(require 'dragonruby-colors)

;; 4. Orchestration
(require 'dragonruby-impl)

(provide 'dragonruby)
;;; dragonruby.el ends here