;;; dragonruby.el --- DragonRuby Cognitive Mode  -*- lexical-binding: t; -*-

;; Author: Macgyber <esteban3261g@gmail.com>
;; Version: 0.2.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: games, dragonruby, tools, help
;; URL: https://github.com/Macgyber/dragonruby-mode

;;; Commentary:
;; Semantic Extension for DragonRuby.

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
    (add-to-list 'load-path (expand-file-name "features" current-dir))))

;; 2. Core
(require 'dragonruby-project)
;; (require 'dragonruby-config) ; Uncomment if config is needed

;; 3. Features
(require 'dragonruby-colors)
(require 'dragonruby-sprites)
(require 'dragonruby-paths)

;; 4. Orchestration
(require 'dragonruby-mode)

(provide 'dragonruby)
;;; dragonruby.el ends here