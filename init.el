;;; init.el --- Emacs Configuration for Macgyber

;; 1. Setup Package Manager (Melpa) - Standard practice
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; 2. DragonRuby Mode Configuration
;; --------------------------------
;; Point Emacs to where we installed the package
(add-to-list 'load-path "~/emacs-packages/dragonruby-mode/src")

;; Load the package
(require 'dragonruby)

;; (Optional) Visualize whitespace or line numbers if you like
(global-display-line-numbers-mode 1)
