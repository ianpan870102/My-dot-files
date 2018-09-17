(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'load-path "~/.emacs.d/evil")

(setq package-enable-at-startup nil)
(package-initialize)

(require 'evil)
(evil-mode 1)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (solarized-dark)))
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(package-selected-packages
   (quote
    (rainbow-delimiters flycheck neotree autopair solarized-theme linum-relative auto-complete evil-surround evil-commentary))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 160 :width normal :foundry "nil" :family "DejaVuSansMono Nerd Font")))))

(set-cursor-color "#fffafa") 

(setq visible-bell t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode 0)

(global-auto-complete-mode t)

(require 'evil-surround)
(global-evil-surround-mode 1)

(evil-commentary-mode)

(require 'linum-relative)
(linum-relative-global-mode)


(require 'autopair)
(autopair-global-mode)

(add-to-list 'load-path "/.emacs.d/elpa/neotree/")
(require 'neotree)

;; Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Rainbow
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)





(add-to-list 'load-path "/.emacs.d/elpa/neotree/")
(require 'neotree)
