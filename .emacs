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
    (evil-smartparens aggressive-indent which-key smooth-scrolling elpy htmlize org-bullets emmet-mode fill-column-indicator rainbow-delimiters flycheck neotree autopair solarized-theme linum-relative auto-complete evil-surround evil-commentary))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 165 :width normal :foundry "nil" :family "DejaVuSansMono Nerd Font"))))
 '(bold ((t (:weight normal))))
 '(buffer-menu-buffer ((t (:weight normal))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "DeepSkyBlue1"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "DarkGoldenrod2"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "DeepPink2"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "plum2"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "medium sea green"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "sienna1"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "RosyBrown2")))))

;; Disable all bold fonts and underlines
(mapc
 (lambda (face)
   (set-face-attribute face nil :weight 'normal :underline nil))
 (face-list))

(set-cursor-color "#FFFAFA") 

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

;; NeoTree
(add-to-list 'load-path "/.emacs.d/elpa/neotree/")
(require 'neotree)
(global-set-key (kbd "s-b") 'neotree-toggle)
(add-hook 'neotree-mode-hook
              (lambda ()
                (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)
                (define-key evil-normal-state-local-map (kbd "A") 'neotree-stretch-toggle)
                (define-key evil-normal-state-local-map (kbd "zh") 'neotree-hidden-file-toggle)))

;; Rainbow
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Line 80 Ruler
(require 'fill-column-indicator)
(setq fci-rule-column 80)

(require 'emmet-mode)
(add-hook 'html-mode-hook #'emmet-mode)

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(setq frame-title-format '("Emacs 26.1 "))


(elpy-enable)

(require 'smooth-scrolling)
(smooth-scrolling-mode 1)

(add-to-list 'load-path "path/to/which-key.el")
(require 'which-key)
(which-key-mode)


;; Aggressive-Indent
(global-aggressive-indent-mode 1)
(add-to-list 'aggressive-indent-excluded-modes 'html-mode)

;; Never use the tab character: always convert to spaces!
(setq-default indent-tabs-mode nil)

(smartparens-global-mode 1)
(add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)
