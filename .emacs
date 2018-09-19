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
 '(ansi-color-names-vector
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#657b83"])
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes (quote (solarized-dark)))
 '(custom-safe-themes
   (quote
    ("62c81ae32320ceff5228edceeaa6895c029cc8f43c8c98a023f91b5b339d633f" "a5956ec25b719bf325e847864e16578c61d8af3e8a3d95f60f9040d02497e408" "f27c3fcfb19bf38892bc6e72d0046af7a1ded81f54435f9d4d09b3bff9c52fc1" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 ;; '(fancy-splash-image nil)
 '(fci-rule-color "#073642")
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
    (("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100))))
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(hl-paren-colors (quote ("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900")))
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(package-selected-packages
   (quote
    (pdf-tools dashboard gruvbox-theme all-the-icons evil-smartparens aggressive-indent which-key smooth-scrolling elpy htmlize org-bullets emmet-mode fill-column-indicator rainbow-delimiters flycheck neotree solarized-theme linum-relative auto-complete evil-surround evil-commentary)))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#c9485ddd1797")
     (60 . "#bf7e73b30bcb")
     (80 . "#b58900")
     (100 . "#a5a58ee30000")
     (120 . "#9d9d91910000")
     (140 . "#9595943e0000")
     (160 . "#8d8d96eb0000")
     (180 . "#859900")
     (200 . "#67119c4632dd")
     (220 . "#57d79d9d4c4c")
     (240 . "#489d9ef365ba")
     (260 . "#3963a04a7f29")
     (280 . "#2aa198")
     (300 . "#288e98cbafe2")
     (320 . "#27c19460bb87")
     (340 . "#26f38ff5c72c")
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83")))
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 165 :width normal :foundry "nil" :family "DejaVuSansMono Nerd Font"))))
 '(bold ((t (:weight normal))))
 '(buffer-menu-buffer ((t (:weight normal))))
 '(neo-dir-link-face ((t (:foreground "#4B90A7" :slant normal :weight bold :height 145 :family "Arial"))))
 '(neo-file-link-face ((t (:foreground "#DADADA" :weight normal :height 135 :family "Arial"))))
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

;; NeoTree
(add-to-list 'load-path "/.emacs.d/elpa/neotree/")
(require 'neotree)
(global-set-key (kbd "s-b") 'neotree-toggle)
(add-hook 'neotree-mode-hook
          (lambda ()
            (define-key evil-normal-state-local-map (kbd "l") 'neotree-enter)
            (define-key evil-normal-state-local-map (kbd "A") 'neotree-stretch-toggle)
            (define-key evil-normal-state-local-map (kbd "zh") 'neotree-hidden-file-toggle)))
(setq neo-theme 'arrow)

;; Rainbow
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Line 80 Ruler
(require 'fill-column-indicator)
(setq fci-rule-column 80)
(setq fci-rule-color "#dc322f")

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

;; Never use the tab character: always convert to spaces!
(setq-default indent-tabs-mode nil)

(smartparens-global-mode 1)
(add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)

(defun always-use-fancy-splash-screens-p () 1)
(defalias 'use-fancy-splash-screens-p 'always-use-fancy-splash-screens-p)
(setq fancy-splash-image (expand-file-name "~/Downloads/emacs.png" ))


;; In order for 'pdflatex' to work
;; Also, had to export PATH from .zshrc
(setenv "PATH" (concat "/usr/texbin:/Library/TeX/texbin:" (getenv "PATH")))
(setq exec-path (append '("/usr/texbin" "/Library/TeX/texbin") exec-path))

(setq make-backup-files nil)

(require 'doc-view)

(setq image-type-available-p 'pdf)
