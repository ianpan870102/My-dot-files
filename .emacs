;;; .emacs --- Custom Emacs Configuration
;; Author: Ian Y.E. Pan
;;; Commentary:
;;; Code:
(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'load-path "~/.emacs.d/evil")

(setq package-enable-at-startup nil)
(package-initialize)

(require 'evil)
(evil-mode 1)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#262626" :foreground "#dab997" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 165 :width normal :foundry "nil" :family "Monaco"))))
 '(bold ((t (:weight normal))))
 '(buffer-menu-buffer ((t (:weight normal))))
 '(mode-line ((t (:foreground "#c1c1c1" :background "#5E421F" :box nil))))
 '(mode-line-inactive ((t (:foreground "#000" :background "#616161" :box nil))))
 '(neo-dir-link-face ((t (:foreground "#F1B03D" :slant normal :weight bold :height 155 :family "San Francisco"))))
 '(neo-file-link-face ((t (:foreground "#D4BA9B" :weight normal :height 155 :family "San Francisco"))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "DeepSkyBlue1"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "DarkGoldenrod2"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "DeepPink2"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "plum2"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "medium sea green"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "sienna1"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "RosyBrown2"))))
 '(scroll-bar ((t (:background "black" :foreground "salmon4" :underline nil :weight normal)))))


(setq ring-bell-function 'ignore)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode 0)

(global-auto-complete-mode t)

(require 'evil-surround)
(global-evil-surround-mode 1)

(evil-commentary-mode)

(require 'linum-relative)
;; (linum-relative-global-mode t)

;; NeoTree
(add-to-list 'load-path "/.emacs.d/elpa/neotree/")
(require 'neotree)
(global-set-key (kbd "s-b") 'neotree-toggle)
(add-hook 'neotree-mode-hook
          (lambda ()
            (define-key evil-normal-state-local-map (kbd "l") 'neotree-enter)
            (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)
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


(elpy-enable)
(define-key yas-minor-mode-map (kbd "C-c k") 'yas-expand)
(define-key global-map (kbd "C-c o") 'iedit-mode)

(require 'smooth-scrolling)
(smooth-scrolling-mode 1)

(add-to-list 'load-path "path/to/which-key.el")
(require 'which-key)
(which-key-mode)


;; Aggressive-Indent
(global-aggressive-indent-mode 1)

;; Never use the tab character: always convert to spaces!
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

(smartparens-global-mode 1)
(add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)

(defun always-use-fancy-splash-screens-p () 1)
(defalias 'use-fancy-splash-screens-p 'always-use-fancy-splash-screens-p)
(setq fancy-splash-image (expand-file-name "~/Downloads/rms1.png" ))


;; In order for 'pdflatex' to work
;; Also, had to export PATH from .zshrc
(setenv "PATH" (concat "/usr/texbin:/Library/TeX/texbin:" (getenv "PATH")))
(setq exec-path (append '("/usr/texbin" "/Library/TeX/texbin") exec-path))

(setq make-backup-files nil)

(require 'doc-view)

(require 'hackernews)


;; Word-wrapping
(global-visual-line-mode t)

(setq user-full-name "Ian Y.E. Pan")

(setq frame-title-format '( "%b" " [" (:eval mode-name) "]"))

;; Cursor-guide current line
;; (global-hl-line-mode)


(setq-default mode-line-format
              (list
               '(:eval (propertize "  %b" 'face 'font-lock-keyword-face
                                   'help-echo (buffer-file-name)))

               '(:eval (when (buffer-modified-p)
                         (concat ":"  (propertize " ✘Save me!✘"
                                                  'face 'font-lock-warning-face
                                                  'help-echo "Buffer has been modified"))))
               " (Line:"
               (propertize "%02l" 'face 'font-lock-type-face)
               ")  "
               "["
               (propertize "%p" 'face 'font-lock-constant-face) ;; % above top
               " | "
               (propertize "%I" 'face 'font-lock-constant-face) ;; size
               "]  "
               "{"
               '(:eval (propertize "%m" 'face 'font-lock-string-face
                                   'help-echo buffer-file-coding-system))
               "} "
               "[" ;; insert vs overwrite mode, input-method in a tooltip
               '(:eval (propertize (if overwrite-mode "Ovr" "Ins")
                                   'face 'font-lock-preprocessor-face
                                   'help-echo (concat "Buffer is in "
                                                      (if overwrite-mode "overwrite" "insert") " mode")))
               '(:eval (when buffer-read-only
                         (concat "|"  (propertize "R-O"
                                                  'face 'font-lock-type-face
                                                  'help-echo "Buffer is read-only"))))
               "] "
               ;; add the time, with the date and the emacs uptime in the tooltip
               '(:eval (propertize (format-time-string "%H:%M")
                                   'help-echo
                                   (concat (format-time-string "%c; ")
                                           (emacs-uptime "Uptime:%hh"))))
               " ------ Minor:("
               minor-mode-alist  ;; list of minor modes
               ")"
               "%-" ;; fill with '-'
               ))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (base16-gruvbox-dark-pale)))
 '(custom-safe-themes
   (quote
    ("50d07ab55e2b5322b2a8b13bc15ddf76d7f5985268833762c500a90e2a09e7aa" "fede08d0f23fc0612a8354e0cf800c9ecae47ec8f32c5f29da841fe090dfc450" default)))
 '(package-selected-packages
   (quote
    (which-key solarized-theme smooth-scrolling rainbow-delimiters pdf-tools org-bullets neotree linum-relative htmlize hackernews gruvbox-theme flycheck fill-column-indicator evil-surround evil-smartparens evil-commentary emmet-mode elpy dashboard base16-theme avy auto-complete all-the-icons aggressive-indent))))

(set-cursor-color "#FFFAFA")
(setq-default indicate-empty-lines t)
(global-set-key (kbd "s-r") 'load-file)   ;; Command + 'r' = reload file (then manually specify which file)
(global-set-key (kbd "s-F") 'replace-string)   ;; Command + Shift + f = replace
(global-set-key (kbd "s-s") 'save-buffer)   ;; Command + s = save
(global-set-key (kbd "s-p") 'find-file)   ;; Command + p
(global-set-key (kbd "C-j") 'linum-relative-toggle)   ;; Ctrl + j (just like in vim)

;;; .emacs ends here
