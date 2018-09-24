;;; .emacs --- Custom Emacs Configuration
;; Author: Ian Y.E. Pan
;;; Commentary:
;;; Code:
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

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
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight light :height 165 :width normal :foundry "nil" :family "Hack"))))
 '(bold ((t (:weight normal))))
 '(buffer-menu-buffer ((t (:weight normal))))
 '(highlight-indentation-face ((t (:background "#3a3a3a" :width condensed))))
 '(line-number ((t (:background "#262626" :foreground "#676767"))))
 '(mode-line ((t (:foreground "#c1c1c1" :background "#333" :box nil))))
 '(mode-line-inactive ((t (:foreground "#3a3a3a" :background "#000" :box nil))))
 '(neo-dir-link-face ((t (:foreground "#F1B03D" :slant normal :weight bold :height 145 :family "San Francisco"))))
 '(neo-file-link-face ((t (:foreground "#D4BA9B" :weight normal :height 140 :family "San Francisco"))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "DarkGoldenrod2"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "DeepPink2"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "DeepSkyBlue1"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "plum2"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "medium sea green"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "sienna1"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "RosyBrown2")))))


(setq ring-bell-function 'ignore)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode 0)

(global-auto-complete-mode t)

(require 'evil-surround)
(global-evil-surround-mode 1)

(evil-commentary-mode)

;; (require 'linum-relative)
;; (global-set-key (kbd "C-j") 'linum-relative-toggle)
(require 'nlinum-relative)
(nlinum-relative-setup-evil)                    ;; setup for evil
(add-hook 'prog-mode-hook 'nlinum-relative-mode)  ;; Use relat.num for prog-mode
(setq nlinum-relative-redisplay-delay 0)      ;; delay
(setq nlinum-relative-current-symbol "")      ;; display current line number
(setq nlinum-relative-offset 0)                 ;; 1 if you want 0, 2, 3...

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
(add-hook 'after-init-hook 'global-flycheck-mode)

;; Line 80 Ruler
(require 'fill-column-indicator)
(setq fci-rule-column 80)
(setq fci-rule-color "#B74835")
(add-hook 'prog-mode-hook 'fci-mode)

(require 'emmet-mode)
(add-hook 'html-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook 'emmet-mode)

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
;; (global-aggressive-indent-mode 1)

;; Never use the tab character: always convert to spaces!
(setq-default tab-width 2)
;; (setq tab-width 2)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)
(setq-default indent-tabs-mode nil)
(setq js-indent-level 2)

(smartparens-global-mode 1)
(add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)

(defun always-use-fancy-splash-screens-p () 1)
(defalias 'use-fancy-splash-screens-p 'always-use-fancy-splash-screens-p)
;; (setq fancy-splash-image (expand-file-name "~/Downloads/rms1.png" ))
(setq fancy-splash-image (expand-file-name "~/Downloads/emacs-logo.png" ))


;; In order for 'pdflatex' to work
;; Also, had to export PATH from .zshrc
(setenv "PATH"
        (
         concat "/usr/texbin:/Library/TeX/texbin:" (getenv "PATH")
         ))
(setq exec-path (append '("/usr/texbin" "/Library/TeX/texbin") exec-path))


(setq make-backup-files nil)

(require 'doc-view)

(require 'hackernews)


;; Word-wrapping
(global-visual-line-mode t)

(setq user-full-name "Ian Y.E. Pan")

(setq frame-title-format '( "%b" " [" (:eval mode-name) "]"))

;;;; Cursor-guide current line
;; (global-hl-line-mode)



(setq-default mode-line-format
              (list
               '(:eval (propertize "(Buffer: %b)" 'face 'font-lock-keyword-face
                                   'help-echo (buffer-file-name)))

               '(:eval (when (buffer-modified-p)
                         (concat " "  (propertize "▲"
                                                  'face 'font-lock-constant-face
                                                  'help-echo "Buffer has been modified"))))
               " {"
               '(:eval (propertize "%m" 'face 'font-lock-string-face
                                   'help-echo buffer-file-coding-system))
               "} "
               '(:eval (propertize (format-time-string "%H:%M ")
                                   'help-echo
                                   (concat (format-time-string "%c; ")
                                           (emacs-uptime "Uptime:%hh"))))
               "["
               (propertize "%p" 'face 'font-lock-constant-face) ;; % above top
               "] [ "
               '(:eval (list (nyan-create)))
               "]"
               " (Line:"
               (propertize "%02l" 'face 'font-lock-type-face)
               ") "
               "%-" ;; fill with '-'
               ))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes (quote (base16-solarized-dark)))
 '(custom-safe-themes
   (quote
    ("50d07ab55e2b5322b2a8b13bc15ddf76d7f5985268833762c500a90e2a09e7aa" "fede08d0f23fc0612a8354e0cf800c9ecae47ec8f32c5f29da841fe090dfc450" default)))
 '(fci-rule-color "#B74835")
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
 '(jdee-server-dir "~/myJars")
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(nyan-animate-nyancat nil)
 '(nyan-animation-frame-interval 0.8)
 '(nyan-bar-length 40)
 '(nyan-cat-face-number 1)
 '(nyan-mode t)
 '(org-format-latex-options
   (quote
    (:foreground default :background default :scale 2.0 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
                 ("begin" "$1" "$" "$$" "\\(" "\\["))))
 '(package-selected-packages
   (quote
    (java-snippets yasnippet-snippets yasnippet-classic-snippets nlinum-relative electric-spacing jdee jedi helm-emmet js2-mode nyan-mode auto-indent-mode which-key solarized-theme smooth-scrolling rainbow-delimiters pdf-tools org-bullets neotree linum-relative htmlize hackernews gruvbox-theme flycheck fill-column-indicator evil-surround evil-smartparens evil-commentary emmet-mode elpy dashboard base16-theme avy auto-complete all-the-icons aggressive-indent)))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(smooth-scroll-margin 2)
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify))
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

(set-cursor-color "#dbdbdb")
(setq-default indicate-empty-lines t)
(global-set-key (kbd "s-r") 'load-file)   ;; Command + 'r' = reload file (then manually specify which file)
(global-set-key (kbd "s-F") 'replace-string)   ;; Command + Shift + f = replace
(global-set-key (kbd "s-s") 'save-buffer)   ;; Command + s = save
(global-set-key (kbd "s-p") 'find-file)   ;; Command + p

;; Indent when RET brackets / parenthesis
(defun newline-and-push-brace ()
  "`newline-and-indent', but bracket aware."
  (interactive)
  (insert "\n")
  (when (looking-at "}")
    (insert "\n")
    (indent-according-to-mode)
    (forward-line -1))
  (indent-according-to-mode)

  (when (looking-at ")")
    (insert "\n")
    (indent-according-to-mode)
    (forward-line -1))
  (indent-according-to-mode)

  (when (looking-at "]")
    (insert "\n")
    (indent-according-to-mode)
    (forward-line -1))
  (indent-according-to-mode)

  )
(global-set-key (kbd "RET") 'newline-and-push-brace)


(require 'auto-indent-mode)


(setq show-paren-delay 0)
(show-paren-mode 1)

;; Spell checker software Aspell (to replace ispell)
(setq ispell-program-name "/usr/local/bin/aspell")

;; Use Command + f for easymotion!
(global-set-key (kbd "s-f") 'avy-goto-char)
(setq avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?n ?w ?e ?r ?y ?u ?i ?o ?t ?v ?l))
(setq avy-background t)

;; IDO Mode
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; Command + Enter: fire up eshell in current frame
(global-set-key (kbd "<s-return>") 'eshell)


;; Customize Eshell prompt
(setq eshell-prompt-function (lambda nil
                               (concat
                                (propertize "\n╭─" 'face `(:foreground "#fe8019"))
                                (format-time-string "%H:%M:%S " (current-time))
                                (propertize (eshell/pwd) 'face `(:foreground "#B0AE37"))
                                (propertize "\n╰─ ~ ✘ " 'face `(:foreground "#fe8019"))
                                )))

(setq eshell-highlight-prompt nil)


;; Clear the EShell buffer and end at the top (instead of the bottom)
(defun eshell/clear ()
  "Clear the eshell buffer to the top."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    ))


(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(js2-imenu-extras-mode)
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-jsx-mode))
;; (add-to-list 'interpreter-mode-alist '("node" . js2-jsx-mode)


;; Set Python default to Python3
(setq elpy-rpc-python-command "/usr/local/bin/python3")
(setq python-shell-interpreter "/usr/local/bin/python3")


;; Set Environment Variables
;; to let Eshell use all the brew-installed commands.
(setenv "PATH"
        (concat
         "/usr/local/bin/" ":"
         (getenv "PATH")
         )
        )

;; Disable Python's ugly indent-guide
(add-hook 'elpy-mode-hook (lambda () (highlight-indentation-mode -1)))


;;; .emacs ends here
