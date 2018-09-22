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
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight semi-light :height 155 :width ultra-condensed :foundry "nil" :family "Monaco"))))
 '(bold ((t (:weight normal))))
 '(buffer-menu-buffer ((t (:weight normal))))
 '(mode-line ((t (:foreground "#c1c1c1" :background "#3a3a3a" :box nil))))
 '(mode-line-inactive ((t (:foreground "#000" :background "#3a3a3a" :box nil))))
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
;; (global-aggressive-indent-mode 1)

;; Never use the tab character: always convert to spaces!
(setq-default tab-width 2)
(setq tab-width 2)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)
(setq-default indent-tabs-mode nil)

(smartparens-global-mode 1)
(add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)

(defun always-use-fancy-splash-screens-p () 1)
(defalias 'use-fancy-splash-screens-p 'always-use-fancy-splash-screens-p)
;; (setq fancy-splash-image (expand-file-name "~/Downloads/rms1.png" ))
(setq fancy-splash-image (expand-file-name "~/Downloads/emacs-logo.png" ))


;; In order for 'pdflatex' to work
;; Also, had to export PATH from .zshrc
(setenv "PATH" (concat "/usr/texbin:/Library/TeX/texbin:" (getenv "PATH")))
(setq exec-path (append '("/usr/texbin" "/Library/TeX/texbin") exec-path))

(setq make-backup-files nil)

(require 'doc-view)

(require 'hackernews)


;; Word-wrapping
(global-visual-line-mode t)
;; (auto-fill-mode t)

(setq user-full-name "Ian Y.E. Pan")

(setq frame-title-format '( "%b" " [" (:eval mode-name) "]"))

;;;; Cursor-guide current line
;; (global-hl-line-mode)



(setq-default mode-line-format
              (list
               '(:eval (propertize "  %b" 'face 'font-lock-keyword-face
                                   'help-echo (buffer-file-name)))

               '(:eval (when (buffer-modified-p)
                         (concat " "  (propertize "▲"
                                                  'face 'font-lock-constant-face
                                                  'help-echo "Buffer has been modified"))))
               " (Line:"
               (propertize "%02l" 'face 'font-lock-type-face)
               ") "
               "["
               (propertize "%p" 'face 'font-lock-constant-face) ;; % above top
               "] "
               "{"
               '(:eval (propertize "%m" 'face 'font-lock-string-face
                                   'help-echo buffer-file-coding-system))
               "} "
               ;; "[" ;; insert vs overwrite mode, input-method in a tooltip
               ;; '(:eval (propertize (if overwrite-mode "Ovr" "Ins")
               ;;                     'face 'font-lock-preprocessor-face
               ;;                     'help-echo (concat "Buffer is in "
               ;;                                        (if overwrite-mode "overwrite" "insert") " mode")))
               ;; '(:eval (when buffer-read-only
               ;;           (concat "|"  (propertize "R-O"
               ;;                                    'face 'font-lock-type-face
               ;;                                    'help-echo "Buffer is read-only"))))
               ;; "] "
               ;; add the time, with the date and the emacs uptime in the tooltip
               '(:eval (propertize (format-time-string "%H:%M [ ")
                                   'help-echo
                                   (concat (format-time-string "%c; ")
                                           (emacs-uptime "Uptime:%hh"))))
               '(:eval (list (nyan-create)))
               "]"
               ;; "Minor:("
               ;; minor-mode-alist  ;; list of minor modes
               ;; ")"

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
 '(jdee-server-dir "~/myJars")
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
    (electric-spacing jdee jedi helm-emmet js2-mode nyan-mode auto-indent-mode which-key solarized-theme smooth-scrolling rainbow-delimiters pdf-tools org-bullets neotree linum-relative htmlize hackernews gruvbox-theme flycheck fill-column-indicator evil-surround evil-smartparens evil-commentary emmet-mode elpy dashboard base16-theme avy auto-complete all-the-icons aggressive-indent)))
 '(smooth-scroll-margin 2)
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify)))

(set-cursor-color "#FFFAFA")
(setq-default indicate-empty-lines t)
(global-set-key (kbd "s-r") 'load-file)   ;; Command + 'r' = reload file (then manually specify which file)
(global-set-key (kbd "s-F") 'replace-string)   ;; Command + Shift + f = replace
(global-set-key (kbd "s-s") 'save-buffer)   ;; Command + s = save
(global-set-key (kbd "s-p") 'find-file)   ;; Command + p
(global-set-key (kbd "C-j") 'linum-relative-toggle)   ;; Ctrl + j (just like in vim)

(defun newline-and-push-brace ()
  "`newline-and-indent', but bracket aware."
  (interactive)
  (insert "\n")
  (when (looking-at "}")
    (insert "\n")
    (indent-according-to-mode)
    (forward-line -1))
  (indent-according-to-mode))
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
        (propertize "\n╰─ ~ ∃ " 'face `(:foreground "#fe8019"))
    )))

  (setq eshell-highlight-prompt nil)


;; Clear the EShell buffer and end at the top (instead of the bottom)
(defun eshell/clear ()
    "Clear the eshell buffer."
    (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(js2-imenu-extras-mode)
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-jsx-mode))
;; (add-to-list 'interpreter-mode-alist '("node" . js2-jsx-mode)

(setq elpy-rpc-python-command "/usr/local/bin/python3")
(setq python-shell-interpreter "/usr/local/bin/python3")


;; Set Environment Variables
(setenv "PATH"
  (concat
   "/usr/local/bin/" ":"
   (getenv "PATH")
  )
)

;; (add-hook 'python-mode-hook #'electric-spacing-mode)
;; (add-hook 'org-mode-hook #'electric-spacing-mode)
;; (add-hook 'emacs-lisp-mode-hook #'electric-spacing-mode)
;; (add-hook 'jdee-mode-hook #'electric-spacing-mode)


;;; .emacs ends here
