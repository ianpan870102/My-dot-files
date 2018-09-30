;;; .emacs --- Custom Emacs Configuration
;; Author: Ian Y.E. Pan
;;; Commentary:
;;; Code:
(require 'package)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'load-path "~/.emacs.d/evil")
(setq package-enable-at-startup nil)
(package-initialize)

;; Load theme by hand, don't use 'customize'
(load-theme 'base16-solarized-dark t)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil
                         :strike-through nil :overline nil :underline nil
                         :slant normal :weight light :height 150 :width normal
                         :foundry "nil" :family "DejaVu Sans Mono"))))
 '(avy-lead-face ((t (:foreground "#ff0000"))))
 '(avy-lead-face-0 ((t (:foreground "#EEAD0F"))))
 '(bold ((t (:weight normal))))
 '(buffer-menu-buffer ((t (:weight normal))))
 '(highlight-indentation-face ((t (:background "#3a3a3a" :width condensed))))
 '(mode-line ((t (:foreground "#c1c1c1" :background "#333" :box nil))))
 '(mode-line-inactive ((t (:foreground "#3a3a3a" :background "#000" :box nil))))
 '(neo-dir-link-face ((t (:foreground "#EEAD0F" :slant normal :weight bold
                                      :height 140 :family "San Francisco"))))
 '(neo-file-link-face ((t (:foreground "#E4DECD" :weight normal :height 140
                                       :family "San Francisco"))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "DarkGoldenrod2"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "DeepPink2"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "DeepSkyBlue1")))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(avy-background t)
 '(display-battery-mode t)
 '(jdee-server-dir "~/myJars")
 '(neo-autorefresh t)
 '(neo-window-position (quote right))
 '(neo-window-width 30)
 '(nyan-animate-nyancat nil)
 '(nyan-bar-length 40)
 '(nyan-cat-face-number 1)
 '(nyan-mode t)
 '(package-selected-packages
   (quote
    (column-enforce-mode yasnippet-snippets yasnippet-classic-snippets
                         which-key smooth-scrolling shrink-path scroll-restore
                         rainbow-delimiters projectile prettier-js pdf-tools
                         org-bullets nyan-mode nlinum-relative neotree
                         linum-relative js2-mode jedi jdee java-snippets
                         htmlize helm-emmet evil-surround evil-smartparens
                         evil-commentary elpy eldoc-eval dashboard base16-theme
                         avy auto-indent-mode)))
 '(python-indent-guess-indent-offset nil)
 '(smooth-scroll-margin 2))

(setq user-full-name "Ian Y.E. Pan")

;; Start-up
(setq frame-title-format '( "%b" " [" (:eval mode-name) "]"))
(defun always-use-fancy-splash-screens-p () "Use splash screen on start-up." 1)
(defalias 'use-fancy-splash-screens-p 'always-use-fancy-splash-screens-p)
(setq fancy-splash-image (expand-file-name "~/Downloads/emacs-logo.png" ))

;; Cleaning up the interface
(setq ring-bell-function 'ignore)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode 0)

;; Auto-completion
(add-hook 'after-init-hook 'global-company-mode)
(setq company-global-modes '(not eshell-mode))  ;; No auto-completion in eshell
(setq company-idle-delay t) ;; no delay
(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous))

;; EVIL
(setq evil-want-C-u-scroll t)
(require 'evil)
(evil-mode 1)
(require 'evil-surround)
(global-evil-surround-mode 1)
(evil-commentary-mode)

;; Line Numbers
(require 'nlinum-relative)
(nlinum-relative-setup-evil)                    ;; setup for evil
(add-hook 'prog-mode-hook 'nlinum-relative-mode)
(setq nlinum-relative-redisplay-delay 0)      ;; delay
(setq nlinum-relative-current-symbol "")      ;; display current line number
(setq nlinum-relative-offset 0)               ;; 1 if want 0, 2, 3...

;; NeoTree
(add-to-list 'load-path "/.emacs.d/elpa/neotree/")
(require 'neotree)
(global-set-key (kbd "s-b") 'neotree-toggle)
(add-hook 'neotree-mode-hook
          (lambda ()
            (define-key evil-normal-state-local-map
              (kbd "l") 'neotree-enter)
            (define-key evil-normal-state-local-map
              (kbd "RET") 'neotree-enter)
            (define-key evil-normal-state-local-map
              (kbd "A") 'neotree-stretch-toggle)
            (define-key evil-normal-state-local-map
              (kbd "zh") 'neotree-hidden-file-toggle)))
(setq neo-theme 'arrow)

;; Rainbow Brackets
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'jdee-mode-hook 'rainbow-delimiters-mode)

;; Flycheck
(add-hook 'after-init-hook 'global-flycheck-mode)

;; Highlight red when pass column-80, since fci-mode breaks a lot of things.
(add-hook 'prog-mode-hook 'column-enforce-mode)
(setq column-enforce-column 79)

(require 'emmet-mode)
(add-hook 'html-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook 'emmet-mode)

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; Python
(elpy-enable)
(setq elpy-rpc-python-command "/usr/local/bin/python3")
(setq python-shell-interpreter "/usr/local/bin/python3")
;; disable Python's ugly indent-guide
(add-hook 'elpy-mode-hook (lambda () (highlight-indentation-mode -1)))

;; Yasnippets enable
(yas-global-mode 1)

(require 'smooth-scrolling)
(smooth-scrolling-mode 1)

(add-to-list 'load-path "path/to/which-key.el")
(require 'which-key)
(which-key-mode)

;; Indenting
(setq-default tab-width 2)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)
(setq-default indent-tabs-mode nil)
(setq js-indent-level 2)
(setq c-default-style '((java-mode . "java") (other . "gnu")))

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
  (indent-according-to-mode))
(global-set-key (kbd "RET") 'newline-and-push-brace)
(require 'auto-indent-mode)

;; Always syntax highlight
(global-font-lock-mode t)

;; Smart Parenthesis
(smartparens-global-mode 1)
(add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)

;; In order for 'pdflatex' to work
;; Also had to export PATH from .zshrc
(setenv "PATH"
        (concat "/usr/texbin:/Library/TeX/texbin:" (getenv "PATH")))
(setq exec-path (append '("/usr/texbin" "/Library/TeX/texbin") exec-path))

;; No 'index.js~' back-ups
(setq make-backup-files nil)

;; Word-wrapping
(global-visual-line-mode t)

;; Mode Line
(setq-default mode-line-format
              (list
               '(:eval (propertize "(Buffer: %b)" 'face 'font-lock-keyword-face
                                   'help-echo (buffer-file-name)))

               '(:eval (when (buffer-modified-p)
                         (concat " "  (propertize "▲"
                                                  'face 'font-lock-constant-face
                                                  'help-echo
                                                  "Buffer has been modified"))))
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

(setq-default indicate-empty-lines t)

;; Some macOS-like keybindings
(global-set-key (kbd "s-r") 'load-file)   ;; Command + 'r' = reload file
(global-set-key (kbd "s-F") 'replace-string)   ;; Command + Shift + f = replace
(global-set-key (kbd "s-s") 'save-buffer)   ;; Command + s = save
(global-set-key (kbd "s-p") 'find-file)   ;; Command + p

(setq show-paren-delay 0)
(show-paren-mode 1)

;; Spell checker software Aspell (to replace ispell)
(setq ispell-program-name "/usr/local/bin/aspell")

;; Avy-easymotion: Cmd + f
(global-set-key (kbd "s-f") 'avy-goto-char)
(setq avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?n ?w ?e ?r ?y ?u ?i ?o ?t ?v ?l))
(setq avy-background t)

;; IDO Mode
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; Eshell
(global-set-key (kbd "<s-return>") 'eshell)
;; (setq eshell-prompt-function (lambda nil
;;                          (concat
;;                           (propertize "\n╭─" 'face
;;                                       `(:foreground "#fe8019"))
;;                           (format-time-string "%H:%M:%S " (current-time))
;;                           (propertize (eshell/pwd) 'face
;;                                       `(:foreground "#B0AE37"))
;;                           (propertize "\n╰─ ~ ✘ " 'face
;;                                       `(:foreground "#fe8019"))
;;                           )))
(setq eshell-prompt-function (lambda nil
                            (concat
                            (propertize "\n" 'face
                                        `(:foreground "#fe8019"))
                            (propertize "▶ " 'face
                                        `(:foreground "#54C22A"))
                            (propertize (if (string= (eshell/pwd)
                                                        (getenv "HOME"))
                                            "~" (eshell/basename
                                                    (eshell/pwd))) 'face
                                                    `(:foreground "#fe8019"))
                            (propertize " " 'face
                                        `(:foreground "#93A1A1"))
                            )))
(setq eshell-highlight-prompt nil)

(defun eshell/clear ()
  "Clear the eshell buffer to the top."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(global-set-key (kbd "C-8") 'eshell-previous-input)
(global-set-key (kbd "C-9") 'eshell-next-input)
;; To let Eshell use brew-installed commands
(setenv "PATH" (concat "/usr/local/bin/" ":" (getenv "PATH")))
;; Eshell aliases
(defalias 'ff 'find-file)

;; JavaScript
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(js2-imenu-extras-mode)
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-jsx-mode))
(require 'prettier-js)
(add-hook 'js2-mode-hook 'prettier-js-mode)
(add-hook 'web-mode-hook 'prettier-js-mode)

(set-cursor-color "#B0C1C1")

;; Natural color title-bar (matching theme)
(add-to-list 'default-frame-alist
             '(ns-transparent-titlebar . t))

;; macOS dark title-bar
(add-to-list 'default-frame-alist
             '(ns-appearance . dark))

;; Enter new buffer in Dired and kill old one
(put 'dired-find-alternate-file 'disabled nil)
(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map (kbd "RET")
              'dired-find-alternate-file)))

;; Use `C-x r j e' to jump to .emacs
(set-register ?e (cons 'file "~/.emacs"))

;;; .emacs ends here
