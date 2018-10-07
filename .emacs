;;; .emacs --- Custom Emacs Configuration
;; Author: Ian Y.E. Pan
;;; Commentary:
;;; Code:
(require 'package)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'load-path "~/.emacs.d/evil")
(setq package-enable-at-startup nil)
(package-initialize)

;; Load theme by hand, don't use 'M-x customize-themes'
(load-theme 'spacemacs-dark t)
(add-hook 'prog-mode-hook 'highlight-numbers-mode)
(add-hook 'prog-mode-hook 'highlight-operators-mode)
(hes-mode)  ;; highlight escape sequences

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight light :height 150 :width normal :foundry "nil" :family "DejaVusansmono nerd font"))))
 '(avy-lead-face ((t (:foreground "#F95323"))))
 '(avy-lead-face-0 ((t (:foreground "#EEAD0F"))))
 '(bold ((t (:weight normal))))
 '(dashboard-banner-logo-title-face ((t (:inherit default :overline t :height 1.15 :family "Monaco"))))
 '(dashboard-heading-face ((t (:inherit default :foreground "#EAB102" :height 1.1))))
 '(magit-diff-added-highlight ((t (:background "#339532" :foreground "#cceecc"))))
 '(magit-diff-removed-highlight ((t (:background "#a73335" :foreground "#eecccc"))))
 '(mode-line ((t (:foreground "#c1c1c1" :background "#000" :box nil))))
 '(mode-line-inactive ((t (:foreground "#3a3a3a" :background "#000" :box nil))))
 '(neo-dir-link-face ((t (:foreground "#fffafa" :height 140 :family "San Francisco"))))
 '(neo-file-link-face ((t (:height 140 :family "San Francisco"))))
 '(nlinum-relative-current-face ((t (:inherit linum :background "#444444" :foreground "#c6c6c6" :weight normal))))
 '(org-block ((t (:background "#1D1124" :foreground "#D2B6F1"))))
 '(org-document-title ((t (:foreground "#E2DCCB" :weight bold :height 2.0))))
 '(org-level-1 ((t (:inherit outline-1 :height 1.3))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.1))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.1))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "DarkGoldenrod2"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "DeepPink2"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "DeepSkyBlue1")))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(css-fontify-colors nil)
 '(css-indent-offset 2)
 '(fringe-mode (quote (nil . 1)) nil (fringe))
 '(jdee-compiler (quote ("javac")))
 '(jdee-server-dir "~/myJars")
 '(neo-autorefresh t)
 '(neo-window-position (quote right))
 '(neo-window-width 30)
 '(nlinum-relative-redisplay-delay 0)
 '(nyan-animate-nyancat nil)
 '(nyan-bar-length 40)
 '(nyan-cat-face-number 1)
 '(nyan-mode t)
 '(org-agenda-files
   (quote
    ("~/todo.org" "~/Notes-in-Org-LaTeX/GNU-Emacs/gnu-emacs.org" "~/Notes-in-Org-LaTeX/Docker/docker.org")))
 '(org-format-latex-options
   (quote
    (:foreground default :background default :scale 2.0 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
                 ("begin" "$1" "$" "$$" "\\(" "\\["))))
 '(org-latex-classes
   (quote
    (("article" "\\documentclass[12pt]{article}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
     ("report" "\\documentclass[11pt]{report}"
      ("\\part{%s}" . "\\part*{%s}")
      ("\\chapter{%s}" . "\\chapter*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
     ("book" "\\documentclass[11pt]{book}"
      ("\\part{%s}" . "\\part*{%s}")
      ("\\chapter{%s}" . "\\chapter*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))))
 '(package-selected-packages
   (quote
    (lorem-ipsum dockerfile-mode evil-org rainbow-mode smex esh-autosuggest evil-magit ido-vertical-mode markdown-mode whitespace-cleanup-mode magit spacemacs-theme highlight-escape-sequences dired-icon highlight-operators highlight-numbers company-jedi emmet-mode column-enforce-mode yasnippet-snippets yasnippet-classic-snippets which-key smooth-scrolling rainbow-delimiters prettier-js pdf-tools org-bullets nyan-mode nlinum-relative neotree js2-mode jedi jdee java-snippets evil-surround evil-smartparens evil-commentary elpy dashboard base16-theme avy auto-indent-mode)))
 '(python-indent-guess-indent-offset nil)
 '(smooth-scroll-margin 2)
 '(spacemacs-theme-comment-bg nil)
 '(spacemacs-theme-comment-italic t))

(setq user-full-name "Ian Y.E. Pan")

;; Start-up
(setq initial-major-mode 'org-mode) ;; for *scratch* buffer
(setq initial-scratch-message nil)
(add-hook 'prog-mode-hook 'whitespace-cleanup-mode)
(setq frame-title-format '( "GNU Emacs @ %b" " [" (:eval mode-name) "]"))
(require 'dashboard)
(dashboard-setup-startup-hook)
(setq dashboard-banner-logo-title "Welcome, my Lord. GNU Emacs at your command.")
;; (setq dashboard-startup-banner "~/Downloads/emacs-logo.png")
(setq dashboard-startup-banner "~/Downloads/gnuemacs.png")

;; Cleaning up the interface
(setq ring-bell-function 'ignore)
(tool-bar-mode -1)
(menu-bar-mode -1)
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

;; Evil Mode
(setq evil-want-C-u-scroll t)
(require 'evil)
(evil-mode 1)
(require 'evil-surround)
(global-evil-surround-mode 1)
(evil-commentary-mode)
(define-key evil-insert-state-map (kbd "C-h") 'delete-backward-char)

;; Line Numbers
(require 'nlinum-relative)
(nlinum-relative-setup-evil)
(add-hook 'prog-mode-hook 'nlinum-relative-mode)
(setq nlinum-relative-redisplay-delay 0)
(setq nlinum-relative-current-symbol "")      ;; display current line number
(setq nlinum-relative-offset 0)

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

;; Rainbow
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'jdee-mode-hook 'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook 'rainbow-mode)

;; Flycheck
(add-hook 'after-init-hook 'global-flycheck-mode)

;; Column-80 Rule
(add-hook 'prog-mode-hook 'column-enforce-mode)
(setq column-enforce-column 79)

;; Emmet
(require 'emmet-mode)
(add-hook 'html-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook 'emmet-mode)

;; Markdown
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Org
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(global-set-key "\C-ca" 'org-agenda)  ;; Use C-c a to active agenda
(require 'evil-org-agenda)
(evil-org-agenda-set-keys)

;; Python
(elpy-enable)
(setq elpy-rpc-python-command "/usr/local/bin/python3")
(setq python-shell-interpreter "/usr/local/bin/python3")
(add-hook 'elpy-mode-hook (lambda () (highlight-indentation-mode -1)))

;; Yasnippets enable
(yas-global-mode 1)

;; Scrolling
(require 'smooth-scrolling)
(smooth-scrolling-mode 1)

;; Which-key
(require 'which-key)
(which-key-mode t)

;; Indenting
(setq-default tab-width 2)
(setq evil-shift-width 2)  ;; Using < and > to shift.
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)
(setq-default indent-tabs-mode nil)
(setq js-indent-level 2)
(setq c-default-style '((java-mode . "java") (other . "gnu")))
(defun newline-and-push-brace () "`newline-and-indent', but bracket aware."
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
(setq show-paren-delay 0)
(show-paren-mode 1)

;; In order for 'pdflatex' to work
;; Also had to export PATH from .zshrc
(setenv "PATH" (concat "/usr/texbin:/Library/TeX/texbin:" (getenv "PATH")))
(setq exec-path (append '("/usr/texbin" "/Library/TeX/texbin") exec-path))

;; No 'index.js~' back-ups
(setq make-backup-files nil)

;; Word-wrapping
(global-visual-line-mode t)

;; Mode Line
(setq-default mode-line-format
              (list
               '(:eval (propertize "  \xf0c9 %b " 'face `(:foreground "#D5A102")
                                   'help-echo (buffer-file-name)))
               '(:eval (when (buffer-modified-p)
                         (concat (propertize "\xe780 "
                                             'face 'font-lock-constant-face
                                             'help-echo
                                             "Buffer has been modified"))))
               "| "
               '(:eval (propertize "\xe799 %m" 'face 'font-lock-string-face
                                   'help-echo buffer-file-coding-system))
               " | "
               '(:eval (propertize (format-time-string "\xe384 %H:%M")
                                   'face `(:foreground "#A79FC5")
                                   'help-echo
                                   (concat (format-time-string "%c; ")
                                           (emacs-uptime "Uptime:%hh"))))
               " | "
               (propertize "\xf161 %p" 'face 'font-lock-constant-face)
               " | "
               '(:eval (list (nyan-create)))
               " | "
               "\xf0ca Line:"
               (propertize "%02l" 'face 'font-lock-type-face)
               " |"
               "%-"  ;; fill with '-'
               ))


(setq-default indicate-empty-lines t)

;; Some macOS-like keybindings
(global-set-key (kbd "s-r") 'load-file)   ;; Command + 'r' = reload file
(global-set-key (kbd "s-F") 'replace-string)   ;; Command + Shift + F = replace
(global-set-key (kbd "s-s") 'save-buffer)   ;; Command + s = save
(global-set-key (kbd "s-p") 'find-file)   ;; Command + p

;; Spell checker software Aspell (to replace ispell)
(setq ispell-program-name "/usr/local/bin/aspell")

;; Avy-easymotion
(define-key evil-normal-state-map (kbd "f") nil)
(define-key evil-normal-state-map (kbd "f") 'avy-goto-word-1)
(setq avy-keys '(?a ?s ?d ?f ?g ?h ?n ?w ?e ?r ?y
                    ?u ?o ?t ?v ?i ?j ?k ?l))

;; IDO Mode (& ido-vertical-mode)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(require 'ido-vertical-mode)
(ido-mode 1)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)
(require 'smex)
(global-set-key (kbd "M-x") 'smex)

;; Eshell
(global-set-key (kbd "<s-return>") 'eshell)
(require 'esh-autosuggest)  ;; Fish-like autosuggestion
(add-hook 'eshell-mode-hook #'esh-autosuggest-mode)
(setq eshell-prompt-function (lambda nil
                               (concat
                                "\n"
                                (propertize "\xf302 " 'face
                                            `(:foreground "#95a71a"))
                                (propertize(format-time-string "| %H:%M:%S"
                                                               (current-time))
                                           'face `(:foreground "#DF7823"))
                                (propertize " | " 'face
                                            `(:foreground "#DF7823"))
                                (propertize "\xf115 " 'face
                                            `(:foreground "#95A71A"))
                                (propertize (if (string= (eshell/pwd)
                                                         (getenv "HOME"))
                                                "~" (eshell/basename
                                                     (eshell/pwd))) 'face
                                                     `(:foreground "#95A71A"))
                                (propertize " $" 'face
                                            `(:foreground "#DF7823"))
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

(set-cursor-color "#B2B2B2")

;; Dark natural color title-bar (matching theme)
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

;; Enter new buffer in Dired and kill old one
(put 'dired-find-alternate-file 'disabled nil)
(add-hook 'dired-mode-hook (lambda () (define-key dired-mode-map (kbd "RET")
                                        'dired-find-alternate-file)))

;; Change yes/no to y/n
(fset 'yes-or-no-p 'y-or-n-p)
;; Registers
(set-register ?e (cons 'file "~/.emacs"))
(set-register ?t (cons 'file "~/todo.org"))

(setq gc-cons-threshold 100000000) ; ie 100mb, default is 800kb

;; Magit
(require 'evil-magit)


;;; .emacs ends here
