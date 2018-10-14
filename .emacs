;;; .emacs --- Custom Emacs Configuration
;; Author: Ian Y.E. Pan
;;; Commentary:
;;; Code:
(require 'package)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'load-path "~/.emacs.d/evil")
(setq package-enable-at-startup nil)

;; Load theme by hand, don't use 'M-x customize-themes'
(load-theme 'doom-city-lights t)
(add-hook 'prog-mode-hook 'highlight-numbers-mode)
(add-hook 'prog-mode-hook 'highlight-operators-mode)
(hes-mode)  ;; highlight escape sequences

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "Black" :foreground "#6A8397" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 150 :width normal :foundry "nil" :family "Monaco"))))
 '(avy-lead-face ((t (:foreground "#FF4E4e"))))
 '(avy-lead-face-0 ((t (:foreground "DarkGoldenrod2"))))
 '(bold ((t (:weight normal :foreground "DarkGoldenrod2"))))
 '(column-enforce-face ((t (:foreground "#EC655D" :underline t))))
 '(company-scrollbar-bg ((t (:background "#000000"))))
 '(company-scrollbar-fg ((t (:background "#aaaaaa"))))
 '(company-template-field ((t (:background "#333333" :foreground "#bbbbbb"))))
 '(company-tooltip ((t (:background "#333333" :foreground "#bbbbbb"))))
 '(company-tooltip-selection ((t (:background "#666666"))))
 '(dashboard-banner-logo-title-face ((t (:inherit default :overline t :height 1.15 :family "Monaco"))))
 '(dashboard-heading-face ((t (:inherit default :foreground "#CBEBFF" :height 1.1))))
 '(eshell-git-prompt-powerline-clean-face ((t (:background "#49A33A" :foreground "#fffafa"))))
 '(eshell-git-prompt-powerline-dir-face ((t (:background "steel blue" :foreground "#fffafa"))))
 '(eshell-git-prompt-powerline-not-clean-face ((t (:background "DarkGoldenrod2" :foreground "#fffafa"))))
 '(font-lock-negation-char-face ((t (:inherit font-lock-preprocessor-face))))
 '(fringe ((t (:background "#000000"))))
 '(highlight ((t (:background "#2a2a2a"))))
 '(linum ((t (:background "#000000" :foreground "#455562"))))
 '(neo-dir-link-face ((t (:foreground "#fffafa" :height 140 :family "San Francisco"))))
 '(neo-file-link-face ((t (:height 140 :family "San Francisco"))))
 '(nlinum-relative-current-face ((t (:inherit linum :background "#000000" :foreground "#c6c6c6" :weight normal))))
 '(org-block ((t (:background "#263038" :foreground "#839DB2"))))
 '(org-document-title ((t (:height 2.0 :family "Georgia"))))
 '(org-level-1 ((t (:inherit outline-1 :weight bold :foreground "#59B1FF" :height 1.3))))
 '(org-level-2 ((t (:inherit outline-2 :weight bold :foreground "#F26749" :height 1.1))))
 '(org-level-3 ((t (:inherit outline-3 :weight bold :foreground "#686BC7" :height 1.1))))
 '(org-table ((t (:background "#002831"))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "#80E3E2"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "#6BB9FE"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#B5DEFF"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "#80E3E2"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "#6BB9FE"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "#B5DEFF"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "#80E3E2"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "#6BB9FE"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "#B5DEFF")))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(all-the-icons-scale-factor 1.0)
 '(column-enforce-column 79)
 '(company-cmake-executable nil)
 '(company-global-modes '(not eshell-mode))
 '(company-idle-delay t)
 '(css-fontify-colors nil)
 '(css-indent-offset 2)
 '(dired-sidebar-width 35)
 '(dired-use-ls-dired nil)
 '(display-time-24hr-format t)
 '(display-time-day-and-date t)
 '(display-time-interval 1)
 '(display-time-mode t)
 '(fringe-mode '(nil . 1) nil (fringe))
 '(global-company-mode t)
 '(jdee-compiler '("javac"))
 '(jdee-server-dir "~/myJars")
 '(js-indent-level 2 t)
 '(js2-strict-missing-semi-warning nil)
 '(moody-mode-line-height 21)
 '(neo-autorefresh t)
 '(neo-window-position 'right)
 '(neo-window-width 30)
 '(nlinum-relative-redisplay-delay 0)
 '(olivetti-body-width 80)
 '(org-agenda-files
   '("~/todo.org" "~/Notes-in-Org-LaTeX/GNU-Emacs/gnu-emacs.org" "~/Notes-in-Org-LaTeX/Docker/docker.org"))
 '(org-format-latex-options
   '(:foreground default :background default :scale 2.0 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
                 ("begin" "$1" "$" "$$" "\\(" "\\[")))
 '(org-highlight-latex-and-related '(latex))
 '(org-latex-classes
   '(("article" "\\documentclass[12pt, a4paper]{article}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))
 '(package-selected-packages
   '(doom-themes writeroom-mode eshell-git-prompt vscode-icon dired-sidebar tide all-the-icons minions moody edit-server flx-ido vimrc-mode lorem-ipsum dockerfile-mode evil-org rainbow-mode smex esh-autosuggest evil-magit ido-vertical-mode markdown-mode whitespace-cleanup-mode magit spacemacs-theme highlight-escape-sequences dired-icon highlight-operators highlight-numbers company-jedi emmet-mode column-enforce-mode yasnippet-snippets yasnippet-classic-snippets which-key smooth-scrolling rainbow-delimiters prettier-js org-bullets nlinum-relative neotree js2-mode jedi jdee java-snippets evil-surround evil-smartparens evil-commentary elpy dashboard base16-theme avy auto-indent-mode))
 '(python-indent-guess-indent-offset nil)
 '(smooth-scroll-margin 2)
 '(writeroom-global-effects
   '(writeroom-set-fullscreen writeroom-set-alpha writeroom-set-menu-bar-lines writeroom-set-tool-bar-lines writeroom-set-bottom-divider-width))
 '(writeroom-mode-line t))

;; Start-up
(setq user-full-name "Ian Y.E. Pan")
(global-set-key (kbd "C-s-f") 'toggle-frame-fullscreen)  ;; Ctrl-Cmd-f to toggle fullscreen
(setq initial-scratch-message nil)
(add-hook 'prog-mode-hook 'whitespace-cleanup-mode)
(setq frame-title-format '( "GNU Emacs @ %b" " [" (:eval mode-name) "]"))
(require 'dashboard)
(dashboard-setup-startup-hook)
(setq dashboard-banner-logo-title "Welcome, my Lord. GNU Emacs at your command.")
(setq dashboard-startup-banner "~/Downloads/gnuemacs.png")
(set-face-attribute 'font-lock-comment-face nil :foreground "#CBEBFF" :background nil :slant 'italic)  ;; Italic comments
(setq split-height-threshold nil)  ;; always split windows side by side
(setq split-width-threshold 0)
;; Dark natural color title-bar (matching theme)
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

;; Cleaning up the interface
(setq ring-bell-function 'ignore)
(tool-bar-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode 0)

;; *scratch* buffer always exists
(setq initial-major-mode 'org-mode) ;; for *scratch* buffer
(with-current-buffer
    (get-buffer-create "*scratch*") (org-mode)
    (make-local-variable 'kill-buffer-query-functions)
    (add-hook 'kill-buffer-query-functions 'kill-scratch-buffer))
(defun kill-scratch-buffer ()
  (set-buffer (get-buffer-create "*scratch*"))
  (remove-hook 'kill-buffer-query-functions 'kill-scratch-buffer)
  (kill-buffer (current-buffer))
  (set-buffer (get-buffer-create "*scratch*")) (org-mode)
  (make-local-variable 'kill-buffer-query-functions)
  (add-hook 'kill-buffer-query-functions 'kill-scratch-buffer) nil)

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

;; God Mode within Evil (better visual-line and insert-mode nagivation)
(define-key evil-normal-state-map "\C-n" 'next-line)
(define-key evil-insert-state-map "\C-n" 'next-line)
(define-key evil-visual-state-map "\C-n" 'next-line)

(define-key evil-normal-state-map "\C-p" 'previous-line)
(define-key evil-insert-state-map "\C-p" 'previous-line)
(define-key evil-visual-state-map "\C-p" 'previous-line)

(define-key evil-normal-state-map "\C-f" 'forward-char)
(define-key evil-insert-state-map "\C-f" 'forward-char)
(define-key evil-insert-state-map "\C-f" 'forward-char)

(define-key evil-normal-state-map "\C-b" 'backward-char)
(define-key evil-insert-state-map "\C-b" 'backward-char)
(define-key evil-visual-state-map "\C-b" 'backward-char)

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
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

;; Rainbow brackets and rainbow color highlight
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'jdee-mode-hook 'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook 'rainbow-mode)

;; Flycheck
(add-hook 'after-init-hook 'global-flycheck-mode)

;; Column-80 Rule
(add-hook 'prog-mode-hook 'column-enforce-mode)
(setq column-enforce-column 79)

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
(setq org-todo-keywords
      '((sequence "TODO" "DOING" "DONE")))
(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "#E74E22" :weight bold))
        ("DOING" . (:foreground "DarkGoldenrod2" :weight bold))
        ("DONE" . (:foreground "#83E230" :weight bold))))
(add-hook 'org-mode-hook 'writeroom-mode)
(global-set-key (kbd "C-c w") 'writeroom-mode) ;; Toggle writeroom

;; Python
(elpy-enable)
(setq elpy-rpc-python-command "/usr/local/bin/python3")
(setq python-shell-interpreter "/usr/local/bin/python3")
(add-hook 'elpy-mode-hook (lambda () (highlight-indentation-mode -1)))
(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))  ;; company-jedi
(add-hook 'python-mode-hook 'my/python-mode-hook)

;; Yasnippets
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

;; Syntax Highlight
(global-font-lock-mode t)

;; Vimrc Mode
(require 'vimrc-mode)
(add-to-list 'auto-mode-alist '("\\.vim\\(rc\\)?\\'" . vimrc-mode))

;; Smart Parenthesis
(smartparens-global-mode 1)
(add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)
(setq show-paren-delay 0)
(show-paren-mode 1)

;; In order for 'pdflatex' to work. Also had to export PATH from .zshrc
(setenv "PATH" (concat "/usr/texbin:/Library/TeX/texbin:" (getenv "PATH")))
(setq exec-path (append '("/usr/texbin" "/Library/TeX/texbin") exec-path))

;; No 'index.js~' back-ups
(setq make-backup-files nil)

;; Word-wrapping and indicating empty lines
(global-visual-line-mode t)
(setq-default indicate-empty-lines t)

;; Some macOS-like keybindings
(global-set-key (kbd "s-s") 'save-buffer)   ;; Cmd + s = save
(global-set-key (kbd "s-r") 'load-file)   ;; Cmd + 'r' = reload file
(global-set-key (kbd "s-F") 'replace-string)   ;; Cmd + Shift + F = replace
(global-set-key (kbd "s-P") 'smex)   ;; Cmd + Shift + P
(global-set-key (kbd "s-x") 'smex)   ;; Cmd + x for (M-x)
(global-set-key (kbd "s-k") 'ido-kill-buffer)   ;; Cmd + k
(global-set-key (kbd "s-K") 'kill-some-buffers)   ;; Cmd + Shift + K
(global-set-key (kbd "s-q") 'save-buffers-kill-terminal)   ;; Cmd + q

;; Spell checker software Aspell (to replace ispell)
(setq ispell-program-name "/usr/local/bin/aspell")

;; Avy-easymotion
(define-key evil-normal-state-map (kbd "f") nil)
(define-key evil-normal-state-map (kbd "f") 'avy-goto-word-1)
(setq avy-keys '(?a ?s ?d ?f ?g ?h ?n ?w ?e ?r ?y ?u ?o ?t ?v ?i ?j ?k ?l))

;; Ido & Ido-Vertical & Flx-Ido
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(require 'ido-vertical-mode)
(ido-mode 1)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)
(require 'flx-ido)
(flx-ido-mode 1)
(setq ido-enable-flex-matching t)

;; SMEX
(require 'smex)
(global-set-key (kbd "M-x") 'smex)

;; Eshell
(global-set-key (kbd "<s-return>") 'eshell)
(require 'esh-autosuggest)  ;; Fish-like autosuggestion
(add-hook 'eshell-mode-hook #'esh-autosuggest-mode)
(eshell-git-prompt-use-theme 'powerline)

(defun eshell/clear ()
  "Clear the eshell buffer to the top."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))
(global-set-key (kbd "C-8") 'eshell-previous-input)
(global-set-key (kbd "C-9") 'eshell-next-input)

;; To let Eshell use brew-installed commands
(setenv "PATH" (concat "/usr/local/bin/" ":" (getenv "PATH")))
(setq exec-path (append '("/usr/local/bin/") exec-path))
;; Eshell aliases
(defalias 'ff 'find-file)

;; Emmet
(require 'emmet-mode)
(add-hook 'html-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook 'emmet-mode)
(add-hook 'js2-mode-hook 'emmet-mode)

;; JavaScript
;; (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-jsx-mode))
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
(require 'prettier-js)
(setq prettier-js-args '(
                         "--trailing-comma" "all"
                         "--bracket-spacing" "true"
                         "--jsx-bracket-same-line" "true"))

(set-cursor-color "#D7F0FF")

;; Dired
(put 'dired-find-alternate-file 'disabled nil)
(add-hook 'dired-mode-hook (lambda () (define-key dired-mode-map (kbd "RET")
                                        'dired-find-alternate-file)))

;; Change yes/no to y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; Registers
(set-register ?e (cons 'file "~/.emacs.d/init.el"))
(set-register ?t (cons 'file "~/todo.org"))

(setq gc-cons-threshold 20000000) ; ie 100mb, default is 800kb

;; Magit
(require 'evil-magit)
(global-set-key (kbd "C-x g") 'magit)

;; Mode Line
(require 'moody)
(moody-replace-mode-line-buffer-identification)
(moody-replace-vc-mode)
(let ((line (face-attribute 'mode-line :underline)))
  (set-face-attribute 'mode-line          nil :foreground   "#CBECFF")
  (set-face-attribute 'mode-line          nil :background   "#516675")
  (set-face-attribute 'mode-line          nil :overline   nil)
  (set-face-attribute 'mode-line-inactive nil :overline   nil)
  (set-face-attribute 'mode-line-inactive nil :underline  nil)
  (set-face-attribute 'mode-line          nil :box        nil)
  (set-face-attribute 'mode-line-inactive nil :box        nil))
(minions-mode)


;; Comparing files
(global-set-key (kbd "C-c d") 'diff)
(global-set-key (kbd "C-c e") 'ediff)
(global-set-key (kbd "C-c D") 'diff-buffer-with-file)
(global-set-key (kbd "C-c E") 'ediff-current-file)
(eval-after-load 'diff-mode
  '(progn
     (set-face-background 'diff-added "#355531")
     (set-face-foreground 'diff-added "#dcffdd")
     (set-face-background 'diff-removed "#553333")
     (set-face-foreground 'diff-removed "#ffdddc")))

;; Transparency
(defun toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ((numberp (cadr alpha)) (cadr alpha))) 100)
         '(75 . 75) '(100 . 100)))))
(global-set-key (kbd "C-c t") 'toggle-transparency)


;; Toggle between horizontal and vertical view
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))
(global-set-key (kbd "C-x 4 5") 'toggle-window-split)


;; Dired Sidebar
(global-set-key (kbd "s-d") 'dired-sidebar-toggle-sidebar)

;; Colourful Org LaTeX Code Blocks
(require 'ox-latex)
(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted)
(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
(setq org-latex-minted-options '(("linenos=true")))


(provide 'init.el)
;;; init.el ends here
