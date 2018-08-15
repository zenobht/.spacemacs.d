(setq evilNormalColor "#D2527F") 
(setq evilInsertColor "#2ABB9B")
(setq evilVisualColor "#665C7E")

(defvar my/skippable-buffers 
  '(
     "*spacemacs*" "*Messages*" "*scratch*" "*Help*" "*Warnings*"
   )
  "Buffer names ignored by `my/next-buffer' and `my/previous-buffer'.")

;; buffer changer function
(defun my/change-buffer (change-buffer)
  "Call CHANGE-BUFFER until current buffer is not in `my/skippable-buffers'."
  (let ((initial (current-buffer)))
    (funcall change-buffer)
    (let ((first-change (current-buffer)))
      (catch 'loop
        (while (member (buffer-name) my/skippable-buffers)
          (funcall change-buffer)
          (when (eq (current-buffer) first-change)
            (switch-to-buffer initial)
            (throw 'loop t)))))))

;; custom next-buffer
(defun my/next-buffer ()
  "Variant of `next-buffer' that skips `my/skippable-buffers'."
  (interactive)
  (my/change-buffer 'next-buffer))

;; custom previous-buffer
(defun my/previous-buffer ()
  "Variant of `previous-buffer' that skips `my/skippable-buffers'."
  (interactive)
  (my/change-buffer 'previous-buffer))

(defun my/evil-paste-after-from-0 ()
  (interactive)
  (let ((evil-this-register ?0))
    (call-interactively 'evil-paste-after)))


(defun my/escape-and-save ()
  (interactive)
  (evil-escape)
  (save-buffer)
  )

(defun my/run-projectile-invalidate-cache (&rest _args)
  ;; We ignore the args to `magit-checkout'.
  (projectile-invalidate-cache nil))


(defun my/enable-visual-line-navigation ()
  "enables visual-line-navigation which is word-wrap"
  (spacemacs/toggle-visual-line-navigation)
  )


(defun my/newline-and-indent ()
  "inserts a newline between the brackets"
  (interactive)
  (newline)
  (save-excursion
    (newline)
    (indent-for-tab-command))
  (indent-for-tab-command)
  )


(defun my/scroll-half-page-down ()
  "scroll down half the page"
  (interactive)
  (scroll-down (/ (window-body-height) 2)))

;; scroll half page up
(defun my/scroll-half-page-up ()
  "scroll up half the page"
  (interactive)
  (scroll-up (/ (window-body-height) 2)))


(defun my/modify-underscore-syntax () 
  (modify-syntax-entry ?_ "w" (syntax-table)))

(defun my/block-comment-setup ()
  "Configure multi-line comments."
  (setq comment-start       "// "
        comment-multi-line  t
        comment-padding     nil
        comment-style       'extra-line
        comment-continue    " * "
        comment-empty-lines t))


(defun my/indent-setup (n)
  ;; java/c/c++
  (setq c-basic-offset n)
  ;; web development
  (setq coffee-tab-width n) ; coffeescript
  (setq javascript-indent-level n) ; javascript-mode
  (setq js-indent-level n) ; js-mode
  (setq js2-basic-offset n) ; js2-mode, in latest js2-mode, it's alias of js-indent-level
  (setq web-mode-markup-indent-offset n) ; web-mode, html tag in html file
  (setq web-mode-css-indent-offset n) ; web-mode, css in html file
  (setq web-mode-code-indent-offset n) ; web-mode, js code in html file
  (setq css-indent-offset n) ; css-mode
  ) 

(defun my/js-symbols-setup () 
  (interactive)
  (mapc (lambda (pair) (push pair prettify-symbols-alist))
        '(
          ("=>" . ?⇒)
          )))

(defun my/react-mode-setup ()
  (prettier-js-mode t)
  ;; (my/js-symbols-setup)
  (my/block-comment-setup)
  (flycheck-mode)
  )

(setq tab-always-indent t)

(defun my/eslint-setup ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(defun my/org-heading-setup ()
  "Stop the org-level headers from increasing in height relative to the other text."
  (dolist (face '(
                  org-level-1
                  org-level-2
                  org-level-3
                  org-level-4
                  org-level-5
                  org-level-6
                  org-level-7
                  org-level-8))
    (set-face-attribute face nil :height 1.2)))

(use-package org
  :ensure t
  :diminish org-mode
  :bind 
  :config
  (progn 

    (setq org-todo-keywords
          (quote ((sequence "☛ TODO(t)" "➜ STARTED(s!)" "|" "✓ DONE(d@)")
                  (sequence "⚑ WAITING(w)" "|")
                  (sequence "|" "✘ CANCELED(c)"))))

    (setq org-todo-keyword-faces
          (quote (("☛ TODO" :foreground "#FFCACA" :weight bold)
                  ("➜ STARTED" :foreground "#CADAFF" :weight bold)
                  ("✓ DONE" :foreground "#CAFFE9" :weight bold)
                  ("⚑ WAITING" :foreground "#FFE2CA" :weight bold)
                  ("✘ CANCELED" :foreground "#FFCAF9" :weight bold)))) 

    ;; set up org-bullet symbols
    (setq org-bullets-bullet-list '("◒" "◐" "◓" "◑" ))

    (setq org-enforce-todo-dependencies t)
    (setq org-agenda-dim-blocked-tasks t)

    (setq org-clock-persist 'history)
    (org-clock-persistence-insinuate)

    ;; enable pretty entities by default in org-mode 
    (add-hook 'org-mode-hook (setq org-pretty-entities t))

    ;; modify org heading in org-mode
    ;; (add-hook 'org-mode-hook 'my/org-heading-setup)

    (evil-define-key 'normal org-mode-map ",v" 'org-todo)
    (evil-define-key 'insert org-mode-map (kbd "s-<return>") 'org-insert-item)

    ))

;; (use-package spaceline
;;    :ensure t)

(use-package spaceline
  :ensure t
  :config
  (progn 

    ;;spaceline-all-the-icons setup
    (setq spaceline-all-the-icons-clock-always-visible nil)
    (setq spaceline-all-the-icons-eyebrowse-display-name nil)
    (setq spaceline-all-the-icons-flycheck-alternate t)
    (setq spaceline-all-the-icons-hide-long-buffer-path t)
    (setq spaceline-all-the-icons-slim-render t)
    ;; (spaceline-toggle-all-the-icons-eyebrowse-workspace-off)

    ;;spaceline/modeline segment config
    (spaceline-toggle-point-position-on)
    (spaceline-toggle-process-off)
    (spaceline-toggle-buffer-encoding-off)
    (spaceline-toggle-buffer-encoding-abbrev-off)
    (spaceline-toggle-purpose-off)
    (spaceline-toggle-minor-modes-off)
    (spaceline-toggle-persp-name-off)
    (setq display-time-default-load-average nil)


    (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)

    (set-face-attribute
     'spaceline-evil-normal nil :background evilNormalColor :foreground "black")
    (set-face-attribute
     'spaceline-evil-motion nil :background evilNormalColor :foreground "black")
    (set-face-attribute
     'spaceline-evil-visual nil :background evilVisualColor :foreground "white")
    (set-face-attribute
     'spaceline-evil-insert nil :background evilInsertColor :foreground "black")
   ))

(use-package yasnippet
  :ensure t
  :config
  (progn 

    (setq yas-snippet-dirs
          '("~/.spacemacs.d/snippets" 
            ))
    (yas-global-mode 1) 
    (setq yas/indent-line nil) 
    ))

(use-package rainbow-mode
  :ensure t
  :config
  (progn 

    (dolist (hook 
             '(prog-mode-hook text-mode-hook react-mode-hook web-mode-hook))
      (add-hook hook 'rainbow-mode))

    ))

(use-package flycheck
  :ensure t
  :config
  (progn 

    ;; flycheck enabled by default
    (add-hook 'after-init-hook #'global-flycheck-mode)
    (setq flycheck-check-syntax-automatically '(mode-enabled save))
    (setq-default flycheck-disabled-checkers
                  (append flycheck-disabled-checkers
                          '(javascript-jshint)))


    (flycheck-add-mode 'javascript-eslint 'react-mode)
    (add-hook 'flycheck-mode-hook #'my/eslint-setup)

    ))

(use-package projectile
  :ensure t
  :config
  (progn 

    (setq projectile-indexing-method 'alien)
    (setq projectile-enable-caching t)

    (add-hook 'projectile-after-switch-project-hook #'setup-project-paths)

    ;; invalidates projectile cache on git actions
    (advice-add 'magit-checkout
                :after #'my/run-projectile-invalidate-cache)
    (advice-add 'magit-branch-and-checkout ; This is `b c'.
                :after #'my/run-projectile-invalidate-cache)

   (setq magit-bury-buffer-function 'magit-mode-quit-window)

    ))

(use-package ivy
  :ensure t
  :bind 
  :config
  (progn 

    ;; ivy config
    (setq ivy-re-builders-alist
          '(
            (counsel-M-x . ivy--regex-plus)
            (swiper . ivy--regex-plus)
            (t . ivy--regex-fuzzy)))
    (add-to-list 'ivy-highlight-functions-alist
                 '(swiper--re-builder . ivy--highlight-ignore-order))

    (setq dumb-jump-selector 'ivy)

    ))

(use-package dired-x
  :config
  (progn
    (setq dired-omit-verbose nil)
    (add-hook 'dired-mode-hook #'dired-omit-mode)
    (setq dired-omit-files
          (concat dired-omit-files "\\|^.DS_STORE$\\|^.projectile$"))))

(require 'dired+)
(toggle-diredp-find-file-reuse-dir 1)
(evil-define-key 'normal dired-mode-map
  (kbd "h") 'diredp-up-directory-reuse-dir-buffer
  (kbd "l") 'dired-find-alternate-file
  (kbd "<escape>") 'kill-this-buffer
  (kbd "q") 'kill-this-buffer)

;; setup encoding
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(use-package wgrep
  :ensure t
  :config
  (progn 

    ;; wgrep binding to save all buffers after edit
    (setq wgrep-auto-save-buffer t)

    ))

(use-package treemacs
  :ensure t
  :config
  (progn 
    (spacemacs/set-leader-keys "ft" #'treemacs)
    (setq treemacs-show-hidden-files t)
    (setq-default treemacs-width 30)))

(with-eval-after-load 'treemacs
  (defun treemacs-ignore-gitignore (file _)
    (string= file ".DS_Store"))
  (push #'treemacs-ignore-gitignore treemacs-ignored-file-predicates))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config 
  (progn

    (setq markdown-command "multimarkdown")
    ;; (prettier-js-mode t)

    ))

(use-package pcre2el
  :ensure t
  :config
  (pcre-mode)
  )

(use-package evil-multiedit
  :ensure t
  :config
  (progn

    (define-key evil-normal-state-map (kbd "M-d") 'evil-multiedit-match-and-next)
    (define-key evil-normal-state-map (kbd "M-D") 'evil-multiedit-match-and-prev)
    (define-key evil-normal-state-map (kbd "M-T") 'evil-multiedit-toggle-or-restrict-region)
    (define-key evil-normal-state-map (kbd "M-A") 'evil-multiedit-match-all)

    ;; Ex command that allows you to invoke evil-multiedit with a regular expression, e.g.
    (evil-ex-define-cmd "ie[dit]" 'evil-multiedit-ex-match)

    ))

(use-package dumb-jump
  :ensure t
  :config
  (progn 

    ;; dumb jump config set to SPC d
    (spacemacs/set-leader-keys "dj" #'dumb-jump-go)
    (spacemacs/set-leader-keys "dq" #'dumb-jump-quick-look)
    (spacemacs/set-leader-keys "db" #'dumb-jump-back)

    ))

(use-package kotlin-mode 
  :ensure t
  :config
  ( progn 
    ;; (require 'flycheck-kotlin)
    ;; (flycheck-mode t)
    ;; (flycheck-kotlin-setup)
    (setq kotlin-tab-width 2)))

(use-package groovy-mode 
  :ensure t
  :config
  ( progn 
    (setq groovy-indent-offset 2)))

(setq js2-strict-missing-semi-warning nil) ;; semi-colon warnings not shown
(setq js2-strict-trailing-comma-warning nil) ;; trailing comma warnings not shown
(my/indent-setup 2)

;; react-mode setup
 (add-to-list 'magic-mode-alist '("\\(import.*from \'react\';\\|\/\/ @flow\nimport.*from \'react\';\\)" . rjsx-mode))
;;(add-to-list 'magic-mode-alist '("import React" . react-mode))
(add-hook 'react-mode-hook #'my/react-mode-setup)
(add-hook 'rjsx-mode-hook #'my/react-mode-setup)

;; js2-mode setup
(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
;;(add-hook 'js2-mode-hook #'my/react-mode-setup)

;; json-mode setup
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))
(add-hook 'json-mode-hook 'prettier-js-mode)

;; css-mode setup
(add-hook 'css-mode-hook 'prettier-js-mode)
(setq css-fontify-colors nil)

;;(add-hook 'web-mode-hook 'prettier-js-mode)

(add-hook 'java-mode-hook 'my/block-comment-setup)

(defun my/python-indent ()
  (interactive)
  (setq-default indent-tabs-mode nil
                tab-width 2)
  )

(venv-initialize-interactive-shells) ;; if you want interactive shell support
(venv-initialize-eshell)

(add-hook 'python-mode-hook #'my/python-indent)

(use-package dockerfile-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("Dockerfile\\'" . dockerfile-mode))
  )

(use-package company
  :ensure t
  :init (global-company-mode)
  )

(use-package web-mode
  :ensure t
  :bind (:map web-mode-map  
              ("s-;" . nil)))

(use-package evil
  :ensure t
  :bind (:map evil-normal-state-map
              ("C-S-l" . evil-jump-forward)
              ("C-S-h" . evil-jump-backward)
              ("C-S-j" . move-text-down)
              ("C-S-k" . move-text-up)
              ("C-b" . evil-scroll-page-up)
              ("C-f" . evil-scroll-page-down)
              ("s-{" . my/next-buffer)
              ("s-}" . my/previous-buffer) 
              ("M-d" . evil-multiedit-match-and-next)
              ("M-D" . evil-multiedit-match-and-prev)
              :map evil-visual-state-map
              ("R" . evil-multiedit-match-all)
              ("p" . my/evil-paste-after-from-0)
              ("C-S-j" . drag-stuff-down)
              ("C-S-k" . drag-stuff-up)
              :map evil-insert-state-map
              ("C-d" . nil)
              ("M-d" . evil-multiedit-match-and-next)
              ("M-D" . evil-multiedit-match-and-prev)
              ("C-M-D" . evil-multiedit-restore)
              )
  :config
  (progn 

    ;; default cursor as bar 
    (setq-default cursor-type '(bar . 3))
    (setq evil-normal-state-cursor `(box ,evilNormalColor)) 
    (setq evil-insert-state-cursor `((bar . 3) ,evilInsertColor)) 
    (setq evil-evilified-state-cursor '((bar . 3) "LightGoldenrod3")) 
    (setq evil-emacs-state-cursor '((bar . 3) "SkyBlue2")) 
    (setq evil-motion-state-cursor `((bar . 3) "HotPink1")) 
    (setq evil-lisp-state-cursor '((bar . 3) "HotPink1")) 

    ;;(setq evil-move-cursor-back nil)

    (evil-leader/set-key
      "jj" 'evil-avy-goto-char-2
      "jJ" 'evil-avy-goto-char
      "od" 'make-directory
      "om" 'markdown-mode
      "oo" 'org-mode
      "os" 'just-one-space
      "ot" 'text-mode
      "si" 'counsel-grep-or-swiper
      ) 

    ))

;; disable few default key bindings
(dolist (key '("C-a" "C-e" "s-H" "s-h" "s-L" "s-e"))
  (global-unset-key (kbd key)))

;; bind cmd+arrow keys to behave the same way as rest of the osx
(global-set-key (kbd "s-<left>") 'move-beginning-of-line)
(global-set-key (kbd "s-<right>") 'move-end-of-line)
(global-set-key (kbd "s-<up>") 'beginning-of-buffer)
(global-set-key (kbd "s-<down>") 'end-of-buffer)

;; treat _ as word
(add-hook 'prog-mode-hook 'my/modify-underscore-syntax)
(add-hook 'text-mode-hook 'my/modify-underscore-syntax)

 (setq-default line-spacing 2)

(add-hook 'text-mode-hook 'auto-fill-mode)
(remove-hook 'prog-mode-hook 'spacemacs//show-trailing-whitespace)
(add-hook 'web-mode-hook (lambda () (flycheck-mode -1)))

(global-hl-line-mode +1)
(show-paren-mode +1)
(global-visual-line-mode nil)

(electric-pair-mode 1)
(push '(?\' . ?\') electric-pair-pairs)
(global-evil-matchit-mode t)

(setq package-check-signature nil)
(setq frame-resize-pixelwise t)
(setq case-fold-search nil)
(setq evil-ex-search-case nil)

(setq-default evil-escape-delay 0.2)

;; visual-line-mode for all text-modes
(add-hook 'text-mode-hook #'my/enable-visual-line-navigation)

;; company-tern property marker
(setq company-tern-property-marker " =>")

;; enable symbols by default
(global-prettify-symbols-mode -1)

;; global move visual block up/down: life-saver
(drag-stuff-global-mode 1)

(setq zeno-theme-enable-italics t)
(spacemacs/load-theme 'zeno)

;; scale text
(define-key global-map (kbd "s-+") 'text-scale-increase)
(define-key global-map (kbd "s--") 'text-scale-decrease)
(define-key global-map (kbd "s-0") (lambda () (interactive) (text-scale-set 0)))

;; key-binding to insert new line between brackets and indent
 (global-set-key (kbd "s-i") 'my/newline-and-indent)

;; remap next-buffer to custom buffer functions
(global-set-key [remap next-buffer] 'my/next-buffer)
(global-set-key [remap previous-buffer] 'my/previous-buffer)

;; as spacemacs is running as daemon, binding qq to kill frame
(spacemacs/set-leader-keys "qq" #'spacemacs/frame-killer)

;; bind snippet expand to s-y
(global-set-key [?\C-y] 'hippie-expand)
(global-set-key [?\C-\s-y] 'dabbrev-completion)

;; (setq-default indent-tabs-mode nil)
(setq standard-indent 2)
(setq tab-width 2)

;; beautify titlebar of emacs :heart-eyes:
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . 'nil))

(spacemacs|diminish drag-stuff-mode " dr")
(spacemacs|diminish emoji-cheat-sheet-plus-display-mode " EM")
(spacemacs|diminish prettier-mode " PR")

;; set symbola font to be used for all unicode symbols
;; other than 
(set-fontset-font "fontset-default" '(#x00C1 . #x2648) "Symbola-12") 

;; native pixel scroll mode
(pixel-scroll-mode t)

;; (setq scroll-step           1
;;       scroll-conservatively 10000)

(setq frame-title-format 
      '((:eval (spacemacs/title-prepare dotspacemacs-frame-title-format))))

;; use font awesome folder icon
(set-fontset-font t '(#Xf07c . #Xf07c) "fontawesome")

;; required to kill customize buffers on pressing q
(setq custom-buffer-done-kill t)

(setq sh-basic-offset 2)
(setq sh-indentation 2)

;; (when (window-system)
;;   (set-frame-font "Fira Code"))
;; (let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
;;                (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
;;                (36 . ".\\(?:>\\)")
;;                (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
;;                (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
;;                (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
;;                (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
;;                (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
;;                (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
;;                (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
;;                (48 . ".\\(?:x[a-zA-Z]\\)")
;;                (58 . ".\\(?:::\\|[:=]\\)")
;;                (59 . ".\\(?:;;\\|;\\)")
;;                (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
;;                (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
;;                (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
;;                (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
;;                (91 . ".\\(?:]\\)")
;;                (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
;;                (94 . ".\\(?:=\\)")
;;                (119 . ".\\(?:ww\\)")
;;                (123 . ".\\(?:-\\)")
;;                (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
;;                (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
;;                )
;;              ))
;;   (dolist (char-regexp alist)
;;     (set-char-table-range composition-function-table (car char-regexp)
;;                           `([,(cdr char-regexp) 0 font-shape-gstring]))))


;; (setq display-line-numbers-grow-only t)
;; (setq display-line-numbers-widen t)
;; (setq display-line-numbers-type 'visual)
;; (setq display-line-numbers-width 2)
;; (setq display-line-numbers-current-absolute nil)
;; (dolist (hook 
;;          '(prog-mode-hook text-mode-hook))
;;   (add-hook hook 'display-line-numbers-mode))

;; handle long line scrolling with so-long
(when (require 'so-long nil :noerror)
  (so-long-enable))
