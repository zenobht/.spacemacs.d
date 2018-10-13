(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   '(
     (auto-completion :variables
                      auto-completion-return-key-behavior 'complete
                      auto-completion-tab-key-behavior 'complete
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-enable-sort-by-usage t)
     (better-defaults :variables
                      better-defaults-move-to-beginning-of-code-first t
                      better-defaults-move-to-end-of-code-first nil)
     colors
     csv
     emacs-lisp
     emoji
     evil-commentary
     (evil-snipe :variables evil-snipe-enable-alternate-f-and-t-behaviors t)
     git
     groovy
     html
     (ivy :variables
          ivy-wrap t
          ivy-height 15
          ivy-use-virtual-buffers nil
          )
     java
     (javascript :variables javascript-disable-tern-port-files t)
     kotlin
     lua
     (markdown :variables
               markdown-command 'pandoc
               markdown-live-preview-engine 'vmd)
     (org :variables
          org-projectile-file "TODOs.org"
          org-enable-github-support t)
     (osx :variables
          osx-command-as 'super)
     pandoc
     php
     (python :variables
             python-sort-imports-on-save t
             python-enable-yapf-format-on-save t)
     react
     ruby
     (shell :variables
            shell-default-shell 'eshell
            shel-default-width  50
            shell-default-term-shell "/bin/zsh"
            shell-default-position 'right)
     spacemacs-org
     (spell-checking :variables spell-checking-enable-by-default nil)
     (syntax-checking :variables syntax-checking-enable-by-default t)
     tern
     (version-control :variables
                      version-control-diff-tool 'git-gutter
                      version-control-diff-side 'left
                      version-control-global-margin t)
     vimscript
     yaml
     )
   dotspacemacs-additional-packages '(
                                      all-the-icons
                                      dart-mode
                                      dockerfile-mode
                                      dracula-theme
                                      drag-stuff
                                      evil-multiedit
                                      exotica-theme
                                      molokai-theme
                                      org-mime
                                      pcre2el
                                      pretty-mode
                                      shrink-path
                                      subatomic-theme
                                      virtualenvwrapper
                                      vue-mode
                                      zeno-theme
                                      )
   dotspacemacs-frozen-packages '()
   dotspacemacs-excluded-packages '(
                                    evil-escape
                                    evil-goggles
                                    linum-relative
                                    smartparens
                                    vi-tilde-fringe
                                    )
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  (setq-default
   dotspacemacs-enable-emacs-pdumper nil
   dotspacemacs-emacs-pdumper-executable-file "emacs-27.0.50"
   dotspacemacs-emacs-dumper-dump-file "spacemacs.pdmp"
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 30
   dotspacemacs-gc-cons '(100000000 0.1)
   dotspacemacs-use-spacelpa nil
   dotspacemacs-verify-spacelpa-archives nil
   dotspacemacs-check-for-update nil
   dotspacemacs-elpa-subdirectory 'emacs-version
   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner 'official
   dotspacemacs-startup-lists nil
   dotspacemacs-startup-buffer-responsive t 
   dotspacemacs-scratch-mode 'text-mode
   dotspacemacs-themes '(
                         zeno
                         exotica
                         spacemacs-dark
                         molokai
                         subatomic
                         )
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Fira Code Mod"
                               :size 13
                               :width normal
                               :weight normal)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-command-key "SPC"
   dotspacemacs-ex-command-key ":"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-distinguish-gui-tab nil
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts nil
   dotspacemacs-auto-generate-layout-names nil
   dotspacemacs-large-file-size 1
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 5
   dotspacemacs-enable-paste-transient-state nil
   dotspacemacs-which-key-delay 0.4
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-switch-to-buffer-prefers-purpose nil
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup t
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-show-transient-state-title t
   dotspacemacs-show-transient-state-color-guide t
   dotspacemacs-mode-line-unicode-symbols nil
   dotspacemacs-smooth-scrolling t
   dotspacemacs-line-numbers nil
   dotspacemacs-folding-method 'evil
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-smart-closing-parenthesis nil
   dotspacemacs-smart-closing-parenthesis nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-enable-server nil
   dotspacemacs-server-socket-dir nil
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")
   dotspacemacs-frame-title-format ""
   dotspacemacs-icon-title-format nil
   dotspacemacs-whitespace-cleanup nil
   dotspacemacs-zone-out-when-idle nil
   dotspacemacs-pretty-docs nil
   dotspacemacs-mode-line-theme '(spacemacs :separator bar :separator-scale 1.5)
   )
  )

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env))

(defun dotspacemacs/user-init ()
  )

(defun killSpacemacsHomeBuffer()
  (when (get-buffer "*spacemacs*")
    (kill-buffer "*spacemacs*"))
  (spacemacs/switch-to-scratch-buffer))

(defun dotspacemacs/user-config ()
  (setq inhibit-startup-screen nil)
  ;; for daemon mode
  ;;(add-hook 'after-init-hook #'killSpacemacsHomeBuffer)
  ;;(killSpacemacsHomeBuffer)

  ;; function to load all el files from a specific path
  (defun load-directory (directory)
    "Load recursively all `.el' files in DIRECTORY."
    (dolist (element (directory-files-and-attributes directory nil nil nil))
      (let* ((path (car element))
             (fullpath (concat directory "/" path))
             (isdir (car (cdr element)))
             (ignore-dir (or (string= path ".") (string= path ".."))))
        (cond
         ((and (eq isdir t) (not ignore-dir))
          (load-directory fullpath))
         ((and (eq isdir nil) (string= (substring path -3) ".el"))
          (load (file-name-sans-extension fullpath)))))))
  (load-directory "~/.spacemacs.d/lib")
  (load-file "~/.spacemacs.d/config.el")
  ;; (add-to-list 'default-frame-alist '(internal-border-width . 1))
  ;; (setq-default left-margin-width 1 right-margin-width 1) ; Define new widths.
  (set-window-buffer nil (current-buffer)) ; Use them now.
  (setq create-lockfiles nil)
  (setq initial-buffer-choice t)
  (setq scroll-margin 1)
  (blink-cursor-mode +1)
)
