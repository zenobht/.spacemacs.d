
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
     git
     ;;(helm :variables helm-no-header t)
     html
     (ibuffer :variables ibuffer-group-buffers-by nil)
     (ivy :variables
          ivy-wrap t
          ivy-height 15
          ;; ivy-enable-advanced-buffer-information t
          ivy-use-virtual-buffers t
          )
     java
     (javascript :variables javascript-disable-tern-port-files t)
     kotlin
     (markdown :variables
               markdown-command 'pandoc
               markdown-live-preview-engine 'vmd)
     octave
     (org :variables
          org-projectile-file "TODOs.org"
          org-enable-org-journal-support t
          org-journal-file-format "%Y-%m-%d"
          org-journal-date-prefix "#+TITLE: "
          org-journal-date-format "%A, %B %d %Y"
          org-journal-time-prefix "* "
          org-journal-time-format "%I:%M%p%n"
          org-enable-reveal-js-support t
          org-enable-github-support t)
     osx
     pandoc
     (python :variables
             python-sort-imports-on-save t
             python-enable-yapf-format-on-save t)
     (ranger :variables ranger-override-dired t)
     react
     ruby
     (shell :variables
            shell-default-shell 'eshell
            shell-default-full-span nil
            shell-default-height 20
            shell-default-position 'bottom)
     spacemacs-org
     (spell-checking :variables spell-checking-enable-by-default nil)
     (syntax-checking :variables syntax-checking-enable-by-default nil)
     (treemacs :variables
               treemacs-use-filewatch-mode t
               treemacs-use-follow-mode t
               treemacs-git-mode 'extended 
               treemacs-use-collapsed-directories 3
               )
     typescript
     (version-control :variables
                      version-control-diff-tool 'git-gutter+
                      version-control-diff-side 'right
                      version-control-global-margin t)
     yaml
     )
   dotspacemacs-additional-packages '(
                                      vue-mode
                                      lsp-mode
                                      lsp-vue
                                      lsp-ui
                                      company-lsp
                                      dracula-theme
                                      drag-stuff
                                      evil-multiedit
                                      exotica-theme
                                      flycheck-flow
                                      flycheck-package
                                      highlight-indent-guides
                                      molokai-theme
                                      org-mime
                                      pretty-mode
                                      (reason-mode
                                       :location (recipe
                                                  :repo "reasonml-editor/reason-mode"
                                                  :fetcher github
                                                  :files ("reason-mode.el" "refmt.el" "reason-indent.el" "reason-interaction.el")))
                                      subatomic-theme
                                      vue-mode
                                      )
   dotspacemacs-frozen-packages '()
   dotspacemacs-excluded-packages '(
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
   dotspacemacs-elpa-subdirectory nil
   dotspacemacs-elpa-https nil
   dotspacemacs-elpa-timeout 60
   dotspacemacs-check-for-update nil
   dotspacemacs-elpa-subdirectory nil
   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner 'official
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   dotspacemacs-startup-buffer-responsive t
   dotspacemacs-scratch-mode 'org-mode
   dotspacemacs-themes '(
                         exotica
                         spacemacs-dark
                         molokai
                         subatomic
                         )
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Operator Mono"
                               :size 17
                               :weight normal 
                               :width normal)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-command-key "SPC"
   dotspacemacs-ex-command-key ":"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-distinguish-gui-tab nil
   dotspacemacs-remap-Y-to-y$ nil
   dotspacemacs-retain-visual-state-on-shift t
   dotspacemacs-visual-line-move-text t
   dotspacemacs-ex-substitute-global nil
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts nil
   dotspacemacs-large-file-size 1
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 5
   dotspacemacs-helm-resize nil
   dotspacemacs-helm-no-header nil
   dotspacemacs-helm-position 'bottom
   dotspacemacs-helm-use-fuzzy 'always
   dotspacemacs-enable-paste-transient-state nil
   dotspacemacs-which-key-delay 0.4
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-loading-progress-bar nil
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup t
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-show-transient-state-title t
   dotspacemacs-show-transient-state-color-guide t
   dotspacemacs-mode-line-unicode-symbols nil 
   dotspacemacs-smooth-scrolling nil
   dotspacemacs-line-numbers nil
   dotspacemacs-folding-method 'origami
   dotspacemacs-smartparens-strict-mode nil 
   dotspacemacs-smart-closing-parenthesis nil 
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup nil
   dotspacemacs-mode-line-theme '(spacemacs
                                  :separator alternate  
                                  )
   dotspacemacs-frame-title-format ""
   ))

(defun dotspacemacs/user-init ()
  ;; required to disable to spacelpa
  (setq configuration-layer-elpa-archives
          '(("melpa"    . "melpa.org/packages/")
            ("org"      . "orgmode.org/elpa/")
            ("gnu"      . "elpa.gnu.org/packages/")))

  (setq-default
   evil-shift-round nil
   avy-all-windows 'all-frames
   )
  (setq exec-path-from-shell-check-startup-files nil)
  )

(defun dotspacemacs/user-config ()

  ;; set up emacs path using system's path
  (exec-path-from-shell-copy-env "PATH")

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
  (org-babel-load-file "~/.spacemacs.d/config.org")
  ;; for transparency, useful to keep
  ;;(set-frame-parameter (selected-frame) 'alpha '(90 . 50))
  ;;(add-to-list 'default-frame-alist '(alpha . (90 . 50)))

  (setq kotlin-tab-width 2)
)


(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(pip-requirements yasnippet-snippets yapfify yaml-mode xterm-color ws-butler winum which-key wgrep web-mode web-beautify vue-mode volatile-highlights vmd-mode uuidgen use-package unfill treemacs-projectile treemacs-evil toc-org tide tagedit symon subatomic-theme string-inflection spaceline-all-the-icons smex smeargle slim-mode shell-pop scss-mode sass-mode rvm ruby-tools ruby-test-mode ruby-refactor ruby-hash-syntax rubocop rspec-mode robe reveal-in-osx-finder restart-emacs request reason-mode rbenv ranger rake rainbow-mode rainbow-identifiers rainbow-delimiters pyvenv pytest pyenv-mode py-isort pug-mode pretty-mode popwin pippel pipenv persp-mode pbcopy password-generator paradox pandoc-mode ox-reveal ox-pandoc ox-gfm overseer osx-trash osx-dictionary origami orgit org-projectile org-present org-pomodoro org-mime org-journal org-download org-bullets org-brain open-junk-file nameless mwim mvn multi-term move-text molokai-theme minitest meghanada maven-test-mode markdown-toc magit-gitflow macrostep lsp-vue lsp-ui lsp-python lorem-ipsum livid-mode live-py-mode link-hint launchctl kotlin-mode json-mode js2-refactor js-doc ivy-xref ivy-purpose ivy-hydra indent-guide importmagic impatient-mode ibuffer-projectile hy-mode hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation highlight-indent-guides helm-make groovy-mode groovy-imports gradle-mode google-translate golden-ratio gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gh-md fuzzy font-lock+ flyspell-correct-ivy flycheck-pos-tip flycheck-package flycheck-kotlin flycheck-flow flx-ido fill-column-indicator fancy-battery eyebrowse expand-region exotica-theme exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-org evil-numbers evil-multiedit evil-mc evil-matchit evil-magit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-commentary evil-cleverparens evil-args evil-anzu eval-sexp-fu eshell-z eshell-prompt-extras esh-help ensime emojify emoji-cheat-sheet-plus emmet-mode elisp-slime-nav editorconfig dumb-jump drag-stuff dracula-theme diminish diff-hl cython-mode csv-mode counsel-projectile counsel-css company-web company-tern company-statistics company-lsp company-emoji company-emacs-eclim company-anaconda column-enforce-mode color-identifiers-mode coffee-mode clean-aindent-mode chruby centered-cursor-mode bundler browse-at-remote auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile aggressive-indent adaptive-wrap ace-link ac-ispell)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)
