(defconst doom-modeline-packages
  '(all-the-icons
    (doom-modeline :location local)))

(defun doom-modeline/init-all-the-icons ()
  (use-package all-the-icons
    :defer t))

(defun doom-modeline/init-doom-modeline ()
  (use-package doom-modeline
    :demand t
    :init
    (setq +doom-modeline-buffer-file-name-style 'file-name
          +doom-modeline-height 27
          +doom-modeline-bar-width 1)
    :config
    (+doom-modeline|init)))
