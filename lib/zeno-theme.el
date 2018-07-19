;;; zeno-theme.el --- A dark theme with vibrant colors

;; Copyright (C) 2017 Bharat Joshi

;; Author: Bharat Joshi <jbharat@outlook.com>
;; Maintainer: Bharat Joshi <jbharat@outlook.com>
;; URL: https://github.com/jbharat/zeno-theme
;; Package-Version: 20180211.1557
;; Created: 22th July 2017
;; Keywords: faces, theme, dark, vibrant colors
;; Version: 1.0.2
;; Package-Requires: ((emacs "24"))

;; License: GPL3

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; This program is distributed in the hope that it will be useful,
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Bright colors over dark background with option to enable italics
;; Inspired by Molokai-theme, Dracula-theme

;;; Code:

(deftheme zeno
  "A dark theme with vibrant colors")

(defgroup zeno-theme nil
  "Zeno-theme options."
  :group 'faces)

(defcustom zeno-theme-enable-italics nil
  "Enable italics for functions, comments, directories"
  :type 'boolean
  :group 'zeno-theme)

(let
    (
     (bg                     "#282A36")
     (fg                     "#E8F0FF")
     (seperator              "#1F2029")
     (cursor                 "#F8F8F0")
     (face1                  "#66D9EF")
     (face2                  "#BB98FC")
     (face3                  "#5FCA81")
     (face4                  "#A6E22E")
     (face5                  "#4577D7")
     (face6                  "#FF84C9")
     (face7                  "#FF5996")
     (face8                  "#60FCEC")
     (face9                  "#665C7E")
     (face10                 "#84B5FF")
     (cursor-match           "#D2527F")
     (m1                     "#C1CAFF")
     (m2                     "#FD971F")
     (m3                     "#EF5939")
     (m7                     "#FF0000")
     (m8                     "#FFCACA")
     (diff1                  "#232526")
     (ml-inactive-face       "#7a7a7a")
     (comment-face           "#6F7181")
     (line-number-face       "#6883A8")
     (warning-bg-face        "#333333")
     (fullWhite              "#FFFFFF")
     (fullBlack              "#000000")
     (hl-line-highlight      "#343850")
     (line-highlight         "#343850")
     (mode-line-active       "#2A3146")
     (mode-line-inactive     "#1d2130")
     
     (slantType (if zeno-theme-enable-italics 'italic 'normal))

   )


(custom-theme-set-faces
 'zeno

 ;; default stuff
 `(default ((t (:background ,bg :foreground ,fg))))
 `(vertical-border ((t (:foreground ,seperator))))
 `(fringe ((t (:background ,bg))))
 `(cursor ((t (:background ,cursor))))
 `(bold ((t (:weight bold))))
 `(bold-italic ((t (:weight bold :slant italic))))
 `(custom-face-tag ((t (:foreground ,face1 :weight bold))))
 `(custom-state ((t (:foreground ,face2))))
 `(italic ((t (:slant italic))))
 `(region ((t (:background ,face9))))
 `(underline ((t (:underline t))))

 ;; diff
 `(diff-added ((t (:foreground ,face2 :weight bold))))
 `(diff-context ((t (:foreground ,fg))))
 `(diff-file-header ((t (:foreground ,face1 :background nil))))
 `(diff-indicator-added ((t (:foreground ,face2))))
 `(diff-indicator-removed ((t (:foreground ,face3))))
 `(diff-header ((t (:foreground ,fg :background ,diff1))))
 `(diff-hunk-header ((t (:foreground ,face4 :background ,diff1))))
 `(diff-removed ((t (:foreground ,face3 :weight bold))))


 `(escape-glyph ((t (:foreground ,m1))))
 `(minibuffer-prompt ((t (:foreground ,face1))))

 ;; powerline/modeline
 `(mode-line ((t (:foreground ,fg :background ,mode-line-active))))
 `(mode-line-inactive ((t (:foreground ,ml-inactive-face :background ,mode-line-inactive :box nil))))
 `(powerline-active0 ((t (:inherit mode-line :background ,bg))))
 `(powerline-active1 ((t (:inherit mode-line :background ,bg))))
 `(powerline-active2 ((t (:inherit mode-line :background ,bg))))
 `(powerline-inactive1 ((t (:inherit mode-line-inactive :background ,bg))))
 `(powerline-inactive2 ((t (:inherit mode-line-inactive :background ,bg))))

 ;; font
 `(font-lock-builtin-face ((t (:foreground ,face2))))
 `(font-lock-comment-face ((t (:foreground ,comment-face :slant ,slantType))))
 `(font-lock-comment-delimiter-face ((t (:foreground ,comment-face :slant ,slantType))))
 `(font-lock-constant-face ((t (:foreground ,face4))))
 `(font-lock-doc-face ((t (:foreground ,m1 :slant ,slantType))))
 `(font-lock-function-name-face ((t (:foreground ,face10 :slant ,slantType))))
 `(font-lock-keyword-face ((t (:foreground ,face1))))
 `(font-lock-negation-char-face ((t (:weight bold))))
 `(font-lock-preprocessor-face ((t (:foreground ,face2))))
 `(font-lock-regexp-grouping-backslash ((t (:weight bold))))
 `(font-lock-regexp-grouping-construct ((t (:weight bold))))
 `(font-lock-string-face ((t (:foreground ,m1))))
 `(font-lock-type-face ((t (:foreground ,face1 :,slantType slant))))
 `(font-lock-variable-name-face ((t (:foreground ,face3))))
 `(font-lock-warning-face ((t (:foreground ,fullWhite (quote :background) ,warning-bg-face))))

 `(hl-todo ((t (:foreground ,m8 :weight bold))))

 ;; Basic face
 `(success ((t (:foreground ,face2))))

 ;; js2-mode
 `(js2-function-call ((t (:inherit default :foreground ,face10 :slant ,slantType))))
 `(js2-function-param ((t (:inherit default :foreground ,face7))))
 `(js2-external-variable ((t (:foreground ,face4))))
 
 ;; highlighting
 `(highlight ((t (:foreground ,fullBlack :background ,m1))))
 `(hl-line ((t (:background ,hl-line-highlight))))
 `(lazy-highlight ((t (:foreground ,fullBlack :background ,m1))))

 ;; isearch
 `(isearch ((t (:foreground ,fullBlack :background ,m1))))
 `(isearch-fail ((t (:foreground ,fullWhite :background ,warning-bg-face))))
 `(ahs-plugin-whole-buffer-face ((t (:foreground ,fullBlack :background ,face10))))
 `(ahs-face ((t (:foreground ,fullBlack :background ,m1))))
 `(ahs-definition-face ((t (:background ,m1 :foreground ,face8))))

 ;; org
 `(outline-1 ((t (:foreground ,face1))))
 `(outline-2 ((t (:foreground ,face2))))
 `(outline-3 ((t (:foreground ,face3))))
 `(outline-4 ((t (:foreground ,face4))))
 `(outline-5 ((t (:foreground ,face5))))
 `(outline-6 ((t (:foreground ,face6))))
 `(outline-7 ((t (:foreground ,face7))))
 `(outline-8 ((t (:foreground ,face8))))
 `(org-level-1 ((t (:foreground ,face10  ))))
 `(org-level-2 ((t (:inherit outline-1 ))))
 `(org-level-3 ((t (:inherit outline-3  ))))
 `(org-level-4 ((t (:inherit outline-4  ))))
 `(org-level-5 ((t (:inherit outline-5  ))))
 `(org-level-6 ((t (:inherit outline-6  ))))
 `(org-level-7 ((t (:inherit outline-7  ))))
 `(org-level-8 ((t (:inherit outline-8  ))))
 `(rainbow-delimiters-depth-1-face ((t (:inherit outline-1))))
 `(rainbow-delimiters-depth-2-face ((t (:inherit outline-2))))
 `(rainbow-delimiters-depth-3-face ((t (:inherit outline-3))))
 `(rainbow-delimiters-depth-4-face ((t (:inherit outline-4)))) 
 `(rainbow-delimiters-depth-5-face ((t (:inherit outline-5))))
 `(rainbow-delimiters-depth-6-face ((t (:inherit outline-6))))
 `(rainbow-delimiters-depth-7-face ((t (:inherit outline-7))))
 `(rainbow-delimiters-depth-8-face ((t (:inherit outline-8))))
 `(rainbow-delimiters-depth-9-face ((t (:foreground ,face1))))

 ;; others
 `(secondary-selection ((t (:foreground ,fg))))
 `(shadow ((t (:foreground ,comment-face))))
 `(widget-inactive ((t (:background ,m7))))
 `(widget-button-pressed ((t (:foreground ,m8))))

 ;; undo-tree
 `(undo-tree-visualizer-active-branch-face ((t (:inherit default))))
 `(undo-tree-visualizer-current-face ((t (:foreground ,m3))))
 `(undo-tree-visualizer-default-face ((t (:inherit shadow))))
 `(undo-tree-visualizer-register-face ((t (:foreground ,m1))))
 `(undo-tree-visualizer-unmodified-face ((t (:foreground ,face1))))

 ;; helm-buffer
 `(helm-buffer-file ((t (:foreground ,face1))))
 `(helm-ff-executable ((t (:foreground ,fullWhite))))
 `(helm-ff-file ((t (:foreground ,fullWhite))))
 `(helm-prefarg ((t (:foreground ,face4))))
 `(helm-selection ((t (:background ,face5 :foreground ,fullWhite))))
 `(helm-buffer-directory ((t (:foreground ,face4))))
 `(helm-ff-directory ((t (:foreground ,face4))))
 `(helm-source-header ((t (:background ,fullBlack :foreground ,fullWhite
                                       :weight bold :height 1.3 :family "Sans Serif"))))
 `(helm-swoop-target-line-block-face ((t (:background ,face5 :foreground ,fullWhite))))
 `(helm-swoop-target-line-face ((t (:background ,face5 :foreground ,fullWhite))))

 ;; ivy
 `(ivy-current-match ((t (:background ,face5 :foreground ,fullWhite))))
 `(ivy-highlight-face ((t (:background ,fullBlack :foreground ,face3))))
 `(ivy-modified-buffer ((t (:inherit default :foreground ,m2))))
 `(ivy-virtual ((t (:inherit default ))))
 `(ivy-minibuffer-match-face-1 ((t (:inherit default :foreground ,face10))))
 `(ivy-minibuffer-match-face-2 ((t (:inherit default :foreground ,face10))))
 `(ivy-minibuffer-match-face-3 ((t (:inherit default :foreground ,face10))))
 `(ivy-minibuffer-match-face-4 ((t (:inherit default :foreground ,face10))))
 `(swiper-line-face ((t (:background ,face5 :foreground ,fullWhite))))
 `(swiper-match-face-2 ((t (:foreground ,face10))))
 `(ivy-modified-buffer ((t (:inherit default :foreground ,face6))))

  ;; company
  `(company-tooltip ((t (:background ,bg :foreground ,fullWhite))))
  `(company-tooltip-mouse ((t (:foreground ,face10))))
  `(company-template-field ((t (:background: ,bg :foreground ,fullWhite))))
  `(company-tooltip-selection ((t (:background ,face5 :foreground ,fullWhite))))
  `(company-echo-common ((t (:foreground ,face6))))
  `(company-scrollbar-bg ((t (:background ,seperator))))
  `(company-scrollbar-fg ((t (:background ,line-highlight))))
  `(company-tooltip-annotation ((t (:foreground ,face3))))
  `(company-tooltip-annotation-selection ((t (:inherit company-tooltip-annotation))))
  `(company-tooltip-common ((t (:foreground ,face10))))
  `(company-preview ((t (:background ,line-highlight))))
  `(company-preview-common ((t (:inherit company-preview ))))
  `(company-preview-search ((t (:inherit company-preview))))

  ;; neotree
  `(neo-dir-link-face ((t (:foreground ,face10 ))))
  `(neo-root-dir-face ((t (:foreground ,face1 ))))

  ;; treemacs
  `(treemacs-directory-face ((t (:foreground ,face10 ))))

  ;; parentheses matching
  `(show-paren-match ((t (:background ,cursor-match :foreground ,fullBlack :weight bold))))
  `(show-paren-mismatch ((t (:background ,m7 :foreground ,fullWhite))))
  `(rainbow-delimiters-mismatched-face ((t (:inherit show-paren-mismatch :underline t))))
  `(rainbow-delimiters-unmatched-face ((t (:inherit show-paren-mismatch))))

  ;; dired
  `(dired-directory ((t (:foreground ,face10 ))))

  ;; Web-mode
  `(web-mode-html-attr-custom-face ((t (:foreground ,face7))))
  `(web-mode-html-attr-equal-face ((t (:foreground ,fullWhite))))
  `(web-mode-html-attr-name-face ((t (:foreground ,face3))))
  `(web-mode-html-attr-value-face ((t (:inherit font-lock-string-face ))))
  `(web-mode-html-tag-bracket-face ((t (:foreground ,fullWhite))))
  `(web-mode-html-tag-face ((t (:inherit font-lock-function-name-face))))
  `(web-mode-html-tag-custom-face ((t (:inherit web-mode-html-tag-face))))

  ;; linum relative line number face
  `(linum-relative-current-face ((t (:inherit linum :foreground ,face2 :weight normal))))
  `(linum ((t (:background ,bg :foreground ,line-number-face :weight normal))))

  ;; native line number face
  `(line-number ((t :background ,bg :foreground ,line-number-face :weight normal)))
  `(line-number-current-line ((t :background ,bg :foreground, face2, :weight normal)))

  ;; imenu-list
  `(imenu-list-entry-subalist-face-0 ((t (:foreground ,face2))))
  `(imenu-list-entry-subalist-face-1 ((t (:foreground ,face4))))
  `(imenu-list-entry-subalist-face-2 ((t (:foreground ,face6))))
  `(imenu-list-entry-subalist-face-3 ((t (:foreground ,face8))))
  `(imenu-list-entry-face-0 ((t (:foreground ,face1))))
  `(imenu-list-entry-face-1 ((t (:foreground ,face3))))
  `(imenu-list-entry-face-2 ((t (:foreground ,face5))))
  `(imenu-list-entry-face-3 ((t (:foreground ,face7))))

  ;; avy
  `(avy-lead-face ((t (:background ,face1 :foreground ,fullBlack :weight bold))))
  `(avy-lead-face-0 ((t (:background ,face2 :foreground ,fullBlack :weight bold))))
  `(avy-lead-face-1 ((t (:background ,face3 :foreground ,fullBlack :weight bold))))
  `(avy-lead-face-2 ((t (:background ,face4 :foreground ,fullBlack :weight bold))))


  ;; indent-guide faces
  `(indent-guide-face ((t (:foreground ,line-number-face ))))
  `(highlight-indent-guides-character-face ((t (:foreground ,line-number-face ))))

  `(link ((t (:foreground ,face8 :background ,bg :underline t))))

  ;; diredp
  `(diredp-dir-heading ((t (:foreground ,face4 :underline t))))
  `(diredp-dir-name ((t (:foreground ,face10 ))))
  `(diredp-file-name ((t (:foreground ,fg))))
  `(diredp-file-suffix ((t (:foreground ,fg))))
  `(diredp-flag-mark ((t (:foreground ,face2))))
  `(diredp-flag-mark-line ((t (:foreground ,face2))))
  `(diredp-deletion ((t (:foreground ,face7))))
  `(diredp-deletion-file-name ((t (:foreground ,face7))))
  `(diredp-number ((t (:foreground ,face1 ))))
  `(diredp-read-priv ((t (:foreground ,face6))))
  `(diredp-exec-priv ((t (:foreground ,face8))))
  `(diredp-dir-priv ((t (:foreground ,face10))))
  `(diredp-write-priv ((t (:foreground ,face3))))
  `(diredp-no-priv ((t (:foreground ,fg))))

  `(evil-snipe-first-match-face ((t (:inherit isearch))))
  `(evil-snipe-matches-face ((t (:inherit isearch))))

 ))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide 'zeno-theme)
;;; zeno-theme.el ends here
