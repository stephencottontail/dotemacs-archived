;;; alabaster-theme.el --- Alabaster theme for Emacs.

;; Copyright (C) 2019-2019 Stephen Dickinson

;; Author: Stephen Dickinson <stephencottontail@me.com>
;; URL: https://github.com/stephencottontail/alabaster-theme.git
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(deftheme alabaster
  "Alabaster theme for Emacs.")

(let* ((class '((class color) (min-colors 89)))

       (gray-3      "#1c1c1e")
       (gray-25     "#686873")
       (gray-2      "#c3c3d6")
       (gray-1      "#ebebf2")
       (green       "#e4f2d0")
       (red         "#f2a5a5")
       (magenta     "#f9e0ff")
       (blue        "#dbf1ff")
       (yellow-dark "#f2edae")
       (yellow      "#fffabc")
       (orange      "#ffc861")
       )

  (custom-theme-set-faces
   'alabaster

   `(default ((,class (:background ,gray-1 :foreground ,gray-3))))

   ;; Global
   `(font-lock-keyword-face           ((,class (:foreground ,gray-3  :background ,gray-1))))
   `(font-lock-constant-face          ((,class (:foreground ,gray-3  :background ,magenta))))
   `(font-lock-type-face              ((,class (:foreground ,gray-3  :background ,gray-1))))
   `(font-lock-builtin-face           ((,class (:foreground ,gray-3  :background ,magenta))))
   `(font-lock-string-face            ((,class (:foreground ,gray-3  :background ,green))))
   `(font-lock-doc-face               ((,class (:foreground ,gray-3  :background ,yellow))))
   `(font-lock-comment-face           ((,class (:foreground ,gray-3  :background ,yellow))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,gray-3  :background ,yellow))))
   `(font-lock-function-name-face     ((,class (:foreground ,gray-3  :background ,blue))))
   `(font-lock-variable-name-face     ((,class (:foreground ,gray-3  :background ,blue))))
   `(line-number                      ((,class (:foreground ,gray-25 :background ,gray-1))))
   `(line-number-current-line         ((,class (:foreground ,gray-3  :background ,gray-1))))
   `(skd/replacement-tags             ((,class (:foreground ,gray-25))))
   
   ;; Flycheck
   `(skd/flycheck-error-status        ((,class (:foreground ,gray-3 :background ,red))))
   `(skd/flycheck-interrupted-status  ((,class (:foreground ,gray-3 :background ,yellow-dark))))
   `(skd/flycheck-suspicious-status   ((,class (:foreground ,gray-3 :background ,yellow-dark))))
   `(skd/flycheck-error-count         ((,class (:foreground ,gray-3 :background ,red))))
   `(skd/flycheck-warning-count       ((,class (:foreground ,gray-3 :background ,yellow-dark))))
   `(skd/flycheck-info-count          ((,class (:foreground ,gray-3 :background ,green))))
   
   ;; Dired
   `(dired-ignored ((,class (:foreground ,gray-3))))
   
   ;; Magit
   `(magit-diff-context           ((,class (:foreground ,gray-3))))
   `(magit-diff-context-highlight ((,class (:foreground ,gray-3))))

   ;; org
   `(org-verbatim                 ((,class (:background ,magenta))))

   ;; Shell scripting
   `(sh-quoted-exec               ((,class (:foreground ,gray-3 :background ,blue))))

   ;; mode line
   `(mode-line                    ((,class (:background ,magenta))))
   `(mode-line-inactive           ((,class (:background ,gray-2))))
   ))

(provide-theme 'alabaster)

;;;###autoload
(when (and (boundp 'custom-theme-load-path)
           load-file-name)
  ;; add theme folder to `custom-theme-load-path' when installing over MELPA
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

;;; alabaster-theme.el ends here
