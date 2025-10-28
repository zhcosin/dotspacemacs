;;; packages.el --- chess-cn layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2025 Sylvain Benner & Contributors
;;
;; Author:  <Administrator@V1>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `chess-cn-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `chess-cn/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `chess-cn/pre-init-PACKAGE' and/or
;;   `chess-cn/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst chess-cn-packages
  '(
    (chess-cn :location local)
    (ucci :location local)
    )
  "The list of Lisp packages required by the chess-cn layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

(defun chess-cn/init-chess-cn ()
  "Initialize chess-cn package"
  (load-file (expand-file-name "layers/chess-cn/local/chess-cn.el" dotspacemacs-directory))

  ;; 确保象棋缓冲区进入 Evil normal 状态，从而应用下面的按键绑定
  (when (featurep 'evil)
    (evil-set-initial-state 'chinese-chess-cn--mode 'normal))

  ;; 设置 leader 按键到 SPC m 前缀
  (spacemacs/set-leader-keys-for-major-mode 'chinese-chess-cn--mode
    "n" 'chess-cn--new
    "u" 'chess-cn--undo
    "s" 'chess-cn--save
    "l" 'chess-cn--load)

  ;; 模式专用按键绑定：Vim 风格用 evil-define-key，Emacs 风格用 define-key
  (if (featurep 'evil)
      (progn
        ;; normal 状态绑定
        (evil-define-key 'normal chinese-chess-cn--mode-map
          (kbd "RET")    'chess-cn--step-cmd
          (kbd "<up>")   'chess-cn--move-point-up
          (kbd "<down>") 'chess-cn--move-point-down
          (kbd "<left>") 'chess-cn--move-point-left
          (kbd "<right>")'chess-cn--move-point-right
          (kbd "h")      'chess-cn--move-point-left
          (kbd "j")      'chess-cn--move-point-down
          (kbd "k")      'chess-cn--move-point-up
          (kbd "l")      'chess-cn--move-point-right)
        ;; motion 状态也绑定一份，覆盖 special-mode 默认行为
        (evil-define-key 'motion chinese-chess-cn--mode-map
          (kbd "RET")    'chess-cn--step-cmd
          (kbd "<up>")   'chess-cn--move-point-up
          (kbd "<down>") 'chess-cn--move-point-down
          (kbd "<left>") 'chess-cn--move-point-left
          (kbd "<right>")'chess-cn--move-point-right
          (kbd "h")      'chess-cn--move-point-left
          (kbd "j")      'chess-cn--move-point-down
          (kbd "k")      'chess-cn--move-point-up
          (kbd "l")      'chess-cn--move-point-right))
    (progn
      (define-key chinese-chess-cn--mode-map (kbd "RET")    'chess-cn--step-cmd)
      (define-key chinese-chess-cn--mode-map (kbd "<up>")   'chess-cn--move-point-up)
      (define-key chinese-chess-cn--mode-map (kbd "<down>") 'chess-cn--move-point-down)
      (define-key chinese-chess-cn--mode-map (kbd "<left>") 'chess-cn--move-point-left)
      (define-key chinese-chess-cn--mode-map (kbd "<right>")'chess-cn--move-point-right)
      (define-key chinese-chess-cn--mode-map (kbd "C-p")    'chess-cn--move-point-up)
      (define-key chinese-chess-cn--mode-map (kbd "C-n")    'chess-cn--move-point-down)
      (define-key chinese-chess-cn--mode-map (kbd "C-b")    'chess-cn--move-point-left)
      (define-key chinese-chess-cn--mode-map (kbd "C-f")    'chess-cn--move-point-right)))

  (add-hook 'chinese-chess-cn--mode-hook
            (lambda ()
              (setq-local global-hl-line-mode nil) ;; 关闭主缓冲区当前行高亮
              (setq-local cursor-type 'box)        ;; 设置主缓冲区光标为块状
              ;; 进入缓冲区时强制切到 normal，避免 evil-collection/special-mode 默认 motion 状态
              (when (featurep 'evil)
                (evil-normal-state))
              ))
  )

(defun chess-cn/init-ucci ()
  "Initialize ucci package"
  (load-file (expand-file-name "layers/chess-cn/local/ucci.el" dotspacemacs-directory))
  
  ;; 设置 UCCI 相关的 leader 按键
  (spacemacs/set-leader-keys-for-major-mode 'chinese-chess-cn--mode
    "e" 'ucci-start-engine
    "E" 'ucci-stop-engine
    "a" 'ucci-get-engine-move
    "A" 'ucci-toggle-ai
    "h" 'ucci-get-hint
    "s" 'ucci-engine-status
    "d" 'ucci-toggle-debug
    "H" 'ucci-start-human-vs-ai)
  )
