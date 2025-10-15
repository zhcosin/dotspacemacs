;;; packages.el --- latex-toc layer packages file for Spacemacs.

(defconst latex-toc-packages
  '(
    (latex-toc :location local)
    ))

(defun latex-toc/init-latex-toc ()
  "Initialize latex-toc package."
  ;; 直接加载文件
  (load-file (expand-file-name "layers/latex-toc/local/latex-toc.el" dotspacemacs-directory))
  
  ;; 设置键绑定 - 只在 latex-mode 中生效
  (spacemacs/set-leader-keys-for-major-mode 'latex-mode
    "ot" 'latex-toc
    "or" 'latex-toc-refresh)
  
  ;; 添加到 LaTeX 模式钩子
  (add-hook 'latex-mode-hook 'latex-toc--setup-auto-refresh))