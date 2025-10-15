
;;; latex-toc.el --- LaTeX Table of Contents for Spacemacs

;;; Commentary:
;; This package provides a table of contents view for LaTeX documents.

;;; Code:

(defvar latex-toc-buffer-name "*LaTeX TOC*"
  "缓冲区名称用于显示LaTeX目录。")

;; 主要的数据结构 - 存储所有章节条目
(defvar latex-toc--entries nil
  "List of entries in the table of contents.")

;; 每个条目的结构大致如下：
;; ((depth . 2)
;;  (number . "1.1")
;;  (name . "Introduction")
;;  (file . "~/doc/main.tex")
;;  (point . 1234)
;;  (start . 2345))

(defvar latex-toc-buffer-side 'left
  "Which side to display the LaTeX TOC buffer on.")

;; 修改：折叠状态管理，现在存储具体的折叠状态而不是简单的布尔值
(defvar latex-toc--folded-entries (make-hash-table :test 'equal)
  "Hash table to store folded state of entries. 
   Key is entry identifier, value is one of: folded, children-only, expanded")

;; 新增：折叠状态枚举
(defvar latex-toc--fold-states '(folded children-only expanded)
  "折叠状态列表：folded(折叠), children-only(只显示直接子级), expanded(完全展开)")

(defvar my-latex-section-counters '((0 . 0) (1 . 0) (2 . 0) (3 . 0) (4 . 0) (5 . 0))
  "保存当前各级章节计数器的值。")

(defun latex-toc--collect-entries ()
  "收集LaTeX文档中的所有章节条目"
  (let ((entries '())
        (source-buffer (or (bound-and-true-p latex-toc-source-buffer) 
                          (current-buffer)))  ; 使用保存的源缓冲区或当前缓冲区
        main-file)
    
    ;; 确保在源LaTeX缓冲区中查找主文档
    (with-current-buffer source-buffer
      (setq main-file (latex-toc--find-main-file)))
    
    ;; 检查是否找到主文档
    (unless main-file
      (error "未找到LaTeX主文档！请确保当前目录包含带有\\documentclass或\\begin{document}的.tex文件"))
    
    ;; 检查主文档文件是否存在，如果没有.tex扩展名则自动添加
    (let ((main-file-with-ext (if (string-match "\\.tex$" main-file)
                                  main-file
                                (concat main-file ".tex"))))
      (unless (file-exists-p main-file-with-ext)
        (error "主文档文件不存在：%s" main-file-with-ext))
      ;; 更新main-file为带扩展名的版本
      (setq main-file main-file-with-ext))
    
    ;; 重置计数器状态
    (setq my-latex-section-counters '((0 . 0) (1 . 0) (2 . 0) (3 . 0) (4 . 0) (5 . 0)))
    
    ;; 递归处理所有文件
    (setq entries (nreverse (latex-toc--collect-entries-in-file main-file main-file entries)))
    ;;(message "%S" entries)
    entries))

(defun latex-toc--collect-entries-in-file (main-file-name filename entries)
  "在指定文件中收集章节信息"
  (with-temp-buffer
    (insert-file-contents filename)
    (latex-mode)
    
    ;; 收集章节条目
    (goto-char (point-min))
    (while (re-search-forward 
            "^\\([^%\n]*\\)\\\\\\(chapter\\|section\\|subsection\\)\\*?{\\([^}]*\\)}" 
            nil t)
      (unless (save-excursion
                (goto-char (match-beginning 0))
                (looking-at-p "[ \t]*%"))
        (let ((entry (latex-toc--make-entry (match-string 2)
                                           (match-string 3)
                                           filename
                                           (point))))
          (push entry entries))))
    
    ;; 处理include/input的文件
    (goto-char (point-min))
    (while (re-search-forward 
            "^\\([^%\n]*\\)\\\\\\(include\\|input\\){\\([^}]+\\)}" 
            nil t)
      (unless (save-excursion
                (goto-char (match-beginning 0))
                (looking-at-p "[ \t]*%"))
        (let ((included-file (match-string 3)))
          (unless (string-match "\\.tex$" included-file)
            (setq included-file (concat included-file ".tex")))
          (setq entries (latex-toc--collect-entries-in-file 
                         main-file-name
                         (expand-file-name included-file (file-name-directory main-file-name))
                         entries)))))
    entries))

(defun latex-toc--make-entry (type title filename position)
  "创建单个章节条目"
  (let* ((depth (cdr (assoc type '(("chapter" . 0)
                                  ("section" . 1)
                                  ("subsection" . 2)
                                  ("subsubsection" . 3)))))
        (number (latex-toc--calculate-section-number depth))
        ;; 修改：使用更稳定的标识符，不依赖具体位置
        (entry-id (format "%s:%s:%d:%s" 
                          (file-name-nondirectory filename) 
                          title 
                          depth
                          number)))
    
    `((depth . ,depth)
      (number . ,number)
      (name . ,title)
      (file . ,filename)
      (point . ,position)
      (start . ,(save-excursion (goto-char position) 
                               (line-beginning-position)))
      (id . ,entry-id))))

(defun latex-toc--calculate-section-number (section-level)
  "根据 SECTION-LEVEL 计算当前的完整章节编号。
SECTION-LEVEL 对应章节目令的层级，例如：0=chapter, 1=section, 2=subsection。"
  ;; 使用局部副本避免污染全局状态
  (let ((current-counters (copy-alist my-latex-section-counters)))
    ;; 1. 递增当前层级的计数器
    (setcdr (assoc section-level current-counters)
            (1+ (cdr (assoc section-level current-counters))))
    
    ;; 2. 重置所有更深层级的计数器（归零）
    (mapcar (lambda (counter-pair)
              (when (> (car counter-pair) section-level)
                (setcdr counter-pair 0)))
            current-counters)
    
    ;; 3. 生成编号字符串（例如 "1.2.3"）
    (let ((number-parts '()))
      ;; 从 chapter (level 0) 开始，到当前级别，收集非零的计数器值
      (dotimes (level (1+ section-level))
        (let ((count (cdr (assoc level current-counters))))
          (when (> count 0)
            (push (number-to-string count) number-parts))))
      
      ;; 更新全局计数器状态
      (setq my-latex-section-counters current-counters)
      
      ;; 将部分组合成最终的编号字符串
      (if number-parts
          (mapconcat 'identity (nreverse number-parts) ".")
        "")))) ; 对于 part 或不编号的情况可能返回空字符串

;; 新增：清理折叠状态的函数
(defun latex-toc--clean-fold-states ()
  "清理不存在的条目的折叠状态"
  (let ((valid-ids (mapcar (lambda (entry) (cdr (assoc 'id entry))) latex-toc--entries))
        (keys-to-remove '()))
    ;; 找出需要删除的键
    (maphash (lambda (key value)
               (unless (member key valid-ids)
                 (push key keys-to-remove)))
             latex-toc--folded-entries)
    ;; 删除无效的键
    (dolist (key keys-to-remove)
      (remhash key latex-toc--folded-entries))))

(defun latex-toc-refresh ()
  "刷新 LaTeX TOC 缓冲区的内容。"
  (interactive)
  (let ((toc-buffer (get-buffer latex-toc-buffer-name)))
    (if (not toc-buffer)
        (message "TOC 缓冲区未找到。请先运行 `latex-toc`。")
      (with-current-buffer toc-buffer
        ;; 保存当前光标位置的条目ID
        (let ((current-entry-id (when (get-text-property (point) 'latex-toc-entry)
                                  (cdr (assoc 'id (get-text-property (point) 'latex-toc-entry))))))
          ;; 重新收集条目
          (setq latex-toc--entries (my-latex-toc-collect-headings))
          ;; 清理无效的折叠状态
          (latex-toc--clean-fold-states)
          ;; 刷新显示
          (latex-toc--refresh-buffer)
          ;; 尝试恢复光标位置
          (when current-entry-id
            (goto-char (point-min))
            (while (and (not (eobp))
                        (let ((entry (get-text-property (point) 'latex-toc-entry)))
                          (not (and entry
                                    (string= (cdr (assoc 'id entry)) current-entry-id)))))
              (forward-line 1)))
          (message "LaTeX TOC 已刷新。"))))))

(defun latex-toc--entry-has-children-p (entry entries)
  "检查条目是否有子条目"
  (let ((entry-depth (cdr (assoc 'depth entry)))
        (entry-index (cl-position entry entries :test 'equal)))
    (when entry-index
      (let ((next-entry (nth (1+ entry-index) entries)))
        (and next-entry
             (> (cdr (assoc 'depth next-entry)) entry-depth))))))

(defun latex-toc--entry-is-folded-p (entry)
  "检查条目是否被折叠（兼容旧版本）"
  (not (eq (latex-toc--get-fold-state entry) 'expanded)))

(defun latex-toc--get-fold-state (entry)
  "获取条目的折叠状态"
  (let ((entry-id (cdr (assoc 'id entry))))
    (or (gethash entry-id latex-toc--folded-entries) 'expanded)))

(defun latex-toc--set-fold-state (entry state)
  "设置条目的折叠状态"
  (let ((entry-id (cdr (assoc 'id entry))))
    (if (eq state 'expanded)
        (remhash entry-id latex-toc--folded-entries)
      (puthash entry-id state latex-toc--folded-entries))))

(defun latex-toc--get-children (entry entries)
  "获取条目的所有子条目"
  (let ((entry-depth (cdr (assoc 'depth entry)))
        (entry-index (cl-position entry entries :test 'equal))
        (children '()))
    (when entry-index
      (let ((i (1+ entry-index)))
        (while (and (< i (length entries))
                    (let ((next-entry (nth i entries)))
                      (> (cdr (assoc 'depth next-entry)) entry-depth)))
          (push (nth i entries) children)
          (setq i (1+ i)))))
    (nreverse children)))

(defun latex-toc--get-direct-children (entry entries)
  "获取条目的直接子条目（只有下一级）"
  (let ((entry-depth (cdr (assoc 'depth entry)))
        (children (latex-toc--get-children entry entries)))
    (cl-remove-if-not (lambda (child)
                        (= (cdr (assoc 'depth child)) (1+ entry-depth)))
                      children)))

(defun latex-toc--cycle-fold-state (entry)
  "循环切换条目的折叠状态：folded -> children-only -> expanded -> folded"
  (let ((current-state (latex-toc--get-fold-state entry)))
    (cond
     ((eq current-state 'folded) 'children-only)
     ((eq current-state 'children-only) 'expanded)
     ((eq current-state 'expanded) 'folded)
     (t 'children-only))))

(defun latex-toc--toggle-fold (entry)
  "切换条目的折叠状态"
  (let ((entry-id (cdr (assoc 'id entry))))
    (if (gethash entry-id latex-toc--folded-entries)
        (remhash entry-id latex-toc--folded-entries)
      (puthash entry-id t latex-toc--folded-entries))))

(defun latex-toc--should-show-entry-p (entry entries)
  "判断条目是否应该显示（考虑新的三态折叠逻辑）"
  (let ((entry-depth (cdr (assoc 'depth entry)))
        (entry-index (cl-position entry entries :test 'equal)))
    (if (<= entry-depth 0)
        t  ; 顶级条目总是显示
      ;; 从当前条目向上查找所有父级条目
      (let ((show-p t))
        (dolist (ancestor (latex-toc--get-ancestors entry entries))
          (when show-p
            (let ((ancestor-state (latex-toc--get-fold-state ancestor))
                  (ancestor-depth (cdr (assoc 'depth ancestor))))
              (cond
               ;; 祖先完全折叠，不显示任何子条目
               ((eq ancestor-state 'folded)
                (setq show-p nil))
               ;; 祖先只显示直接子条目
               ((eq ancestor-state 'children-only)
                ;; 只有当前条目是该祖先的直接子条目时才可能显示
                (unless (= (1+ ancestor-depth) entry-depth)
                  (setq show-p nil)))
               ;; 祖先完全展开，继续检查下一个祖先
               ((eq ancestor-state 'expanded)
                nil)))))
        show-p))))

(defun latex-toc--get-ancestors (entry entries)
  "获取条目的所有祖先条目（按从近到远的顺序）"
  (let ((entry-depth (cdr (assoc 'depth entry)))
        (entry-index (cl-position entry entries :test 'equal))
        (ancestors '()))
    (when entry-index
      ;; 向前查找所有深度小于当前条目的条目
      (let ((current-index (1- entry-index)))
        (while (>= current-index 0)
          (let ((candidate (nth current-index entries))
                (candidate-depth (cdr (assoc 'depth (nth current-index entries)))))
            (when (< candidate-depth entry-depth)
              ;; 找到一个祖先
              (push candidate ancestors)
              ;; 更新当前深度，继续查找更高级的祖先
              (setq entry-depth candidate-depth))
            (setq current-index (1- current-index)))))
    ancestors)))

;; 可以删除 latex-toc--get-siblings 函数，因为它不再需要
(defun latex-toc--get-siblings (entry entries)
  "获取与当前条目同级的条目列表"
  (let ((entry-depth (cdr (assoc 'depth entry)))
        (entry-index (cl-position entry entries :test 'equal))
        (siblings '()))
    (when entry-index
      ;; 向前查找同级条目
      (let ((current-index (1- entry-index)))
        (while (>= current-index 0)
          (let ((candidate (nth current-index entries))
                (candidate-depth (cdr (assoc 'depth (nth current-index entries)))))
            (cond
             ((< candidate-depth entry-depth) (return))
             ((= candidate-depth entry-depth) (push candidate siblings))))
          (setq current-index (1- current-index)))
      
      ;; 向后查找同级条目
      (let ((current-index (1+ entry-index)))
        (while (< current-index (length entries))
          (let ((candidate (nth current-index entries))
                (candidate-depth (cdr (assoc 'depth (nth current-index entries)))))
            (cond
             ((< candidate-depth entry-depth) (return))
             ((= candidate-depth entry-depth) (push candidate siblings))))
          (setq current-index (1+ current-index)))))
    (nreverse siblings))))

(defun latex-toc--get-fold-indicator (entry entries)
  "获取折叠指示符"
  (if (latex-toc--entry-has-children-p entry entries)
      (let ((state (latex-toc--get-fold-state entry)))
        (cond
         ((eq state 'folded) "+ ")
         ((eq state 'children-only) "▷ ")
         ((eq state 'expanded) "▽ ")
         (t "  ")))
    "  "))

(defun latex-toc--refresh-buffer ()
  "刷新TOC缓冲区内容"
  (let ((inhibit-read-only t))
    (erase-buffer)
    
    (dolist (entry latex-toc--entries)
      (when (latex-toc--should-show-entry-p entry latex-toc--entries)
        (let ((depth (cdr (assoc 'depth entry)))
              (number (cdr (assoc 'number entry)))
              (name (cdr (assoc 'name entry)))
              (fold-indicator (latex-toc--get-fold-indicator entry latex-toc--entries))
              (start (point)))
          
          ;; 插入格式化的行 - 修改缩进计算方式
          (insert (make-string (* 2 (max 0 depth)) ?\s)  ; 修改为4空格缩进
                  fold-indicator                        ; 折叠指示符
                  (if number (concat number " ") "")    ; 章节编号
                  name "\n")                           ; 章节名称
          
          ;; 添加文本属性用于跳转和折叠
          (add-text-properties 
           start (1- (point))
           `(latex-toc-entry ,entry
             mouse-face highlight
             help-echo "鼠标点击跳转到该章节，TAB键展开/折叠")))))
    
    (goto-char (point-min))))

(defun latex-toc--toggle-fold-at-point ()
  "在当前位置循环切换折叠状态"
  (interactive)
  (let ((entry (get-text-property (point) 'latex-toc-entry)))
    (when entry
      (if (latex-toc--entry-has-children-p entry latex-toc--entries)
          (let* ((current-state (latex-toc--get-fold-state entry))
                 (new-state (latex-toc--cycle-fold-state entry))
                 (state-names '((folded . "折叠")
                               (children-only . "显示直接子级")
                               (expanded . "完全展开"))))
            (latex-toc--set-fold-state entry new-state)
            (latex-toc--refresh-buffer)
            ;; 保持光标位置
            (let ((entry-id (cdr (assoc 'id entry))))
              (goto-char (point-min))
              (while (and (not (eobp))
                          (let ((current-entry (get-text-property (point) 'latex-toc-entry)))
                            (not (and current-entry
                                      (string= (cdr (assoc 'id current-entry)) entry-id)))))
                (forward-line 1))))
        (message "该条目没有子条目")))))

(defun latex-toc--expand-all ()
  "展开所有条目"
  (interactive)
  (clrhash latex-toc--folded-entries)
  (latex-toc--refresh-buffer))

(defun latex-toc--collapse-all ()
  "折叠所有有子条目的条目"
  (interactive)
  (dolist (entry latex-toc--entries)
    (when (latex-toc--entry-has-children-p entry latex-toc--entries)
      (latex-toc--set-fold-state entry 'folded)))
  (latex-toc--refresh-buffer))

(defun latex-toc--show-children-only-all ()
  "将所有有子条目的条目设置为只显示直接子级"
  (interactive)
  (dolist (entry latex-toc--entries)
    (when (latex-toc--entry-has-children-p entry latex-toc--entries)
      (latex-toc--set-fold-state entry 'children-only)))
  (latex-toc--refresh-buffer))

;; 定义键盘映射 - 必须在 define-derived-mode 之前定义
(defvar latex-toc-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'latex-toc--jump-to-entry)
    (define-key map (kbd "TAB") 'latex-toc--toggle-fold-at-point)
    (define-key map (kbd "<tab>") 'latex-toc--toggle-fold-at-point)
    (define-key map (kbd "r") 'latex-toc-refresh)
    (define-key map (kbd "g") 'latex-toc-refresh)
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "E") 'latex-toc--expand-all)
    (define-key map (kbd "C") 'latex-toc--collapse-all)
    (define-key map (kbd "S") 'latex-toc--show-children-only-all)
    (define-key map [mouse-1] 'latex-toc--jump-to-entry)
    map)
  "Keymap for latex-toc-mode.")

(define-derived-mode latex-toc-mode special-mode "LaTeX-TOC"
  "Major mode for LaTeX table of contents."
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (setq-local mouse-1-click-follows-link nil))

(defun latex-toc--jump-to-entry ()
  "跳转到当前条目对应的位置"
  (interactive)
  (let ((entry (get-text-property (point) 'latex-toc-entry)))
    (when entry
      (let ((file (cdr (assoc 'file entry)))
            (point (cdr (assoc 'point entry))))
        ;; 修改：使用 find-file 而不是 find-file-other-window
        (find-file file)
        (goto-char point)
        (recenter)
        ;; 新增：确保窗口焦点切换到目标缓冲区
        (select-window (get-buffer-window (current-buffer)))))))

(defun my-latex-toc-collect-headings ()
  "使用现有的latex-toc包来收集条目"
  (latex-toc--collect-entries))

(defun latex-toc--setup-auto-refresh ()
  "设置自动刷新机制"
  (add-hook 'after-save-hook 'latex-toc--maybe-refresh nil t))

(defun latex-toc--maybe-refresh ()
  "在保存LaTeX文件时可能刷新TOC"
  (when (and (derived-mode-p 'latex-mode)
             (get-buffer latex-toc-buffer-name))
    (latex-toc-refresh)))

(defun latex-toc ()
  "显示LaTeX目录表"
  (interactive)
  (let ((source-buffer (current-buffer))  ; 保存源LaTeX缓冲区
        (buffer (get-buffer-create latex-toc-buffer-name)))
    (display-buffer buffer 
                   '((display-buffer-in-side-window)
                     (side . left)
                     (slot . 1)
                     (window-width . 0.3)))
    
    (with-current-buffer buffer
      (unless (eq major-mode 'latex-toc-mode)
        (latex-toc-mode))
      ;; 设置源缓冲区变量，供其他函数使用
      (setq-local latex-toc-source-buffer source-buffer)
      (latex-toc-refresh))))

(defun latex-toc--find-main-file ()
  "查找LaTeX主文档文件"
  (or
   ;; 策略1: 检查TeX-master变量 (AUCTeX)
   (latex-toc--get-tex-master)
   
   ;; 策略2: 查找包含\documentclass的文件
   (latex-toc--find-by-documentclass)
   
   ;; 策略3: 查找包含\begin{document}的文件  
   (latex-toc--find-by-begin-document)
   
   ;; 策略4: 使用启发式规则查找可能的根文件
   (latex-toc--find-root-heuristic)
   
   ;; 策略5: 回退到当前文件
   (buffer-file-name)))

(defun latex-toc--get-tex-master ()
  "从AUCTeX获取主文档信息或解析文件中的TeX-master注释"
  (or
   ;; 首先尝试从AUCTeX变量获取
   (when (bound-and-true-p TeX-master)
     (if (stringp TeX-master)
         (expand-file-name TeX-master)
       ;; 如果TeX-master是t，说明当前文件是主文档，但我们需要检查文件注释
       nil))
   
   ;; 尝试解析文件中的TeX-master注释
   (latex-toc--parse-tex-master-from-file)))

(defun latex-toc--parse-tex-master-from-file ()
  "从当前缓冲区中解析TeX-master注释"
  (save-excursion
    ;; 直接从当前缓冲区解析，而不是从磁盘文件
    (goto-char (point-min))
    (let ((buffer-content (buffer-string)))
      
      ;; 检查文件末尾内容
      (goto-char (point-max))
      (forward-line -5)  ; 向上查看最后5行
      (let ((end-content (buffer-substring-no-properties (point) (point-max))))
        
        ;; 支持多种TeX-master注释格式
        (let ((result
               (or
                ;; 格式1: 直接搜索TeX-master（带引号）
                (progn
                  (goto-char (point-min))
                  (when (re-search-forward "TeX-master: *\"\\([^\"]+\\)\"" nil t)
                    (match-string 1)))
                
                ;; 格式2: 不带引号的TeX-master
                (progn
                  (goto-char (point-min))
                  (when (re-search-forward "TeX-master: *\\([^ \t\n\"]+\\)" nil t)
                    (match-string 1)))
                
                ;; 格式3: 手动解析，逐行检查
                (progn
                  (goto-char (point-min))
                  (let ((found nil))
                    (while (and (not found) (not (eobp)))
                      (let ((line (buffer-substring-no-properties 
                                   (line-beginning-position) 
                                   (line-end-position))))
                        (when (string-match "TeX-master.*\"\\([^\"]+\\)\"" line)
                          (setq found (match-string 1 line))))
                      (forward-line 1))
                    found)))))
          
          (when result
            ;; 清理结果
            (setq result (string-trim result))
            (setq result (replace-regexp-in-string "[\"']" "" result))
            
            ;; 自动添加.tex扩展名（如果没有的话）
            (unless (string-match "\\.tex$" result)
              (setq result (concat result ".tex")))
            
            ;; 处理相对路径
            (let ((current-dir (file-name-directory (buffer-file-name))))
              (let ((full-path
                     (if (file-name-absolute-p result)
                         result
                       (expand-file-name result current-dir))))
                (setq result full-path))))
          
          result)))))

(defun latex-toc--find-by-documentclass (&optional dir)
  "在目录中查找包含documentclass的文件"
  (let ((dir (or dir default-directory))
        (candidates '()))
    
    ;; 查找所有.tex文件
    (dolist (file (directory-files dir t "\\.tex\\'"))
      (when (latex-toc--file-contains-documentclass file)
        (push file candidates)))
    
    (car (sort candidates #'latex-toc--main-file-priority))))

(defun latex-toc--file-contains-documentclass (file)
  "检查文件是否包含documentclass命令"
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (re-search-forward "\\\\documentclass\\(\\[.*?\\]\\)?{.*?}" nil t)))

(defun latex-toc--find-by-begin-document (&optional dir)
  "查找包含begin{document}的文件"
  (let ((dir (or dir default-directory)))
    (dolist (file (directory-files dir t "\\.tex\\'"))
      (when (latex-toc--file-contains-begin-document file)
        (return file)))))

(defun latex-toc--file-contains-begin-document (file)
  "检查文件是否包含begin{document}"
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (re-search-forward "\\\\begin{document}" nil t)))

(defun latex-toc--find-root-heuristic (&optional dir)
  "使用启发式规则查找根文件"
  (let ((dir (or dir default-directory))
        (candidates '()))
    
    (dolist (file (directory-files dir t "\\.tex\\'"))
      (let ((score (latex-toc--calculate-file-score file)))
        (when (> score 0)
          (push (cons file score) candidates))))
    
    (car (sort candidates (lambda (a b) (> (cdr a) (cdr b)))))))

(defun latex-toc--calculate-file-score (file)
  "计算文件作为主文档的得分"
  (let ((score 0)
        (filename (file-name-nondirectory file)))
    
    ;; 文件名特征加分
    (cond
     ((string-match "main\\|thesis\\|dissertation" filename) (+ score 10))
     ((string-match "report\\|article\\|book" filename) (+ score 5))
     ((string-match "chapter" filename) (+ score 3)))
    
    ;; 文件内容特征加分
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      
      (when (re-search-forward "\\\\documentclass" nil t) (+ score 20))
      (when (re-search-forward "\\\\begin{document}" nil t) (+ score 15))
      (when (re-search-forward "\\\\tableofcontents" nil t) (+ score 10))
      (when (re-search-forward "\\\\includeonly" nil t) (+ score 5))
      
      ;; 包含其他文件减分（可能是子文件）
      (when (re-search-forward "\\\\include\\|\\\\input" nil t) (- score 5)))
    
    score))

(defun latex-toc--find-main-recursive (start-dir &optional max-depth)
  "递归查找主文档"
  (let ((max-depth (or max-depth 3))
        (found nil))
    
    (latex-toc--search-directory start-dir 0 max-depth
                                (lambda (file)
                                  (when (latex-toc--is-likely-main file)
                                    (setq found file)
                                    'stop))))
    found)

(defun latex-toc--search-directory (dir depth max-depth callback)
  "递归搜索目录"
  (when (<= depth max-depth)
    (dolist (file (directory-files dir t))
      (when (and (file-regular-p file)
                 (string-match "\\.tex\\'" file))
        (when (eq (funcall callback file) 'stop)
          (return t)))
      
      (when (and (file-directory-p file)
                 (not (string-match "/\\.\\.?$" file)))
        (latex-toc--search-directory file (1+ depth) max-depth callback)))))

(defvar latex-toc--main-file-cache (make-hash-table :test 'equal)
  "缓存项目目录到主文件的映射")

(defun latex-toc--get-cached-main-file (project-dir)
  "获取缓存的主文件"
  (gethash project-dir latex-toc--main-file-cache))

(defun latex-toc--cache-main-file (project-dir main-file)
  "缓存主文件查找结果"
  (puthash project-dir main-file latex-toc--main-file-cache))

(defun latex-toc--ensure-main-file ()
  "确保找到主文档文件，如果找不到则提示用户"
  (or (latex-toc--find-main-file)
      (let ((main-file (read-file-name 
                        "请指定LaTeX主文档文件: "
                        default-directory nil t nil
                        (lambda (f) (string-match "\\.tex\\'" f)))))
        (when (and main-file (file-exists-p main-file))
          main-file))
      (error "无法确定LaTeX主文档")))

(defun latex-toc--integrate-with-projectile ()
  "与Projectile项目管理工具集成"
  (when (fboundp 'projectile-project-p)
    (when (projectile-project-p)
      (let ((project-root (projectile-project-root)))
        (latex-toc--get-cached-main-file project-root)))))

(provide 'latex-toc)
