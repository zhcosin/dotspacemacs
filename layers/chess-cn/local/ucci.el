;;; ucci.el --- UCCI protocol support for Chinese Chess

;;; Commentary:
;; This file implements UCCI (Universal Chinese Chess Interface) protocol
;; support for the chess-cn layer, enabling human vs AI gameplay.

;;; Code:

;; 依赖 chess-cn 模块
(require 'chess-cn)

;; 变量定义
(defvar ucci-engine-process nil
  "UCCI 引擎进程")

(defvar ucci-engine-path "D:/XQWizard/eleeye.exe"
  "UCCI 引擎可执行文件路径")

(defvar ucci-search-depth 8
  "引擎搜索深度")

(defvar ucci-thinking-time 5000
  "引擎思考时间（毫秒）")

(defvar ucci-debug-output nil
  "是否显示调试输出")

(defvar ucci-ai-enabled nil
  "是否启用 AI 对手")

(defvar ucci-ai-side nil
  "AI 控制的一方，'chess-cn--side-red 或 'chess-cn--side-blue")

(defvar chess-cn-after-move-hook nil
  "走子完成后的钩子函数列表")

;; 坐标转换函数
(defun ucci-coord-to-ucci (coord)
  "将内部坐标转换为 UCCI 格式"
  (let ((x (car coord))
        (y (cdr coord)))
    (format "%c%d" (+ ?a x) y)))

(defun ucci-ucci-to-coord (ucci-str)
  "将 UCCI 格式转换为内部坐标"
  (let ((x (- (aref ucci-str 0) ?a))
        (y (- 9 (- (aref ucci-str 1) ?0))))  ; 翻转Y坐标以匹配FEN格式的行序翻转
    (cons x y)))

;; FEN 格式转换
(defun ucci-piece-to-fen-char (piece)
  "将棋子符号转换为 FEN 字符"
  (if (not piece)
      "1"
    (let* ((piece-name (chess-cn--get-piece-name piece))
           (piece-value (symbol-value piece))
           (side (plist-get piece-value 'side)))
      (cond
       ((string= piece-name "帥") (if (eq side 'chess-cn--side-red) "K" "k"))
       ((string= piece-name "將") (if (eq side 'chess-cn--side-red) "K" "k"))
       ((string= piece-name "仕") (if (eq side 'chess-cn--side-red) "A" "a"))
       ((string= piece-name "士") (if (eq side 'chess-cn--side-red) "A" "a"))
       ((string= piece-name "相") (if (eq side 'chess-cn--side-red) "B" "b"))
       ((string= piece-name "象") (if (eq side 'chess-cn--side-red) "B" "b"))
       ((string= piece-name "馬") (if (eq side 'chess-cn--side-red) "N" "n"))
       ((string= piece-name "車") (if (eq side 'chess-cn--side-red) "R" "r"))
       ((string= piece-name "炮") (if (eq side 'chess-cn--side-red) "C" "c"))
       ((string= piece-name "砲") (if (eq side 'chess-cn--side-red) "C" "c"))
       ((string= piece-name "兵") (if (eq side 'chess-cn--side-red) "P" "p"))
       ((string= piece-name "卒") (if (eq side 'chess-cn--side-red) "P" "p"))
       (t "1")))))

(defun ucci-situation-to-fen (situation)
  "将棋局状态转换为 FEN 格式"
  (let ((fen-rows '())
        (current-side (plist-get chess-cn--playing 'curt-side)))
    ;; 从第0行到第9行（chess-cn坐标系，红方在下方第9行）
    (dotimes (y 10)
      (let ((actual-row y)  ; 保持原始行序，稍后通过reverse翻转
            (row-str "")
            (empty-count 0))
        (dotimes (x 9)
          (let* ((coord (cons x actual-row))
                 (piece (chess-cn--get-piece-from-situation coord))
                 (fen-char (ucci-piece-to-fen-char piece)))
            (if (string= fen-char "1")
                (setq empty-count (1+ empty-count))
              (when (> empty-count 0)
                (setq row-str (concat row-str (number-to-string empty-count)))
                (setq empty-count 0))
              (setq row-str (concat row-str fen-char)))))
        (when (> empty-count 0)
          (setq row-str (concat row-str (number-to-string empty-count))))
        (push row-str fen-rows)))
    
    ;; 构建完整的 FEN 字符串
    (let ((board-fen (mapconcat 'identity (reverse fen-rows) "/"))  ; 翻转行序以符合标准FEN格式
          (active-color (if (eq current-side 'chess-cn--side-red) "w" "b"))
          (castling "-")
          (en-passant "-")
          (halfmove "0")
          (fullmove "1"))
      (format "%s %s %s %s %s %s" board-fen active-color castling en-passant halfmove fullmove))))

;; 引擎通信
(defun ucci-send-command (command)
  "向引擎发送命令"
  (when (and ucci-engine-process (process-live-p ucci-engine-process))
    (when ucci-debug-output
      (message "发送: %s" command))
    (process-send-string ucci-engine-process (concat command "\n"))))

;; 处理引擎输出的全局变量，防止重复处理同一个 bestmove
(defvar ucci-processing-move nil "标记是否正在处理引擎走法")
(defvar ucci-last-processed-move nil "记录最后处理的走法，防止重复处理")

(defun ucci-process-filter (process output)
  "处理引擎输出"
  (dolist (line (split-string output "\n" t))
    (let ((line (string-trim line)))
      (cond
       ;; 引擎准备就绪
       ((string-match "^ucciok" line)
        (message "UCCI 引擎已准备就绪"))
       
       ;; 引擎走法
       ((string-match "^bestmove \\([a-i][0-9][a-i][0-9]\\)" line)
        (let ((move (match-string 1 line)))
          (when (and (not ucci-processing-move)
                     (not (string= move ucci-last-processed-move)))
            (setq ucci-processing-move t)
            (setq ucci-last-processed-move move)
            (ucci-apply-engine-move move)
            (setq ucci-processing-move nil))))
       
       ;; 无走法
       ((string-match "^nobestmove" line)
        (message "引擎无法找到走法，可能是局面有问题"))))))

(defun ucci-process-sentinel (process event)
  "处理引擎进程状态变化"
  (when (string-match "\\(finished\\|exited\\)" event)
    (setq ucci-engine-process nil)
    (setq ucci-ai-enabled nil)
    (message "UCCI 引擎已停止")))

;; 引擎管理
(defun ucci-start-engine ()
  "启动 UCCI 引擎"
  (interactive)
  (when (and ucci-engine-process (process-live-p ucci-engine-process))
    (ucci-stop-engine))
  
  (condition-case err
      (progn
        (setq ucci-engine-process
              (start-process "ucci-engine" "*ucci-engine*" ucci-engine-path))
        (set-process-filter ucci-engine-process 'ucci-process-filter)
        (set-process-sentinel ucci-engine-process 'ucci-process-sentinel)
        (sleep-for 0.5)
        (ucci-send-command "ucci")
        (message "正在启动 UCCI 引擎..."))
    (error
     (message "启动引擎失败: %s" (error-message-string err)))))

(defun ucci-stop-engine ()
  "停止 UCCI 引擎"
  (interactive)
  (when (and ucci-engine-process (process-live-p ucci-engine-process))
    (ucci-send-command "quit")
    (delete-process ucci-engine-process)
    (setq ucci-engine-process nil)
    (setq ucci-ai-enabled nil)
    (message "UCCI 引擎已停止")))

(defun ucci-engine-status ()
  "显示引擎状态"
  (interactive)
  (if (and ucci-engine-process (process-live-p ucci-engine-process))
      (message "引擎状态: 运行中 | AI: %s | AI方: %s"
               (if ucci-ai-enabled "启用" "禁用")
               (cond
                ((eq ucci-ai-side 'chess-cn--side-red) "红方")
                ((eq ucci-ai-side 'chess-cn--side-blue) "黑方")
                (t "未设置")))
    (message "引擎状态: 未运行")))

;; 走法处理
(defun ucci-get-engine-move ()
  "获取引擎走法"
  (interactive)
  (ucci-request-move))

(defun ucci-request-move ()
  "请求引擎走法"
  (when (and ucci-engine-process 
             (process-live-p ucci-engine-process)
             (not ucci-processing-move))  ; 只有在不处理走法时才请求新走法
    (setq ucci-last-processed-move nil)  ; 清除上次处理的走法记录
    (let ((fen (ucci-situation-to-fen (plist-get chess-cn--playing 'situation))))
      (ucci-send-command (format "position fen %s" fen))
      (ucci-send-command (format "go time %d" ucci-thinking-time)))))

(defun ucci-apply-engine-move (move-str)
  "应用引擎走法到棋局"
  (let* ((from-coord (ucci-ucci-to-coord (substring move-str 0 2)))
         (to-coord (ucci-ucci-to-coord (substring move-str 2 4)))
         (piece-at-from (chess-cn--get-piece-from-situation from-coord))
         (piece-at-to (chess-cn--get-piece-from-situation to-coord))
         (current-side-before (plist-get chess-cn--playing 'curt-side)))
    (when piece-at-from
      ;; 检查棋子是否属于当前方
      (let ((piece-side (plist-get (symbol-value piece-at-from) 'side)))
        (if (eq piece-side current-side-before)
            (progn
              ;; 直接进行棋子移动
              (let ((moved-piece (chess-cn--get-piece-from-situation from-coord)))
                (if piece-at-to
                    ;; 吃子操作
                    (progn
                      (chess-cn--remove-piece-impl piece-at-to to-coord)
                      (chess-cn--move-piece-impl moved-piece from-coord to-coord)
                      (chess-cn--push-history from-coord to-coord moved-piece piece-at-to))
                  ;; 移动操作
                  (progn
                    (chess-cn--move-piece-impl moved-piece from-coord to-coord)
                    (chess-cn--push-history from-coord to-coord moved-piece nil)))
                ;; 清除选中状态
                (plist-put chess-cn--playing 'curt-selected-cord nil)
                ;; 切换走子方 - 引擎走子后应该切换到对方
                (plist-put chess-cn--playing 'curt-side (chess-cn--get-other-side current-side-before))
                ;; 重新绘制棋盘
                (chess-cn--draw-board-by-situation (plist-get chess-cn--playing 'situation))
                (message "AI 走子: %s" move-str))))
    (unless piece-at-from
      (message "错误：起始位置 %s 没有棋子！" from-coord))))))

(defun ucci-get-move-history ()
  "获取走法历史"
  (let ((history (plist-get chess-cn--playing 'history))
        (moves '()))
    (dolist (step history)
      (let* ((from-coord (plist-get step 'oldcord))
             (to-coord (plist-get step 'dstcord))
             (move-str (concat (ucci-coord-to-ucci from-coord)
                              (ucci-coord-to-ucci to-coord))))
        (push move-str moves)))
    (reverse moves)))

;; 人机对战模式
(defun ucci-toggle-ai ()
  "切换 AI 模式"
  (interactive)
  (if ucci-ai-enabled
      (progn
        (setq ucci-ai-enabled nil)
        (message "AI 模式已禁用"))
    (if (and ucci-engine-process (process-live-p ucci-engine-process))
        (progn
          (setq ucci-ai-enabled t)
          (unless ucci-ai-side
            (setq ucci-ai-side 'chess-cn--side-blue))
          (message "AI 模式已启用，AI 控制 %s"
                   (if (eq ucci-ai-side 'chess-cn--side-red) "红方" "黑方"))
          ;; 如果当前轮到 AI，立即走子
          (ucci-check-ai-turn))
      (message "请先启动引擎"))))

(defun ucci-set-ai-side (side)
  "设置 AI 控制的一方"
  (interactive
   (list (intern (completing-read "选择 AI 方: "
                                  '("chess-cn--side-red" "chess-cn--side-blue")
                                  nil t))))
  (setq ucci-ai-side side)
  (message "AI 现在控制 %s" (if (eq side 'chess-cn--side-red) "红方" "黑方")))

(defvar ucci-hook-running nil "防止钩子重复执行的标记")

(defun ucci-check-ai-turn ()
  "检查是否轮到 AI 走子"
  ;; 防止重复执行
  (unless ucci-hook-running
    (setq ucci-hook-running t)
    (when (and ucci-ai-enabled
               ucci-ai-side
               (eq (plist-get chess-cn--playing 'curt-side) ucci-ai-side)
               (not (plist-get chess-cn--playing 'game-over))
               (not ucci-processing-move))  ; 防止在处理走法时重复触发
      (ucci-request-move))
    ;; 使用定时器重置标记，确保下次走子时可以正常执行
    (run-with-timer 0.1 nil (lambda () (setq ucci-hook-running nil)))))

;; 配置和设置
(defun ucci-set-thinking-time (time)
  "设置引擎思考时间"
  (interactive "n思考时间（毫秒）: ")
  (setq ucci-thinking-time time)
  (message "引擎思考时间设置为 %d 毫秒" time))

(defun ucci-set-engine-option (option value)
  "设置引擎选项"
  (interactive "s选项名: \ns选项值: ")
  (ucci-send-command (format "setoption name %s value %s" option value)))

(defun ucci-toggle-debug ()
  "切换调试输出"
  (interactive)
  (setq ucci-debug-output (not ucci-debug-output))
  (message "调试输出: %s" (if ucci-debug-output "开启" "关闭")))

;; 快捷命令
(defun ucci-start-human-vs-ai ()
  "开始人机对战"
  (interactive)
  (chess-cn--new)
  (unless (and ucci-engine-process (process-live-p ucci-engine-process))
    (ucci-start-engine)
    (sleep-for 1))
  (setq ucci-ai-enabled t)
  (setq ucci-ai-side 'chess-cn--side-blue)
  (message "人机对战开始！你执红方，AI 执黑方"))

(defun ucci-get-hint ()
  "获取走法提示"
  (interactive)
  (when (and ucci-engine-process (process-live-p ucci-engine-process))
    (let ((fen (ucci-situation-to-fen (plist-get chess-cn--playing 'situation))))
      (ucci-send-command (format "position fen %s" fen))
      (ucci-send-command "go depth 3")
      (message "正在获取走法提示..."))))

;; 初始化和清理
(defvar ucci-initialized nil "标记 UCCI 是否已初始化")

(defun ucci-init ()
  "初始化 UCCI 模块"
  (unless ucci-initialized
    ;; 完全清理钩子，然后重新添加
    (setq chess-cn-after-move-hook nil)
    (add-hook 'chess-cn-after-move-hook 'ucci-check-ai-turn)
    (add-hook 'kill-emacs-hook 'ucci-cleanup)
    (setq ucci-initialized t)))

(defun ucci-cleanup ()
  "清理 UCCI 模块"
  (when (and ucci-engine-process (process-live-p ucci-engine-process))
    (ucci-stop-engine)))

;; 自动初始化 - 恢复自动初始化
(eval-after-load 'chess-cn
  '(unless (memq 'ucci-check-ai-turn chess-cn-after-move-hook)
     (ucci-init)))

(provide 'ucci)

;;; ucci.el ends here