;;; TODO more sanity checks

(defun line-number-at-marker (one-marker)
  "Get the line number of the marker at its corresponding buffer."
  (let ((one-buffer (marker-buffer one-marker))
        (one-pos (marker-position one-marker)))
    (with-current-buffer one-buffer
      (line-number-at-pos one-marker))))

;;; search around `line-number-at-marker'
(defvar recentloc-context-line-num 3
  "The number of lines around context that need to be searched for.")

(defun recentloc-get-context-region (one-marker)
  "Return a cons (BEG . END) of the context region around
  ONE-MARKER, i.e. the +/- `recentloc-context-line-num' lines
  around the line where ONE-MARKER lies."
  (with-current-buffer (marker-buffer one-marker)
   (let ((min-line 1)
         (max-line (line-number-at-pos (point-max)))
         (marker-line (line-number-at-marker one-marker))
         beg-line end-line
         beg end)
     (setq beg-line (if (< (- marker-line min-line) recentloc-context-line-num)
                        min-line
                      (- marker-line recentloc-context-line-num)))
     (setq end-line (if (< (- max-line marker-line) recentloc-context-line-num)
                        max-line
                      (+ marker-line recentloc-context-line-num)))
     ;; (cons beg-line end-line)
     (save-excursion
       (goto-line beg-line)
       (setq beg (line-beginning-position))
       (goto-line end-line)
       (setq end (line-end-position)))
     (cons beg end))))

(defvar recentloc-overlays nil
  "List of overlays of the recentloc buffer.")

;;; search through all markers' context
(defun recentloc-search-markers (query-re markers)
  "Search QUERY-RE through MARKERS context, return a list of
matched markers."
  (loop for marker being the elements of markers
        when (let ((context-region (recentloc-get-context-region marker)))
               (with-current-buffer (marker-buffer marker)
                 (save-excursion
                   (goto-char (car context-region))
                   (re-search-forward query-re (cdr context-region) t))))
        collect marker))

;;; 
;; (defun reb-delete-overlays ()
;;   "Delete all RE Builder overlays in the `reb-target-buffer' buffer."
;;   (when (buffer-live-p reb-target-buffer)
;;     (with-current-buffer reb-target-buffer
;;       (mapc 'delete-overlay reb-overlays)
;;       (setq reb-overlays nil))))


(defvar recentloc-input-idle-delay 0.1
  "Be idle for this many seconds before updating recentloc
searching results")

(defvar recentloc-buffer-window nil
  "Currently used window for recentloc.")

(defun recentloc-display-marker-simple (marker)
  "Simple function to display marker in `recentloc' way."
  (let ((buf (marker-buffer marker))
        ;; more reasonable settings, should be global I think?
        (split-height-threshold 48))
    (if (and recentloc-buffer-window
             (window-live-p recentloc-buffer-window))
        (set-window-buffer recentloc-buffer-window buf)
      ;; else create a new window
      (setq recentloc-buffer-window
            (display-buffer buf
                            `(display-buffer-pop-up-window
                              ,`(window-height . ,(+ 1 (* 2 recentloc-context-line-num)
                                                     ;; mode line and the possible header line
                                                     2))))))
    (with-selected-window recentloc-buffer-window
      (goto-char marker)
      (recenter))))

(defun recentloc-reset-overlays ()
  "Reset recentloc overlays."
  (mapc #'delete-overlay recentloc-overlays)
  (setq recentloc-overlays nil))

(defun recentloc-update-overlays (re marker)
  "Update overlays in `recentloc-buffer-window'. Should be used
after `recentloc-display-buffer-simple'."
  ;; TODO colorful highlighting
  (when (and (not (s-blank? re))
             (markerp marker)
             (buffer-live-p (marker-buffer marker)))
    (with-selected-window recentloc-buffer-window
      (let ((context-region (recentloc-get-context-region marker)))
        (recentloc-reset-overlays)
        ;; TODO only for the context
        (goto-char (car context-region))
        (while (re-search-forward re (cdr context-region) t)
          (let ((overlay (make-overlay (match-beginning 0)
                                       (match-end 0)))
                (face 'hi-pink))
            (add-to-list 'recentloc-overlays overlay)
            (overlay-put overlay 'face face)))))))

;; (defvar recentloc-minibuffer-local-map
;;   "Keymap used for minibuffer input for `recentloc'")

(defvar recentloc-mode-line-indicator nil
  "Mode line indicator for `recentloc'.")

(defun recentloc-update-mode-line-indicator (&optional idx tcount)
  "Helper function to update `recentloc-mode-line-indicator'. IF
IDX or TCOUNT is nil, reset `recentloc-mode-line-indicator'."
  (if (and idx tcount)
      (setq recentloc-mode-line-indicator
            (format "[%d/%d]" idx tcount))
    (setq recentloc-mode-line-indicator nil)))

;;; master command
(defun recentloc-jump ()
  "Master command to jump to a position using `recentloc'"
  (interactive)
  (let ((matched-markers (ring-elements my-recentloc-ring))
        (cur-matched-idx 0)
        timer
        ;; input should be trimed before stored
        (cur-input "") (last-input "")
        user-query-str
        need-ace-jump?)
    (cl-flet* ((matched-cycle-next (&optional arg)
                                   (interactive "p")
                                   (setq cur-matched-idx
                                         (if (zerop arg)
                                             0
                                           (mod (funcall (if (> arg 0) #'1+ #'1-) cur-matched-idx)
                                                (length matched-markers))))
                                   (let ((cur-marker (nth cur-matched-idx matched-markers)))
                                     (recentloc-display-marker-simple cur-marker)
                                     ;; TODO add a length limit?
                                     (if (s-blank? cur-input)
                                         (recentloc-reset-overlays)
                                       (recentloc-update-overlays (regexp-opt (split-string cur-input))
                                                                  cur-marker))
                                     (recentloc-update-mode-line-indicator cur-matched-idx
                                                                           (length matched-markers))))
               (matched-cycle-previous nil (interactive) (matched-cycle-next -1)))
      (unwind-protect
          (minibuffer-with-setup-hook
              (lambda ()
                (setq timer
                      (run-with-idle-timer
                       (max recentloc-input-idle-delay 0.01) 'repeat
                       (lambda ()
                         ;; TODO add guard if previous actions are still
                         ;; ongoing
                         (save-selected-window
                           (with-selected-window (or (active-minibuffer-window)
                                                     (minibuffer-window))
                             (setq cur-input (s-trim (minibuffer-contents-no-properties)))))
                         (cond
                          ((s-equals? last-input cur-input)
                           ;;bootstrap
                           (unless recentloc-buffer-window
                             (matched-cycle-next 0)))
                          ((s-blank? cur-input)
                           (setq matched-markers (ring-elements my-recentloc-ring))
                           (recentloc-reset-overlays)
                           (when matched-markers (matched-cycle-next 0)))
                          ;; use white space as query string separator
                          (t
                           (setq last-input cur-input)
                           (loop for single-query being the elements of (split-string cur-input)
                                 do (setq matched-markers
                                          (recentloc-search-markers
                                           (regexp-quote single-query)
                                           matched-markers)))
                           (when matched-markers (matched-cycle-next 0))))))))
            (setq user-query-str (read-from-minibuffer "DWIM-Recentloc: " nil
                                                       (let ((keymap (make-sparse-keymap)))
                                                         (set-keymap-parent keymap minibuffer-local-map)
                                                         (define-key keymap (kbd "C-n") #'matched-cycle-next)
                                                         (define-key keymap (kbd "C-p") #'matched-cycle-previous)
                                                         keymap))))
        ;; clean up timer first
        (when timer (cancel-timer timer))
        ;; if user confirms selection
        (when user-query-str
          (let ((cur-marker (nth cur-matched-idx matched-markers)))
            ;; note in case of cancel in minibuffer reading, the following in
            ;; this level doesn't execute
            ;; no `ace-jump' for fast switching back
            (unless (s-blank? user-query-str)
              (setq need-ace-jump? t))
            (when (markerp cur-marker)
                
              (unless (loop for win being the elements of (window-list)
                            when (eq (window-buffer win) (marker-buffer cur-marker))
                            do (select-window win))
                (switch-to-buffer (marker-buffer cur-marker)))
              (goto-char cur-marker)
              (recenter))))
        ;; clean up `recentloc' states
        (setq recentloc-buffer-window nil)
        (recentloc-reset-overlays)
        (recentloc-update-mode-line-indicator)
        ;; call `ace-jump-mode' to enhance movement
        (when need-ace-jump?
          (call-interactively #'ace-jump-mode))))))

(global-set-key (kbd "C-z m") #'recentloc-jump)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; put `recentloc-mode-line-indicator' to mode line
(customize-set-variable 'mode-line-buffer-identification
                        (append mode-line-buffer-identification '(recentloc-mode-line-indicator)))
(dolist (buf (buffer-list))
  (with-current-buffer buf
    (customize-set-value 'mode-line-buffer-identification
                         (propertized-buffer-identification "%12b"))
    (setq mode-line-buffer-identification
          (append mode-line-buffer-identification
                  '(recentloc-mode-line-indicator)))))
