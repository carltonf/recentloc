(require 'subr-x)


;;;: Generic helper functions
;; TODO more sanity checks
(defun line-number-at-marker (one-marker)
  "Get the line number of the marker at its corresponding buffer.
Return nil if `one-marker' is not valid."
  (let ((one-buffer (marker-buffer one-marker))
        (one-pos (marker-position one-marker)))
    (if (buffer-live-p one-buffer)
        (with-current-buffer one-buffer
          (line-number-at-pos one-marker))
      (warn "%s is invalid!" one-marker)
      nil)))

(defun pos-in-region-p (pos/marker region)
  "Return true if POS/MARKER is in the REGION. REGION is a
cons (beg . end)."
  (let ((pos (if (markerp pos/marker) (marker-position) pos/marker))
        (beg (car region))
        (end (cdr region)))
    (<= beg pos end)))

;;;: Mode specific setting
(defvar recentloc-context-line-num 5
  "The number of lines around the marker that is considered to be
the context.")

(defun recentloc-get-context-region (pos/marker)
  "Return the context region (BEG . END) around POS/MARKER, i.e.
the +/-`recentloc-context-line-num' lines around the line where
POS/MARKER lies. POS/MARKER can be either a position or marker."
  (with-current-buffer (if (markerp pos/marker)
                           (marker-buffer pos/marker)
                         (current-buffer))
   (let ((min-line 1)
         (max-line (line-number-at-pos (point-max)))
         (marker-line (line-number-at-marker pos/marker))
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
       (goto-char (point-min))
       (forward-line (1- beg-line))
       (setq beg (line-beginning-position))
       (goto-char (point-min))
       (forward-line (1- end-line))
       (setq end (line-end-position)))
     (cons beg end))))

(defun recentloc-search-markers (query-re markers)
  "Search QUERY-RE through MARKERS context, return a list of
matched markers."
  (loop for marker in markers
        when (let ((mk-buf (marker-buffer marker)))
               (when (buffer-live-p mk-buf)
                 (or
                  ;; buffer name matching
                  (string-match-p query-re (buffer-name mk-buf))
                  ;; context searching
                  (let ((context-region (recentloc-get-context-region marker)))
                    (with-current-buffer (marker-buffer marker)
                      (save-excursion
                        (goto-char (car context-region))
                        (re-search-forward query-re (cdr context-region) t)))))))
        collect marker))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;: Main mode facility

;; TODO move to `provide'-related place
(defun recentloc-initializer ()
  "Initialize necessary bits for `recentloc' to work properly."
  ;;: mode line setup
  (unless (member 'recentloc-mode-line-indicator
                  (default-value 'mode-line-buffer-identification))
    (customize-set-variable 'mode-line-buffer-identification
                            (append mode-line-buffer-identification
                                    '(recentloc-mode-line-indicator))))
  ;;: marker manager setup
  ;; don't add to local hook as local and global hooks all get executed
  (add-hook 'pre-command-hook #'recentloc-marker-recorder t)
  (setq recentloc-marker-record-analyzer-idle-timer
        (run-with-idle-timer
         (max recentloc-marker-record-analyzer-idle-delay 2) 'repeat
         #'recentloc-marker-record-analyzer)))

(defun recentloc-deinitializer ()
  "Clean up `recentloc'-related settings."
  (when recentloc-marker-record-analyzer-idle-timer
    (cancel-timer recentloc-marker-record-analyzer-idle-timer))
  (remove-hook 'pre-command-hook #'recentloc-marker-recorder)
  (when (member 'recentloc-mode-line-indicator
                (default-value 'mode-line-buffer-identification))
    (customize-set-variable 'mode-line-buffer-identification
                            (delq 'recentloc-mode-line-indicator
                                  mode-line-buffer-identification))))

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

(defvar recentloc-overlays nil
  "List of overlays of the recentloc buffer.")

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
(put 'recentloc-mode-line-indicator 'risky-local-variable t)

(defun recentloc-update-mode-line-indicator (&optional idx tcount)
  "Helper function to update `recentloc-mode-line-indicator'. IF
IDX or TCOUNT is nil, reset `recentloc-mode-line-indicator'."
  ;; set up local `mode-line-buffer-identification'
  (unless (member 'recentloc-mode-line-indicator mode-line-buffer-identification)
    (setq mode-line-buffer-identification
          (append mode-line-buffer-identification
                  '(recentloc-mode-line-indicator))))
  (if (and idx tcount)
      (progn
        (setq recentloc-mode-line-indicator
              (concat "  "
                      (propertize (format "[%d/%d]" idx tcount)
                                  'face (if (zerop tcount)
                                            'font-lock-warning-face
                                          'font-lock-function-name-face))))
        ;;; Emacs's redisplay is buggy...
        (force-mode-line-update 'all)
        ;;; partial update, effective but leaves some mode line mismatch...
        ;; (when (window-live-p recentloc-buffer-window)
        ;;   (with-current-buffer (window-buffer recentloc-buffer-window)
        ;;     ))
        )
    (setq recentloc-mode-line-indicator nil)))

;;; main command
(defun recentloc-jump ()
  "Master command to jump to a position using `recentloc'"
  (interactive)
  (let* ((all-markers (hash-table-keys recentloc-marker-table))
         (matched-markers all-markers)
         (cur-matched-idx 0)
         cur-marker
         timer
         ;; input should be trimed before stored
         (cur-input "") (last-input "")
         user-query-str)
    (cl-flet* ((matched-cycle-next
                (&optional arg) (interactive "p")
                (let ((matched-mk-len (length matched-markers)))
                  (if (not (zerop matched-mk-len))
                      (progn
                        (setq cur-matched-idx (if (zerop arg)
                                                  0
                                                (mod (funcall (if (> arg 0) #'1+ #'1-)
                                                              cur-matched-idx)
                                                     matched-mk-len))
                              cur-marker (nth cur-matched-idx matched-markers))
                        (recentloc-display-marker-simple cur-marker)
                        ;; TODO add a length limit?
                        (if (s-blank? cur-input)
                            (recentloc-reset-overlays)
                          (recentloc-update-overlays
                           (regexp-opt (split-string cur-input)) cur-marker))
                        (recentloc-update-mode-line-indicator
                         (1+ cur-matched-idx) (length matched-markers)))
                    (recentloc-reset-overlays)
                    ;; no matched marker, only update mode-line
                    (recentloc-update-mode-line-indicator 0 0))))
               (matched-cycle-previous nil (interactive) (matched-cycle-next -1)))
      (unwind-protect
          (minibuffer-with-setup-hook
              (lambda ()
                (setq timer
                      (run-with-idle-timer
                       (max recentloc-input-idle-delay 0.2) 'repeat
                       (lambda ()
                         ;; TODO add guard if previous actions are still
                         ;; ongoing
                         (save-selected-window
                           (with-selected-window (or (active-minibuffer-window)
                                                     (minibuffer-window))
                             (setq cur-input (s-trim (minibuffer-contents-no-properties)))))
                         (if (s-equals? last-input cur-input)
                             ;; do nothing, other than first time boostrap
                             (unless recentloc-buffer-window
                               (matched-cycle-next 0))
                           ;; every time, the initial matched markers is the total
                           (setq matched-markers all-markers)
                           (cond
                            ((s-blank? cur-input)
                             (recentloc-reset-overlays)
                             (matched-cycle-next 0))
                            ;; use white space as query string separator
                            (t
                             (setq last-input cur-input)
                             (loop for single-query being the elements of (split-string cur-input)
                                   do (setq matched-markers
                                            (recentloc-search-markers
                                             (regexp-quote single-query)
                                             matched-markers)))
                             (matched-cycle-next 0))))))))
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
          ;; note in case of cancel in minibuffer reading, the following in
          ;; this level doesn't execute
          (when (markerp cur-marker)
            (unless (loop for win being the elements of (window-list)
                          when (eq (window-buffer win) (marker-buffer cur-marker))
                          do (select-window win))
              (switch-to-buffer (marker-buffer cur-marker)))
            (goto-char cur-marker)
            (recenter)))
        ;; clean up `recentloc' states
        (setq recentloc-buffer-window nil)
        (recentloc-reset-overlays)
        (recentloc-update-mode-line-indicator)))))

;;; TODO move this to user settings
(global-set-key (kbd "C-z m") #'recentloc-jump)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;: Marker Generator & Manager
;;;: Terms:
;; Record: raw markers recorded by `recentloc-marker-recorder'
;; Table: parsed and managed markers, stored in `recentloc-marker-table'

(defvar recentloc-marker-table (make-hash-table :test #'eq)
  "A table to hold all recentloc markers and their metadata.")

(defun recentloc-marker-record-analyzer ()
  "Analyze `recentloc-marker-record-ring' to extract needed
useful markers to `recentloc-marker-table'. NOTE this function also
reset `recentloc-marker-record-ring'."
  ;; TODO consider longer history, currently we only take the tip into
  ;; consideration since this function is run when Emacs is idle with < 3 sec.
  (let* ((record-ring-len (ring-length recentloc-marker-record-ring))
         (curbuf (current-buffer))
         (curpos (point))         
         idx-cmd-mk idx-cmd idx-mk idx-mk-buf idx-mk-region
         (result (catch 'break
                   (loop for idx from 0 below record-ring-len do
                         (setq idx-cmd-mk (ring-ref recentloc-marker-record-ring idx)
                               idx-cmd (car idx-cmd-mk)
                               idx-mk (cdr idx-cmd-mk)
                               idx-mk-buf (marker-buffer idx-mk))
                         (when (buffer-live-p idx-mk-buf)
                           (setq idx-mk-region (recentloc-get-context-region idx-mk))
                           (when (or (not (eq curbuf idx-mk-buf))
                                     ;; outside the region
                                     (not (pos-in-region-p curpos idx-mk-region)))
                             (throw 'break idx-mk)))))))
    (when result
      (recentloc-marker-table-updater result)
      (loop repeat record-ring-len
            do (ring-remove recentloc-marker-record-ring)))))

(defun recentloc-marker-table-updater (arg)
  "Update `recentloc-marker-table' with ARG. ARG for now is only
the marker, but might contain other metadata in the future."
  (let ((pos (marker-position arg))
        (buf (marker-buffer arg))
        (old-hash-keys (hash-table-keys recentloc-marker-table))
        (is-new-marker t))
    (loop for mk in old-hash-keys
          do (let* ((mk-buf (marker-buffer mk))
                    mk-region)
               (if (buffer-live-p mk-buf)
                   ;; not a new mark since an old valid marker covers it
                   (progn
                     (setq mk-region (recentloc-get-context-region mk))
                     (when (and (eq buf mk-buf)
                                (pos-in-region-p pos mk-region))
                       (setq is-new-marker nil)))
                 ;; clean ineffective marker
                 (remhash mk recentloc-marker-table))))
    (when is-new-marker
      ;; TODO replace t with more useful metadata
      (puthash arg t recentloc-marker-table))))

(defvar recentloc-marker-record-analyzer-idle-delay 2
  "The idle time needed for analyzer to generate markers
according to history.")

(defvar recentloc-marker-record-analyzer-idle-timer nil
  "Idle timer for `recentloc-marker-record-analyzer'.")

(defvar recentloc-marker-record-ring (make-ring 64)
  "A ring to record all commands. Each element is a cons cell
like (command-symbol . marker). This is ring is initialized to be
non-empty.")

(defun recentloc-marker-recorder ()
  "Records command execution in `command-frequency-table' hash."
  (unless (or (active-minibuffer-window)
              (helm-alive-p)
              (bound-and-true-p edebug-active)
              (bound-and-true-p company-candidates)
              ;; don't record repeated entries
              (and (not (ring-empty-p recentloc-marker-record-ring))
                   (eq (car (ring-ref recentloc-marker-record-ring 0))
                       real-this-command)))
    (ring-insert recentloc-marker-record-ring
                 (cons real-this-command (point-marker)))
    ;; DBG: this inferences with `transient-mark-mode'
    ;; (let ((record-buf (get-buffer-create " *recentloc-command-records*")))
    ;;   (with-current-buffer record-buf
    ;;     (goto-char (point-max))
    ;;     (insert (symbol-name real-this-command) "-->\n"
    ;;             (pp-to-string (ring-elements recentloc-marker-record-ring))
    ;;             "\n")))
    ))
