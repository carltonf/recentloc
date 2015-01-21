;;; Notions:
;;;
;;; Regmks: Region Markers, a pair of markers that designate a region, which CAR
;;; be the at the starting point of the region (beginning-of-line) and the CDR
;;; be the ending point of the region (end-of-line).


(eval-when-compile (require 'cl))
(require 'subr-x)
(require 's)
(require 'dash)
(require 'eieio)

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
(defconst recentloc-context-line-num 5
  "The number of lines around the marker that is considered to be
the context.")

(defun recentloc-get-context-regmks (marker)
  "Return the context Regmks around MARKER, i.e.
  the +/-`recentloc-context-line-num' lines around the line where
  MARKER lies."
  (let (begm endm)
    (save-excursion
      (set-buffer (marker-buffer marker))
      (goto-char marker)
      (beginning-of-line (- recentloc-context-line-num))
      (setq begm (point-marker))
      (goto-char marker)
      (end-of-line recentloc-context-line-num)
      (setq endm (point-marker)))
    (cons begm endm)))

(defun recentloc-get-context-region (marker)
  "Return the context region (BEG . END) around MARKER, i.e.
the +/-`recentloc-context-line-num' lines around the line where
MARKER lies."
  (let ((regmks (recentloc-get-context-regmks marker)))
    (cons (marker-position (car regmks))
          (marker-position (cdr regmks)))))

(defun recentloc-search-markers (query-re markers)
  "Search QUERY-RE through MARKERS context, return a list of
matched markers. If QUERY-RE is empty, it's considered all match. "
  (let* ((matched-markers
          (loop for marker in markers
                when (let ((mk-buf (marker-buffer marker))
                           (mk-meta (gethash marker recentloc-marker-table)))
                       (when (buffer-live-p mk-buf)
                         (or (s-blank? query-re)
                             (s-contains? query-re (oref mk-meta :context)))))
                collect marker)))
    matched-markers))

(defun recentloc--sort-matched-markers (matched-markers)
  "Sorting MATCHED-MARKERS. Use this function before displaying
any `recentloc' candidates to get sane results."
  (sort* matched-markers
         (lambda (m1 m2)
           (let ((m1-meta (gethash m1 recentloc-marker-table))
                 (m2-meta (gethash m2 recentloc-marker-table)))
             (not (time-less-p (oref m1-meta :timestamp)
                               (oref m2-meta :timestamp)))))))

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
         (max recentloc-marker-record-analyzer-idle-delay 1) 'repeat
         #'recentloc-marker-record-analyzer))
  (setq recentloc-table-maintainer-idle-timer
        (run-with-idle-timer
         (max recentloc-table-maintainer-idle-delay 3) 'repeat
         #'recentloc-table-maintainer)))

(defun recentloc-deinitializer ()
  "Clean up `recentloc'-related settings."
  (when recentloc-table-maintainer-idle-timer
    (cancel-timer recentloc-table-maintainer-idle-timer))
  (when recentloc-marker-record-analyzer-idle-timer
    (cancel-timer recentloc-marker-record-analyzer-idle-timer))
  (remove-hook 'pre-command-hook #'recentloc-marker-recorder)
  (when (member 'recentloc-mode-line-indicator
                (default-value 'mode-line-buffer-identification))
    (customize-set-variable 'mode-line-buffer-identification
                            (delq 'recentloc-mode-line-indicator
                                  mode-line-buffer-identification))))

(defconst recentloc-input-idle-delay 0.1
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

;;; TODO make this mode related
(defvar recentloc-jump-alive-p nil
  "Indicator whether `recentloc-jump' is active.")

(lexical-let ((cur-matched-idx 0))
  (defun recentloc-jump-cycle-matched-markers (matched-markers cur-input mode)
    "A function used in `recentloc-jump' to cycle
through MATCHED-MARKERS under the CUR-INPUT. An internal counter
is used.

MODE is a symbol defines the action takes:
'reset: shows the first matched marker, reset the internal counter.
'next: cycle to the next one.
'prev: cycle to the previous one."
    (unless recentloc-jump-alive-p
      (error "Should only be called while `recentloc-jump' is alive."))
    (let ((matched-mk-len (length matched-markers))
          cur-marker)
      (if (not (zerop matched-mk-len))
          (progn
            (setq cur-matched-idx (case mode
                                    ('reset 0)
                                    ('next (ring-plus1 cur-matched-idx matched-mk-len))
                                    ('prev (ring-minus1 cur-matched-idx matched-mk-len))
                                    ;; other mode code is not defined, no
                                    ;; cycling happens
                                    (t
                                     (warn "'%s' is invalid mode code!"
                                           (pp-to-string mode))
                                     cur-matched-idx))
                  cur-marker (nth cur-matched-idx matched-markers))
            (recentloc-display-marker-simple cur-marker)
            ;; TODO add a length limit?
            (if (s-blank? cur-input)
                (recentloc-reset-overlays)
              (recentloc-update-overlays
               (regexp-opt (split-string cur-input)) cur-marker))
            (recentloc-update-mode-line-indicator
             (1+ cur-matched-idx) matched-mk-len))
        ;; no matched marker, only update overlays and mode-line
        (recentloc-reset-overlays)
        (recentloc-update-mode-line-indicator 0 0))))
  ;;
  (defun recentloc-jump-cycle-chosen-marker (matched-markers)
    "Helper function to return the currently chosen marker"
    (nth cur-matched-idx matched-markers)))

;;; main command
(defvar recentloc-debug-data nil
  "Helper variable that collects some debug data.")

(defun recentloc-jump ()
  "Master command to jump to a position using `recentloc'"
  (interactive)
  ;; don't cache all markers as multiple timers might invalidate the cache
  (let* ((cursor-buffer (current-buffer))
         (cursor-region (recentloc-get-context-region (point-marker)))
         matched-markers
         chosen-marker
         timer
         ;; input should be trimed before stored
         (cur-input "") (last-input "")
         user-query-str)
    (cl-flet ((all-sane-markers ()
               "Return all markers that are considered sane, i.e. exclude the marker enclose the current cursor."
               (loop for mk in (hash-table-keys recentloc-marker-table)
                     unless (and (eq cursor-buffer (marker-buffer mk))
                                 (pos-in-region-p (marker-position mk)
                                                  cursor-region))
                     collect mk)))
      (setq matched-markers (all-sane-markers))
      (unwind-protect
          (progn
            (setq recentloc-jump-alive-p t)
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
                               ;; do nothing, other than first time bootstrap
                               (unless recentloc-buffer-window
                                 (setq matched-markers (recentloc--sort-matched-markers matched-markers))
                                 (recentloc-jump-cycle-matched-markers matched-markers cur-input 'reset))
                             (cond
                              ((s-blank? cur-input)
                               (setq matched-markers (all-sane-markers))
                               (setq matched-markers (recentloc--sort-matched-markers matched-markers))
                               (recentloc-jump-cycle-matched-markers matched-markers cur-input 'reset))
                              ;; use white space as query string separator
                              (t
                               ;; if not incremental search, reset markers
                               ;; NOTE: empty last input is always the prefix, so skip it
                               (unless (and (not (s-blank? last-input))
                                            (s-prefix? last-input cur-input)
                                            (-is-prefix? (split-string last-input)
                                                         (split-string cur-input)))
                                 (setq matched-markers (all-sane-markers)))
                               (loop for single-query in (split-string cur-input)
                                     do (setq matched-markers
                                              (recentloc-search-markers (regexp-quote single-query)
                                                                        matched-markers)))
                               (setq matched-markers (recentloc--sort-matched-markers matched-markers))
                               (recentloc-jump-cycle-matched-markers matched-markers cur-input 'reset))))
                           ;; always sync last-input regardless
                           (setq last-input cur-input)))))
              (setq user-query-str (read-from-minibuffer
                                    "DWIM-Recentloc: " nil
                                    (let ((keymap (make-sparse-keymap)))
                                      (set-keymap-parent keymap minibuffer-local-map)
                                      (define-key keymap (kbd "C-n")
                                        (lambda () (interactive)
                                          (recentloc-jump-cycle-matched-markers matched-markers cur-input 'next)))
                                      (define-key keymap (kbd "C-p")
                                        (lambda () (interactive)
                                          (recentloc-jump-cycle-matched-markers matched-markers cur-input 'prev)))
                                      (define-key keymap (kbd "C-d")
                                        (lambda () (interactive)
                                          (setq recentloc-debug-data
                                                (cons (recentloc-jump-cycle-chosen-marker matched-markers)
                                                      recentloc-debug-data))))
                                      keymap)))))
        ;; clean up timer first
        (when timer (cancel-timer timer))
        ;; if user confirms selection
        (when user-query-str
          ;; note in case of cancel in minibuffer reading, the following in
          ;; this level doesn't execute
          (setq chosen-marker (recentloc-jump-cycle-chosen-marker matched-markers))
          (when (markerp chosen-marker)
            (unless (loop for win in (window-list)
                          when (eq (window-buffer win) (marker-buffer chosen-marker))
                          do (select-window win))
              (switch-to-buffer (marker-buffer chosen-marker)))
            ;; jump to the first matched string (better than goto the forgotten marker position)
            (let ((region (recentloc-get-context-region chosen-marker)))
              (goto-char (car region))
              (re-search-forward (regexp-opt (split-string cur-input)) (cdr region))
              (recenter))))
        ;; clean up `recentloc' states
        (setq recentloc-buffer-window nil)
        (recentloc-reset-overlays)
        (recentloc-update-mode-line-indicator)
        (setq recentloc-jump-alive-p nil)))))

;;; TODO move this to user settings
(global-set-key (kbd "C-z m") #'recentloc-jump)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;: Marker Generator & Manager
;;;: Terms:
;; Record: raw markers recorded by `recentloc-marker-recorder'
;; Table: parsed and managed markers, stored in `recentloc-marker-table'

(defvar recentloc-marker-table (make-hash-table :test #'eq
                                                :size 256)
  "A table to hold all recentloc markers and their metadata.")

(defvar recentloc-buffer-markers-table (make-hash-table :test #'eq
                                                       :size 128)
  "A table to hold buffer-marker_list pair. Mainly used for
speeding up `recentloc-marker-record-analyzer' and
`recentloc-marker-table-updater'.")

(defun recentloc-marker-record-analyzer ()
  "Analyze `recentloc-marker-record-ring' to extract needed
useful markers to `recentloc-marker-table'. NOTE this function also
reset `recentloc-marker-record-ring'.

Reverse look through the `recentloc-marker-record-ring' and
compare it with current position. If a \"significant different\"
marker is found, record this marker."
  ;; TODO consider longer history, currently we only take the tip into
  ;; consideration since this function is run when Emacs is idle with < 3 sec.
  (unless (active-minibuffer-window)    ;ignore minibuffer
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
              do (ring-remove recentloc-marker-record-ring))))))

(defconst recentloc-marker-buffer-idle-cleaner-delta-delay-limit 5
  "Delta Idle Delay limit used by
`recentloc-marker-buffer-idle-cleaner' to start worker timer with
a random delta delay.")

(defun recentloc-marker-buffer-idle-cleaner (buf)
  "Remove BUF from `recentloc-buffer-markers-table' and all
related markers from `recentloc-marker-table'. The function runs
an one-shot idle timer to do the real work. The delta delay used
is randomized to create a \"balanced\" workload. The delta limit
is `recentloc-marker-buffer-idle-cleaner-delta-delay-limit'.

Supposed to used by other timers."
  (cl-flet ((cleanup-buf-mks (buf)
                             (loop for mk in (gethash buf recentloc-buffer-markers-table)
                                   do (remhash mk recentloc-marker-table))
                             (remhash buf recentloc-buffer-markers-table)))
    (run-with-idle-timer
     (time-add (or (current-idle-time) (seconds-to-time 0))
               (seconds-to-time (1+ (random recentloc-marker-buffer-idle-cleaner-delta-delay-limit))))
     nil
     #'cleanup-buf-mks buf)))

(defconst recentloc-marker-buffer-metadata-idle-updater-delta-delay 3
  "Delta Idle Delay limit used by
`recentloc-marker-buffer-metadata-idle-updater' to start worker
timer with a random delta delay.")

(defun recentloc-marker-buffer-metadata-idle-updater (buf)
  "Update the metadata of all markers associated with BUF in
`recentloc-marker-table' if necessary."
  (if (not (buffer-live-p buf))
      ;; as this is an idle timer, something might happen before this timer
      ;; starts
      (recentloc-marker-buffer-idle-cleaner buf)
    (let ((buf-mk-lst (gethash buf recentloc-buffer-markers-table)))
      (setq buf-mk-lst
            (loop for curmk in buf-mk-lst
                  by (lambda (rest-buf-mk-lst)
                       (setq rest-buf-mk-lst (cdr rest-buf-mk-lst))
                       (let* ((curmk-region (recentloc-get-context-region curmk))
                              (curmk-meta (gethash curmk recentloc-marker-table))
                              ;; timestamp
                              (curmk-time (oref curmk-meta :timestamp))
                              (new-curmk-time curmk-time)
                              mk-time
                              ;; position and context
                              (old-curmk-reglen (oref curmk-meta :reglen))
                              (old-curmk-regmks (oref curmk-meta :regmks)))
                         ;; check the rest of markers behind `curmk'
                         (loop for mk in rest-buf-mk-lst
                               while (pos-in-region-p (marker-position mk)
                                                      curmk-region)
                               ;; merging markers that are too close, use the most
                               ;; up-to-date timestamp. (This happens as the user
                               ;; editing buffers)
                               do (progn
                                    (setq mk-time (oref (gethash mk recentloc-marker-table)
                                                        :timestamp))
                                    (when (time-less-p curmk-time mk-time)
                                      (setq new-curmk-time mk-time))
                                    (setq rest-buf-mk-lst (cdr rest-buf-mk-lst))
                                    (remhash mk recentloc-marker-table)))
                         ;; all-in-one metadata update
                         (let ((new-timestamp (when (time-less-p curmk-time new-curmk-time)
                                                new-curmk-time))
                               (pos/context-update-p (not (= old-curmk-reglen
                                                             (- (cdr old-curmk-regmks)
                                                                (car old-curmk-regmks))))))
                           (when (or new-timestamp pos/context-update-p)
                             (recentloc--marker-table-update-metadata
                              curmk (not pos/context-update-p) new-timestamp))))
                       ;; remaining buffer marker list
                       rest-buf-mk-lst)
                  collect curmk))
      (puthash buf buf-mk-lst recentloc-buffer-markers-table))))

(defclass recentloc-marker-metadata ()
  ((context :initarg :context
            :initform ""
            :type string
            :documentation
            "Context strings cached to speed up searching.")
   (timestamp :initarg :timestamp
              :initform (seconds-to-time 0)
              :type (satisfies listp)
              :documentation
              "Last visited timestamp, format is the same as `current-time'")
   (regmks :initarg :regmks
           :initform (nil . nil)
           :type (satisfies consp)
           :documentation
           "A cons cell. Region Markers around the Marker. Used
           to calculate region length.")
   (reglen :initarg :reglen
           :initform 0
           :type number
           :documentation
           "A number calculated from `:regmks'. Used to indicate
          whether this region has been changed and thus grant an
          update in the cache. ")
   (count     :initarg :count
              :initform 1
              :type number
              :documentation
              "The number of visits.")
   (autotag   :initarg :autotag
              :initform (make-ring 8)
              :type (satisfies ring-p)
              :documentation
              "Tokenized string list that have been used to
              switch to this marker")
   ;; TODO needs a way to auto-update this in OO way
   (score     :initarg :score
              :initform 0
              :type number
              :documentation
              "Quantified priority in the search result. Used for
              sorting."))
  "Metadata in `recentloc-marker-table'.")

(defun recentloc--marker-table-update-metadata (marker &optional not-context
                                                       timestamp update-count-p
                                                       autotag)
  "Update metadata in `recentloc-marker-table' for MARKER. If
MARKER is not in the table yet, add it.

Default to update context and pos only if NOT-CONTEXT is unset or
nil. If TIMESTAMP, UPDATE-COUNT-P or AUTOTAG is non-nil update
timestamp, count and autotag respectively."
  (let ((buf (marker-buffer marker))
        regmks beg end
        (metadata (or (gethash marker recentloc-marker-table)
                      (recentloc-marker-metadata "metadata"))))
    (if (not (buffer-live-p buf))
        (recentloc-marker-buffer-idle-cleaner buf)
      ;; update context and regmks
      (unless not-context
        (with-current-buffer buf
          (setq regmks (recentloc-get-context-regmks marker)
                beg (marker-position (car regmks))
                end (marker-position (cdr regmks)))
          (oset metadata :regmks regmks)
          (oset metadata :reglen (- end beg))
          (oset metadata
                :context (s-concat
                          (format "'%s' '%s' "
                                  (buffer-name)
                                  (or (which-function) ""))
                          (buffer-substring-no-properties beg end)))))
      (when timestamp
        (oset metadata :timestamp timestamp))
      (when update-count-p
        (oset metadata :count (1+ (oref metadata :count))))
      ;; TODO needs a OO way of updating the field s.t. the details of ring data
      ;; structure is hidden from here
      ;; (when autotag
      ;;   (oset metadata :autotag))
      (puthash marker metadata recentloc-marker-table))))

(defun recentloc-marker-table-updater (marker)
  "Update `recentloc-marker-table' with MARKER. Usually MARKER is
from `recentloc-marker-record-ring'."
  (let* ((pos (marker-position marker))
         region
         (buf (marker-buffer marker))
         (buf-mk-lst (gethash buf recentloc-buffer-markers-table))
         ;; marker index
         (mk-idx 0)
         (new-marker-p t))
    (if (buffer-live-p buf)
        (progn
          (loop for mk in buf-mk-lst
                until (if (pos-in-region-p pos (recentloc-get-context-region mk))
                          ;;: old marker, update timestamp only (not context)
                          (progn
                            (setq new-marker-p nil)
                            (recentloc--marker-table-update-metadata mk t (current-time))
                            t)
                        ;;: new marker
                        ;; the first MK after MARKER and not cover MARKER, and
                        ;; thus MARKER is new and should be inserted just before
                        ;; MK. (buf-mk-lst should be sorted)
                        (when (> (marker-position mk) pos)
                          t))
                do (setq mk-idx (1+ mk-idx)))
          (when new-marker-p
            (recentloc--marker-table-update-metadata marker nil (current-time))
            (setq buf-mk-lst (-insert-at mk-idx marker buf-mk-lst))
            (puthash buf buf-mk-lst recentloc-buffer-markers-table))
          ;; a marker change is found in BUF, check others
          ;; TODO Can we further reduce the workload? Only relevant markers.
          (run-with-idle-timer
           (time-add
            (or (current-idle-time) (seconds-to-time 0))
            (seconds-to-time
             recentloc-marker-buffer-metadata-idle-updater-delta-delay))
           nil
           #'recentloc-marker-buffer-metadata-idle-updater buf))
      ;; buffer is no longer valid
      (recentloc-marker-buffer-idle-cleaner buf))))

(defconst recentloc-table-maintainer-idle-delay 3
  "The idle time before starting `recentloc-table-maintainer'.")

(defvar recentloc-table-maintainer-idle-timer nil
  "Idle timer for `recentloc-table-maintainer'")

(defun recentloc-table-maintainer ()
  "An idle timer to maintain `recentloc' data, includes:
`recentloc-marker-table'
`recentloc-buffer-markers-table'.

The responsibility of the maintainer is:
1. Clean up all markers associated with dead/gone buffers.
2. TODO fast algorithms to update metadata to speed up searching.

NOTE: this function shouldn't take too long and hard tasks ought
  to split up."
  (loop for buf in (hash-table-keys recentloc-buffer-markers-table)
        ;; validate buffers
        do (if (not (buffer-live-p buf))
               (recentloc-marker-buffer-idle-cleaner buf)
             ;; the marker at position 1 is usually meaningless
             (let* ((buf-mk-lst (gethash buf recentloc-buffer-markers-table))
                    (first-marker (car buf-mk-lst)))
               (cond
                ;; TODO first-marker should not be null at this place
                ((null first-marker)
                 (recentloc-marker-buffer-idle-cleaner buf))
                ((= 1 (marker-position first-marker))
                 (remhash first-marker recentloc-marker-table)
                 (if (cdr buf-mk-lst)
                     (puthash buf (cdr buf-mk-lst) recentloc-buffer-markers-table)
                   ;; no marker left, remove buf
                   (remhash buf recentloc-buffer-markers-table))))))))

(defconst recentloc-marker-record-analyzer-idle-delay 1
  "The idle time needed for analyzer to generate markers
according to history.")

(defvar recentloc-marker-record-analyzer-idle-timer nil
  "Idle timer for `recentloc-marker-record-analyzer'.")

(defvar recentloc-marker-record-ring (make-ring 64)
  "A ring to record all commands. Each element is a cons cell
like (command-symbol . marker). This is ring is initialized to be
non-empty.")

(defvar recentloc-marker-recorder-ignore-buffers "\\`[ *]"
  "Regexp matching the names of buffers to ignore.")

(defun recentloc-marker-recorder ()
  "Records command execution in `command-frequency-table' hash."
  (unless (or (active-minibuffer-window)
              ;; (helm-alive-p)
              ;; ignore help mode for now (lines can be too long)
              ;; (derived-mode-p 'help-mode)
              ;; info marker tends to lose meanings (the way `Info-mode' narrow
              ;; regions for displaying), it needs bookmark-like marker. Ignore
              ;; it for now.
              ;; (derived-mode-p 'Info-mode)
              ;; follow the buffer listing tradition to ignore certain buffers
              (let ((buf-name (buffer-name)))
                (and (not (s-equals? buf-name "*scratch*"))
                     (not (s-equals? buf-name "*scratch-text*"))
                     (string-match recentloc-marker-recorder-ignore-buffers buf-name)))
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;: Test area.
;;; Useful code for debugging, copy to other buffers to use them.

(defun recentloc-data-reset ()
  "Reset all `recentloc' metadata. This includes the metadata
part in `recentloc-marker-table' and all of
`recentloc-buffer-markers-table'.

Mainly used when the data structures are changed during the
development.

More about data structure clarity than efficiency. Try to make
every block self-contained (block as defined by ';;:')."

  ;;: regenerate the metadata part of `recentloc-marker-table', only context for
  ;; now
  (loop for mk in (hash-table-keys recentloc-marker-table)
        do (recentloc--marker-table-update-metadata
            mk nil (oref (gethash mk recentloc-marker-table) :timestamp)))

  ;;: Regenerate `recentloc-buffer-markers-table' from markers in
  ;;`recentloc-marker-table'.
  (progn
    (clrhash recentloc-buffer-markers-table)
    (loop for mk in (hash-table-keys recentloc-marker-table)
          do (let* ((mk-buf (marker-buffer mk))
                    (buf-mk-lst (gethash mk-buf recentloc-buffer-markers-table
                                         nil)))
               (if (and (buffer-live-p mk-buf)
                        (not (string-match recentloc-marker-recorder-ignore-buffers
                                           (buffer-name mk-buf))))
                   (puthash mk-buf (add-to-list 'buf-mk-lst mk nil #'eq)
                            recentloc-buffer-markers-table)
                 ;; remove buffer and marker
                 (remhash mk-buf recentloc-buffer-markers-table) ;safe sake
                 (remhash mk recentloc-marker-table)))))

  ;;: sorting `recentloc-buffer-markers-table' in position-ascending order
  (let ((sorted-table (make-hash-table :test #'eq
                                       :size 128)))
    (maphash (lambda (buf buf-mk-lst)
               (puthash buf (sort* buf-mk-lst
                                   (lambda (mk1 mk2)
                                     (< (marker-position mk1)
                                        (marker-position mk2))))
                        sorted-table))
             recentloc-buffer-markers-table)
    (setq recentloc-buffer-markers-table sorted-table))

  ;;: sanitizing marker buffer table as done by
  ;;`recentloc-marker-buffer-metadata-idle-updater'. In case of merging markers,
  ;;update the timestamp in `recentloc-marker-table'
  (let ((sanitized-table (make-hash-table :test #'eq
                                          :size 128)))
    (maphash (lambda (buf buf-mk-lst)
               (setq buf-mk-lst
                     (loop for curmk = (car buf-mk-lst) then (car buf-mk-lst)
                           until (null curmk)
                           do (let* ((curmk-region (recentloc-get-context-region curmk))
                                     (curmk-time (oref (gethash curmk recentloc-marker-table)
                                                       :timestamp))
                                     (new-curmk-time curmk-time)
                                     mk-time
                                     (rest-buf-mk-lst (cdr buf-mk-lst)))
                                (setq buf-mk-lst (cdr buf-mk-lst))
                                (loop for mk in rest-buf-mk-lst
                                      while (pos-in-region-p (marker-position mk)
                                                             curmk-region)
                                      do (progn
                                           (setq mk-time (oref (gethash mk recentloc-marker-table)
                                                               :timestamp))
                                           (when (time-less-p curmk-time mk-time)
                                             (setq new-curmk-time mk-time))
                                           ;; is it safe to do this?
                                           (setq buf-mk-lst (cdr buf-mk-lst))))
                                (when (time-less-p curmk-time new-curmk-time)
                                  (recentloc--marker-table-update-metadata curmk t new-curmk-time)))
                           collect curmk))
               (puthash buf buf-mk-lst sanitized-table))
             recentloc-buffer-markers-table)
    (setq recentloc-buffer-markers-table sanitized-table)))

;; ;;; unbound a symbol and check it
;; (let ((sym 'recentloc-command-record-ring))
;;   (makunbound sym)
;;   (boundp sym))

;; ;;; fix timer
;; (setq timer-idle-list
;;       (remove-if (lambda (timer)
;;                    (eq 200000
;;                        (aref timer 3)))
;;                  timer-idle-list))

;; (recentloc-initializer)
;; (recentloc-deinitializer)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
