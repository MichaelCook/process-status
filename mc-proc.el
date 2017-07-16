;; mc-proc.el : Buffer mode for displaying information from /proc.
;;
;; `M-x mc-proc RET` shows a graph of processes with parent/child
;; relationships.  For example:
;;
;;       1   systemd
;;     236    \_ systemd-journal
;;     248    \_ systemd-udevd
;;     669    \_ repowerd
;;     670    \_ avahi-daemon
;;     704    |   \_ avahi-daemon
;;    1163    \_ lightdm
;;    1215    |   \_ Xorg
;;    1326    |   \_ lightdm
;;    1353    |       \_ run-systemd-ses
;;    1461    |           \_ unclutter
;;    1494    |           \_ ssh-agent
;;    1645    |           \_ systemctl
;; ...
;;
;; Press RET on any line to see details about that process.
;; For example:
;;
;; pid      1645
;; ppid     1353
;; argv     systemctl --user start --wait ubuntu-session.target
;; comm     systemctl
;; cwd      /home/michael
;; exe      /bin/systemctl
;; state    Sleeping in an interruptible wait (S)
;; started  31 hours ago at 2017-06-30 09:58:25
;; session  1353
;; pgrp     1353
;; faults   major 0, minor 186, children 0/0
;; priority 20
;; nice     0
;; threads  1
;; itreal   0
;; rss      403 of unlimited (32 bits)
;; environ  CLUTTER_IM_MODULE=xim
;;          COMPIZ_CONFIG_PROFILE=ubuntu
;;          DBUS_SESSION_BUS_ADDRESS=unix:path=/run/user/1000/bus
;;          DEFAULTS_PATH=/usr/share/gconf/ubuntu.default.path
;; ...
;;
;; g to refresh.
;;
;; To send a signal to processes, mark the processes with k (KILL), t (TERM),
;; h (HUP) or \ (QUIT), then press x to send the signal(s).
;;
;; n and p move to next and previous lines and display the process details.

;; This software is distributed under the "Simplified BSD license":
;;
;; Copyright Michael Cook <michael@waxrat.com>. All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;
;;    1. Redistributions of source code must retain the above copyright notice,
;;       this list of conditions and the following disclaimer.
;;
;;    2. Redistributions in binary form must reproduce the above copyright
;;       notice, this list of conditions and the following disclaimer in the
;;       documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
;; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
;; OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;; TODO: 't' - sort by process creation time

(defun mc-proc-snapshot ()
  "Get the current status information for all processes.

Returns list of alists: PID STATS
where each STATS is an alist: COMM STATE PPID CHILDREN.
See proc(5)."
  (let ((procs (delq nil
                     (mapcar (lambda (entry)
                               (let ((pid (string-to-number entry)))
                                 (and (> pid 0)
                                      (with-temp-buffer
                                        (ignore-errors
                                          (insert-file-contents (mc-proc-file-name "stat" pid)))
                                        (goto-char (point-min))
                                        ;; pid (comm) state ppid
                                        ;; `comm` is always in parens and might contain spaces.
                                        ;; See proc(5).
                                        (if (looking-at "[0-9]+ (\\(.*\\)) \\(\\S+\\) \\([0-9]+\\) ")
                                            (cons pid
                                                  (list (cons 'comm
                                                              (buffer-substring (match-beginning 1)
                                                                                (match-end 1)))
                                                        (cons 'state
                                                              (buffer-substring (match-beginning 2)
                                                                                (match-end 2)))
                                                        (cons 'ppid
                                                              (string-to-number
                                                               (buffer-substring (match-beginning 3)
                                                                                 (match-end 3))))
                                                        (cons 'children nil))))))))
                             (directory-files "/proc")))))

    ;; populate the children list of each process
    (dolist (proc procs)
      (let* ((pid (car proc))
             (info (cdr proc))
             (ppid (cdr (assq 'ppid info)))
             (parent (assq ppid procs)))
        (if parent
            (let* ((parent-info (cdr parent))
                   (children (assq 'children parent-info)))
              (setcdr children (cons pid (cdr children)))))))

    ;; sort each children list
    (dolist (proc procs)
      (let* ((pid (car proc))
             (info (cdr proc))
             (children (assq 'children info)))
        (setcdr children (sort (cdr children) '<))))

    ;; sort by pid
    (setq procs (sort procs (lambda (a b)
                              (< (car a) (car b)))))

    procs))

(defvar mc-proc-tree-strings
  (if (char-displayable-p ?\u251c)
      '("    " " \u2502  " " \u2514\u2500 " " \u251c\u2500 ")
    '("    " " |  " " \\_ " " |_ "))
  "Four strings for drawing the process tree: BLANK VERTICAL CORNER TEE")
;; (nth 0 mc-proc-tree-strings) ;BLANK
;; (nth 1 mc-proc-tree-strings) ;VERTICAL
;; (nth 2 mc-proc-tree-strings) ;CORNER
;; (nth 3 mc-proc-tree-strings) ;TEE

(defun mc-proc-insert-tree-internal (pid is-last-child procs shown)
  (if (memq pid shown)
      nil
    (let ((info (assq pid procs)))
      (if (not info)
          nil
        (setq info (cdr info))
        (let ((state (cdr (assq 'state info)))
              (comm (cdr (assq 'comm info))))

          ;; Most processes are sleeping...
          (if (string= state "S")
              (setq state " "))

          (insert (format "  %5s %s %s%s\n"
                          pid
                          state
                          (if is-last-child
                              (concat (apply 'concat (mapcar (lambda (x)
                                                               (if (eq x 'yes)
                                                                   (nth 0 mc-proc-tree-strings) ;BLANK
                                                                 (nth 1 mc-proc-tree-strings))) ;VERTICAL
                                                             (reverse (cdr is-last-child))))
                                      (if (eq (car is-last-child) 'yes)
                                          (nth 2 mc-proc-tree-strings) ;CORNER
                                        (nth 3 mc-proc-tree-strings))) ;TEE
                            "")
                          comm)))
        (setcdr shown (cons pid (cdr shown)))
        (let ((children (cdr (assq 'children info))))
          (when children
            (dolist (child (butlast children))
              (mc-proc-insert-tree-internal
               child
               (cons 'no is-last-child)
               procs shown))
            (mc-proc-insert-tree-internal
             (car (last children))
             (cons 'yes is-last-child)
             procs shown)))))))

(defun mc-proc-insert-tree (procs)
  "Insert a textual tree representation of the given PROCS."
  (let ((shown (list 0)))
    (dolist (proc procs)
      (mc-proc-insert-tree-internal (car proc) nil procs shown))))

(defvar mc-proc-clk-tck 100
  "The value returned from sysconf(_SC_CLK_TCK).")

(defvar mc-proc-marks
  '((?T . SIGTERM)
    (?H . SIGHUP)
    (?K . SIGKILL)
    (?\\ . SIGQUIT))
  "The various marks and the signals they map to.")

(defun mc-proc-status ()
  "Create a process status buffer called `*process-status*'."
  (interactive)
  (switch-to-buffer (get-buffer-create "*process-status*"))
  (mc-proc-update))

(defun mc-proc-update ()
  (let ((wbl (mc-proc-get-window-buffer-locations (current-buffer))))
    (setq buffer-read-only nil)
    (erase-buffer)
    (mc-proc-insert-tree (mc-proc-snapshot))
    (set-buffer-modified-p nil)
    (mc-proc-mode)
    (mc-proc-set-window-buffer-locations (current-buffer) wbl)))

(defun mc-proc-next-line (arg)
  "Move to next line and show process info."
  (interactive "P")
  (next-line arg)
  (mc-proc-info))

(defun mc-proc-previous-line (arg)
  "Move to previous line and show process info."
  (interactive "P")
  (previous-line arg)
  (mc-proc-info))

(defun mc-proc-file-name (name &optional pid)
  "Return /proc/PID/NAME.  PID defaults to `mc-proc-get-pid`."
  (concat "/proc/"
          (number-to-string (or pid (mc-proc-get-pid)))
          "/" name))

(defun mc-proc-dired-fd ()
  "Run dired on this process' fd directory."
  (interactive)
  (dired-other-window (mc-proc-file-name "fd")))

(defun mc-proc-dired-cwd ()
  "Run dired on this process' current working directory."
  (interactive)
  (dired-other-window
   (file-truename (mc-proc-file-name "cwd"))))

(defun mc-proc-mouse-up (event)
  "Set point at the mouse click and display the process info."
  (interactive "e")
  (let ((posn (elt event 1)))
    (with-selected-window (posn-window posn)
      (goto-char (posn-point posn))
      (mc-proc-info))))

(defvar mc-proc-mode-map nil "")
(if mc-proc-mode-map
    ()
  (setq mc-proc-mode-map (make-keymap))
  (suppress-keymap mc-proc-mode-map t)

  (define-key mc-proc-mode-map "t" 'mc-proc-mark-term)
  (define-key mc-proc-mode-map "h" 'mc-proc-mark-hup)
  (define-key mc-proc-mode-map "k" 'mc-proc-mark-kill)
  (define-key mc-proc-mode-map "\\" 'mc-proc-mark-quit)

  (define-key mc-proc-mode-map "\r" 'mc-proc-info)
  (define-key mc-proc-mode-map [mouse-1] 'mc-proc-mouse-up)
  (define-key mc-proc-mode-map "u" 'mc-proc-unmark)
  (define-key mc-proc-mode-map "U" 'mc-proc-unmark-all)
  (define-key mc-proc-mode-map "x" 'mc-proc-execute)
  (define-key mc-proc-mode-map "f" 'mc-proc-dired-fd)
  (define-key mc-proc-mode-map "w" 'mc-proc-dired-cwd)
  (define-key mc-proc-mode-map "n" 'mc-proc-next-line)
  (define-key mc-proc-mode-map "p" 'mc-proc-previous-line)
  (define-key mc-proc-mode-map "G" 'mc-proc-toggle-refresh)
  (define-key mc-proc-mode-map "g" 'revert-buffer)
  (define-key mc-proc-mode-map "q" 'bury-buffer))

;; mode is suitable only for specially formatted data.
(put 'mc-proc-mode 'mode-class 'special)

(defun mc-proc-mode ()
  "Major mode for editing a list of processes.
Each line describes one of the processes.
Letters do not insert themselves; instead, they are commands.
\\<mc-proc-mode-map>"
  (kill-all-local-variables)
  (use-local-map mc-proc-mode-map)
  (setq major-mode 'mc-proc-mode)
  (setq mode-name "PS")
  (make-local-variable 'revert-buffer-function)
  (setq revert-buffer-function 'mc-proc-revert-function)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (run-hooks 'mc-proc-mode-hook))

(defun mc-proc-revert-function (ignore1 ignore2)
  (mc-proc-update)
  ;; Update the details buffer in case it's already displayed, but don't force
  ;; it to be displayed.
  (mc-proc-make-info-buffer (mc-proc-get-pid)))

(defun mc-proc-mark (num sig)
  ;; Mark the current NUM processes indicating
  ;; which signal they should receive.
  (forward-line 0)
  (let (buffer-read-only)
    (while (and (not (eobp))
                (> num 0))
      (delete-char 1)
      (insert (if sig
                  (car (rassoc sig mc-proc-marks))
                ? ))
      (forward-line 1)
      (setq num (1- num)))))

(defun mc-proc-mark-term (num)
  "Mark this process to receive a SIGTERM signal."
  (interactive "p")
  (mc-proc-stop-refresh)
  (mc-proc-mark num 'SIGTERM))

(defun mc-proc-mark-hup (num)
  "Mark this process to receive a SIGHUP signal."
  (interactive "p")
  (mc-proc-stop-refresh)
  (mc-proc-mark num 'SIGHUP))

(defun mc-proc-mark-kill (num)
  "Mark this process to receive a SIGKILL signal."
  (interactive "p")
  (mc-proc-stop-refresh)
  (mc-proc-mark num 'SIGKILL))

(defun mc-proc-mark-quit (num)
  "Mark this process to receive a SIGQUIT signal."
  (interactive "p")
  (mc-proc-stop-refresh)
  (mc-proc-mark num 'SIGQUIT))

(defun mc-proc-unmark (num)
  "Mark this process to receive no signal."
  (interactive "p")
  (mc-proc-mark num nil))

(defun mc-proc-unmark-all ()
  "Remove signal marks from all processes."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (mc-proc-mark (count-lines (point) (point-max)) nil)))

(defun mc-proc-get-link (pid name)
  "Return the contents of symlink /proc/PID/NAME, or nil if not available."
  (let ((link (mc-proc-file-name name pid)))
    (if (file-symlink-p link)
        (file-truename link))))

(defun mc-proc-get-file (pid name terminator)
  "Return the contents of /proc/PID/NAME, or nil if not available.

If TERMINATOR is not nil and if the last character in the file is that
character, then remove it from the returned string."
  (ignore-errors
    (with-temp-buffer
      (insert-file-contents (mc-proc-file-name name pid))
      (goto-char (point-max))
      (if (and terminator
               (not (bobp))
               (= (char-before) terminator))
          (goto-char (1- (point))))
      (buffer-substring (point-min) (point)))))

(defun mc-proc-get-argv (pid)
  "Return the command line arguments for pid (or nil if not available)."
  (let ((cmdline (mc-proc-get-file pid "cmdline" ?\0)))
    (if (and cmdline
             (> (length cmdline) 0))
        (split-string cmdline "\0"))))

(defun mc-proc-get-environ (pid)
  "Return the environment variables for pid (or nil if not available)."
  (let ((environ (mc-proc-get-file pid "environ" ?\0)))
    (if (and environ
             (> (length environ) 0))
        (split-string environ "\0"))))

(defun mc-proc-shell-quote-argument (string)
  (if (string-match "[^[:alnum:]_@%:,./=+-]" string)
      (concat "'" (replace-regexp-in-string "'" "'\\\\''" string) "'")
    string))

(defun mc-proc-make-info-buffer (pid)
  "Construct the *Process Status Details* buffer for PID.

Return the buffer."
  (let ((buffer (get-buffer-create "*Process Status Details*"))
        stat)
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert "pid      " (number-to-string pid) "\n")

      (setq stat (mc-proc-get-file pid "stat" ?\n))
      (when stat
        ;; pid (comm) state ...
        ;; `comm` is always in parens and might contain spaces.
        (if (not (string-match "^.*) " stat))
            (setq stat nil)
          (setq stat (split-string (concat "- - - " ;so indexes match proc(5)
                                           (substring stat (match-end 0)) " ")))
          (setq stat (list (cons 'state (nth 3 stat))
                           (cons 'ppid (nth 4 stat))
                           (cons 'pgrp (nth 5 stat))
                           (cons 'session (nth 6 stat))
                           (cons 'tty_nr (nth 7 stat))
                           (cons 'tpgid (nth 8 stat))
                           (cons 'flags (nth 9 stat))
                           (cons 'minflt (nth 10 stat))
                           (cons 'cminflt (nth 11 stat))
                           (cons 'majflt (nth 12 stat))
                           (cons 'cmajflt (nth 13 stat))
                           (cons 'utime (nth 14 stat))
                           (cons 'stime (nth 15 stat))
                           (cons 'cutime (nth 16 stat))
                           (cons 'cstime (nth 17 stat))
                           (cons 'priority (nth 18 stat))
                           (cons 'nice (nth 19 stat))
                           (cons 'num_threads (nth 20 stat))
                           (cons 'itrealvalue (nth 21 stat))
                           (cons 'starttime (nth 22 stat))
                           (cons 'vsize (nth 23 stat))
                           (cons 'rss (nth 24 stat))
                           (cons 'rsslim (nth 25 stat))

                           ;(cons 'startcode (nth 26 stat))
                           ;(cons 'endcode (nth 27 stat))
                           ;(cons 'startstack (nth 28 stat))
                           ;(cons 'kstkesp (nth 29 stat))
                           ;(cons 'kstkeip (nth 30 stat))
                           ;(cons 'signal (nth 31 stat))
                           ;(cons 'blocked (nth 32 stat))
                           ;(cons 'sigignore (nth 33 stat))
                           ;(cons 'sigcatch (nth 34 stat))
                           ;(cons 'wchan (nth 35 stat))
                           ;(cons 'nswap (nth 36 stat))
                           ;(cons 'cnswap (nth 37 stat))
                           ;(cons 'exit_signal (nth 38 stat))
                           ;(cons 'processor (nth 39 stat))
                           ;(cons 'rt_priority (nth 40 stat))
                           ;(cons 'policy (nth 41 stat))
                           ;(cons 'delayacct_blkio_ticks (nth 42 stat))
                           ;(cons 'guest_time (nth 43 stat))
                           ;(cons 'cguest_time (nth 44 stat))
                           ;(cons 'start_data (nth 45 stat))
                           ;(cons 'end_data (nth 46 stat))
                           ;(cons 'start_brk (nth 47 stat))
                           ;(cons 'arg_start (nth 48 stat))
                           ;(cons 'arg_end (nth 49 stat))
                           ;(cons 'env_start (nth 50 stat))
                           ;(cons 'env_end (nth 51 stat))
                           ;(cons 'exit_code (nth 52 stat))

                           ))))

      (if stat
          (let ((ppid (assoc-default 'ppid stat)))
            (if (and ppid
                     (not (string= ppid "0")))
                (insert "ppid     " ppid "\n"))))

      (let ((argv (mc-proc-get-argv pid)))
        (when argv
          (insert "argv    ")
          (dolist (arg argv)
            (insert " " (mc-proc-shell-quote-argument arg)))
          (insert "\n")))

      (let ((comm (mc-proc-get-file pid "comm" ?\n)))
        (and comm
             (insert "comm     " comm "\n")))

      (let ((cwd (mc-proc-get-link pid "cwd")))
        (and cwd
             (insert "cwd      " cwd "\n")))

      (let ((exe (mc-proc-get-link pid "exe")))
        (and exe
             (insert "exe      " exe "\n")))

      (if stat
          (let ((state (assq 'state stat)))
            (if state
                (insert "state    "
                        (assoc-default (cdr state)
                                       '(("R" . "Running")
                                         ("S" . "Sleeping in an interruptible wait")
                                         ("D" . "Waiting in uninterruptible disk sleep")
                                         ("Z" . "Zombie")
                                         ("T" . "Stopped on signal")
                                         ("t" . "Tracing stop")
                                         ("W" . "Paging")
                                         ("X" . "Dead")
                                         ("x" . "Dead")
                                         ("K" . "Wakekill")
                                         ("W" . "Waking")
                                         ("P" . "Parked"))
                                       nil
                                       "(unknown)")
                        " ("
                        (cdr state)
                        ")\n"))))

      (when stat
        ;; The time the process started after system boot (clock ticks).
        (let* ((started (/ (float (string-to-number
                                   (assoc-default 'starttime stat nil "0")))
                           mc-proc-clk-tck))
               (uptime (with-temp-buffer
                         (insert-file-contents "/proc/uptime")
                         (string-to-number (buffer-substring (point-min)
                                                             (point-max)))))
               (age (- uptime started))
               (then (seconds-to-time (- (float-time (current-time)) age))))
          (insert (format "started  %s ago at %s\n"
                          (cond ((> age (* 1.5 60 60 24 7))
                                 (format "%.0f weeks" (/ age (* 60 60 24 7))))
                                ((> age (* 1.5 60 60 24))
                                 (format "%.0f days" (/ age (* 60 60 24))))
                                ((> age (* 1.5 60 60))
                                 (format "%.0f hours" (/ age (* 60 60))))
                                ((> age (* 1.5 60))
                                 (format "%.0f minutes" (/ age 60)))
                                (t
                                 (format "%.0f seconds" age)))
                          (format-time-string "%Y-%m-%d %H:%M:%S" then))))

        (let ((session (assoc-default 'session stat)))
          (if session
              (insert "session  "
                      session
                      (if (= (string-to-number session) pid)
                          " (leader)" "")
                      "\n")))

        (let ((pgrp (assoc-default 'pgrp stat)))
          (if pgrp
              (insert "pgrp     "
                      pgrp
                      (if (= (string-to-number pgrp) pid)
                          " (leader)" "")
                      "\n")))

        (insert (format "faults   major %s, minor %s, children %s/%s\n"
                        (assoc-default 'majflt stat)
                        (assoc-default 'minflt stat)
                        (assoc-default 'cmajflt stat)
                        (assoc-default 'cminflt stat)))

        (insert (format "priority %s\n" (assoc-default 'priority stat)))
        (insert (format "nice     %s\n" (assoc-default 'nice stat)))
        (insert (format "threads  %s\n" (assoc-default 'num_threads stat)))
        (insert (format "itreal   %s\n" (assoc-default 'itrealvalue stat)))

        (let ((rss (assoc-default 'rss stat))
              (rsslim (assoc-default 'rsslim stat)))
          (insert (format "rss      %s of %s\n"
                          rss
                          (cond ((string= rsslim "4294967295")
                                 "unlimited (32 bits)")
                                ((string= rsslim "18446744073709551615")
                                 "unlimited (64 bits)")
                                (t rsslim))))))

      (let ((root (mc-proc-get-link pid "root")))
        (and root
             (not (string= root "/"))
             (insert "root     " root "\n")))

      (let ((environ (mc-proc-get-environ pid))
            (label "environ  "))
        (dolist (env (sort environ 'string<))
          (insert label (mc-proc-quote-env-for-shell env) "\n")
          (setq label "         ")))

      (goto-char (point-min))
      (set-buffer-modified-p nil)
      (setq buffer-read-only t)
      buffer)))

(defun mc-proc-escape-string-for-shell (string)
  (let ((escape-map '((?' . "\\'")
                      (?\\ . "\\\\")
                      (?\a . "\\a")
                      (?\b . "\\b")
                      (?\e . "\\e")
                      (?\f . "\\f")
                      (?\n . "\\n")
                      (?\r . "\\r")
                      (?\t . "\\t")
                      (?\v . "\\v"))))
    (replace-regexp-in-string "[\\'\x00-\x1f\x7f-\xff]"
                              (lambda (c)
                                (setq c (string-to-char c))
                                (or (assoc-default c escape-map 'eq)
                                    (format "\\x%02X" c)))
                              string
                              t    ;fixedcase
                              t))) ;literal

(defun mc-proc-quote-string-for-shell (string)
  (let ((escaped (mc-proc-escape-string-for-shell string)))
    (if (string= escaped string)
        (if (string-match " " string)
            (concat "'" string "'")
          string)
      (concat "$'" escaped "'"))))

;; (mc-proc-quote-string-for-shell "foo\tbar\rbaz\0qux\ncan't\\foo")

(defun mc-proc-quote-env-for-shell (string)
  (let ((i (string-match "=" string)))
    (if i
        (concat (mc-proc-quote-string-for-shell (substring string 0 i))
                "="
                (mc-proc-quote-string-for-shell (substring string (1+ i))))
      (mc-proc-quote-string-for-shell string))))

;; (mc-proc-quote-env-for-shell "HELLO=foo\tbar")

(defun mc-proc-info ()
  "Show details about the process at point."
  (interactive)
  (display-buffer (mc-proc-make-info-buffer (mc-proc-get-pid))))

(defun mc-proc-get-pid ()
  "Get the pid for the current line."
  ;; Find the pid.
  ;; The first character might be a signal marker like:
  ;; H 12345 \_ command
  (save-excursion (forward-line 0)
                  (or (looking-at "^. *\\([0-9]+\\)\\>")
                      (error "No PID")))
  (string-to-number (buffer-substring
                     (match-beginning 1)
                     (match-end 1))))

(defun mc-proc-execute ()
  "Send the indicated signals to all of the marked processes."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let ((mark (char-after (point))))
	(if (= mark ? )
	    ;; Skip this line.  It is unmarked.
	    (forward-line 1)
	  ;; Find the process ID.
	  (let* ((pid (mc-proc-get-pid))
		 (result (mc-proc-send-signal
			  (cdr (assoc mark mc-proc-marks))
			  pid)))
	    (cond ((null result)	;Signal sent ok.
		   (forward-line 1))
		  ((stringp result)	;An error message.
		   (message "kill %d: %s" pid result)
		   (forward-line 1))
		  (t			;No such process
		   (let (buffer-read-only)
		     (delete-region (save-excursion (beginning-of-line)
						    (point))
				    (save-excursion (forward-line 1)
						    (point))))))))))))

(defun mc-proc-send-signal (sig pid)
  ;; Send the signal SIG to the process PID.
  ;; Returns nil if the signal is sent without error.
  ;; Returns t if the process no longer exists.
  ;; Returns a string describing the error otherwise.

  (with-temp-buffer
    (call-process "kill"
		  nil			;INFILE (nil means `/dev/null')
		  t			;BUFFER (t means current buffer)
		  nil			;DISPLAY
		  (concat "-" (symbol-name sig))
		  (number-to-string pid))
    (goto-char (point-min))
    ;; "kill: (1234): No such process"
    (if (re-search-forward ".*: \\(.*\\)" nil t)
	(let ((err (buffer-substring (match-beginning 1)
				     (match-end 1))))
	  (if (string= err "No such process")
	      t				;Process does not exist.
	    err))			;Some other error occurred.
      nil)))				;Success.

(defvar mc-proc-refresh-timer nil
  "When not nil, the buffer is being automatically refreshed.")

(defun mc-proc-refresh-timer-handler ()
  "Called each time the refresh timer expires."
  (let ((buffer (get-buffer "*process-status*")))
    (if (and buffer
             (get-buffer-window buffer 'visible))
        (with-current-buffer buffer
          (revert-buffer))
      ;; Automatically stop refreshing when the buffer is no longer
      ;; displayed anywhere.
      (cancel-timer mc-proc-refresh-timer)
      (setq mc-proc-refresh-timer nil))))

(defun mc-proc-stop-refresh ()
  (when mc-proc-refresh-timer
    (cancel-timer mc-proc-refresh-timer)
    (setq mc-proc-refresh-timer nil)))

(defun mc-proc-toggle-refresh ()
  "Enable or disable auto-refreshing."
  (interactive)
  (if mc-proc-refresh-timer
      (progn
        (mc-proc-stop-refresh)
        (message "Refresh disabled"))
    (setq mc-proc-refresh-timer
          (run-with-timer .1 2 #'mc-proc-refresh-timer-handler))
    (message "Refresh enabled")))

(defun mc-proc-get-window-buffer-locations (buffer)
  "Get point for BUFFER in all windows.

Returns a list of window window-start line column (zero based)."
  (let (locations)
    (dolist (frame (frame-list))
      (dolist (window (window-list frame))
        (if (eq buffer (window-buffer window))
            (with-selected-window window
              (with-current-buffer buffer
                (setq locations
                      (cons (list (cons 'window window)
                                  (cons 'window-start (window-start window))
                                  (cons 'line
                                        (count-lines 1 (save-excursion (forward-line 0)
                                                                       (point))))
                                  (cons 'column
                                        (- (point)
                                           (save-excursion (forward-line 0)
                                                           (point)))))
                            locations)))))))
    locations))

(defun mc-proc-set-window-buffer-locations (buffer locations)
  "Set point for BUFFER in the given windows.

LOCATIONS is from `mc-proc-get-window-buffer-locations`."
  (dolist (location locations)
    (let ((window (cdr (assq 'window location)))
          (window-start (cdr (assq 'window-start location)))
          (line (cdr (assq 'line location)))
          (column (cdr (assq 'column location))))
      (with-selected-window window
        (with-current-buffer buffer
          (set-window-start window window-start)
          (goto-char (point-min))
          (forward-line line)
          (if (< (- (save-excursion (end-of-line)
                                    (point))
                    (point))
                 column)
              (end-of-line)
            (forward-char column)))))))
