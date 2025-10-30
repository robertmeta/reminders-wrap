;;; reminders.el --- Emacs interface for macOS Reminders CLI -*- lexical-binding: t -*-

;; Author: Robert Melton
;; Version: 1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, tools

;;; Commentary:

;; This package provides an Emacs interface to the macOS `reminders` CLI tool.
;; It allows you to view, add, edit, complete, and delete reminders from within Emacs.
;;
;; Features:
;; - View active and recently completed reminders
;; - Toggle completion status with RET
;; - Add, edit, delete reminders
;; - Add and edit notes (with clickable links)
;; - Set priorities and due dates
;; - Sort by due date or creation date (ascending/descending)
;; - Full Emacspeak integration for screen reader users
;; - Evil mode support with vim-like keybindings

;;; Code:

(require 'json)
(require 'seq)
(require 'cl-lib)

(defgroup reminders nil
  "Interface to macOS Reminders CLI."
  :group 'tools)

(defcustom reminders-default-list "Reminders"
  "Default reminder list to use."
  :type 'string
  :group 'reminders)

(defcustom reminders-command "reminders"
  "Path to the reminders command-line tool."
  :type 'string
  :group 'reminders)

(defcustom reminders-show-completed nil
  "Whether to show completed reminders by default."
  :type 'boolean
  :group 'reminders)

(defvar reminders-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'reminders-toggle-complete)
    (define-key map (kbd "a") 'reminders-add)
    (define-key map (kbd "d") 'reminders-delete)
    (define-key map (kbd "e") 'reminders-edit)
    (define-key map (kbd "g") 'reminders-refresh)
    (define-key map (kbd "l") 'reminders-switch-list)
    (define-key map (kbd "L") 'reminders-show-all-lists)
    (define-key map (kbd "n") 'next-line)
    (define-key map (kbd "p") 'previous-line)
    (define-key map (kbd "q") 'quit-window)
    (define-key map (kbd "t") 'reminders-toggle-show-completed)
    (define-key map (kbd "N") 'reminders-add-notes)
    (define-key map (kbd "P") 'reminders-set-priority)
    (define-key map (kbd "D") 'reminders-set-due-date)
    (define-key map (kbd "s") 'reminders-sort-by-due-date)
    (define-key map (kbd "?") 'reminders-help)
    map)
  "Keymap for `reminders-mode'.")

(defvar-local reminders-current-list nil
  "The currently displayed reminder list.")

(defvar-local reminders-sort-by nil
  "Current sort method: nil, 'due-date, or 'creation-date.")

(defvar-local reminders-sort-order 'ascending
  "Current sort order: 'ascending or 'descending.")

(defvar-local reminders-reminders-data nil
  "Cached reminders data for the current buffer.")

;;; Utility functions

(defun reminders--run-command (&rest args)
  "Run reminders command with ARGS and return output."
  (with-temp-buffer
    (let ((exit-code (apply 'call-process reminders-command nil t nil args)))
      (if (zerop exit-code)
          (buffer-string)
        (error "Reminders command failed: %s" (buffer-string))))))

(defun reminders--run-command-json (&rest args)
  "Run reminders command with ARGS and parse JSON output."
  (let* ((all-args (append args '("--format" "json")))
         (output (apply 'reminders--run-command all-args)))
    (if (string-empty-p output)
        nil
      (let ((json-array-type 'list))
        (json-read-from-string output)))))

(defun reminders--get-lists ()
  "Get list of all reminder lists."
  (split-string (reminders--run-command "show-lists") "\n" t))

(defun reminders--parse-iso8601 (date-string)
  "Parse ISO8601 DATE-STRING to Emacs time format."
  (when (and date-string (not (string-empty-p date-string)))
    (if (fboundp 'parse-iso8601-time-string)
        ;; Emacs 29+
        (parse-iso8601-time-string date-string)
      ;; Emacs 27-28: use date-to-time which handles ISO8601
      (date-to-time date-string))))

(defun reminders--format-due-date (date-string)
  "Format DATE-STRING for display."
  (if (or (null date-string) (string-empty-p date-string))
      ""
    (let* ((parsed (reminders--parse-iso8601 date-string))
           (now (current-time))
           (diff-days (- (time-to-days parsed) (time-to-days now))))
      (cond
       ((< diff-days 0) (format "(%d days ago)" (abs diff-days)))
       ((= diff-days 0) "(today)")
       ((= diff-days 1) "(tomorrow)")
       ((< diff-days 7) (format "(in %d days)" diff-days))
       ((< diff-days 14) "(next week)")
       (t (format-time-string "(%b %d)" parsed))))))

(defun reminders--priority-string (priority)
  "Convert PRIORITY number to string representation."
  (cond
   ((= priority 1) "[!!!] ")
   ((= priority 5) "[!!] ")
   ((= priority 9) "[!] ")
   (t "")))

(defun reminders--get-reminder-at-point ()
  "Get the reminder data at point."
  (get-text-property (point) 'reminder-data))

;;; Emacspeak integration

(defun reminders--emacspeak-speak-line ()
  "Custom Emacspeak line speaking for reminders."
  (when (and (boundp 'emacspeak-speak-mode) emacspeak-speak-mode)
    (let ((reminder (reminders--get-reminder-at-point)))
      (when reminder
        (let* ((title (alist-get 'title reminder))
               (due-date (alist-get 'dueDate reminder))
               (priority (or (alist-get 'priority reminder) 0))
               (is-completed (eq (alist-get 'isCompleted reminder) t))
               (notes (alist-get 'notes reminder))
               (display-idx (or (alist-get 'display-index reminder) 0))
               (due-str (reminders--format-due-date due-date))
               (priority-text (cond
                               ((= priority 1) "high priority")
                               ((= priority 5) "medium priority")
                               ((= priority 9) "low priority")
                               (t "")))
               (status-text (if is-completed "completed" "not completed"))
               (speech-text (format "%s%s%s%s, item %d"
                                    title
                                    (if (string-empty-p priority-text) "" (concat ", " priority-text))
                                    (if (string-empty-p due-str) "" (concat " due " due-str))
                                    (concat ", " status-text)
                                    display-idx)))
          (dtk-speak speech-text))
        t))))

;;; Display functions

(defun reminders--save-notes-from-buffer ()
  "Save edited notes back to the reminder."
  (interactive)
  (let ((list-name (buffer-local-value 'reminders--notes-list-name (current-buffer)))
        (reminder-id (buffer-local-value 'reminders--notes-id (current-buffer)))
        (title (buffer-local-value 'reminders--notes-title (current-buffer)))
        (notes (buffer-substring-no-properties (point-min) (point-max))))
    (when (and list-name reminder-id title)
      (reminders--run-command "edit" list-name reminder-id
                              "--notes" notes title)
      (message "Notes saved")
      (quit-window)
      ;; Refresh the reminders buffer if it exists
      (let ((reminders-buffer (get-buffer (format "*Reminders: %s*" list-name))))
        (when reminders-buffer
          (with-current-buffer reminders-buffer
            (reminders-refresh)))))))

(defun reminders--show-notes (notes list-name reminder-id title)
  "Display NOTES in an editable popup buffer for reminder with REMINDER-ID in LIST-NAME with TITLE."
  (with-current-buffer (get-buffer-create "*Reminder Notes*")
    (erase-buffer)
    (insert notes)
    (goto-char (point-min))
    (text-mode)
    (setq-local reminders--notes-list-name list-name)
    (setq-local reminders--notes-id reminder-id)
    (setq-local reminders--notes-title title)
    (local-set-key (kbd "C-c C-c") 'reminders--save-notes-from-buffer)
    (local-set-key (kbd "C-x C-s") 'reminders--save-notes-from-buffer)
    (setq header-line-format "Edit notes - Save with C-c C-c or C-x C-s, quit with q")
    (pop-to-buffer (current-buffer))))

(defun reminders--insert-reminder (reminder &optional use-letter)
  "Insert REMINDER into the buffer.
If USE-LETTER is non-nil, use a letter (a, b, c...) instead of a number for display."
  (let* ((title (alist-get 'title reminder))
         (due-date (alist-get 'dueDate reminder))
         (priority (or (alist-get 'priority reminder) 0))
         (is-completed (eq (alist-get 'isCompleted reminder) t))
         (notes (alist-get 'notes reminder))
         (external-id (alist-get 'externalId reminder))
         (display-idx (or (alist-get 'display-index reminder) 0))
         (due-str (reminders--format-due-date due-date))
         (priority-str (reminders--priority-string priority))
         (checkbox (if is-completed "[X]" "[ ]"))
         (face (if is-completed 'shadow 'default))
         (start (point))
         (index-str (if use-letter
                        (string (+ ?a display-idx))
                      (format "%2d" display-idx))))
    (insert (format "%2s: %s %s%s %s"
                    index-str
                    checkbox
                    priority-str
                    title
                    due-str))
    (when notes
      (insert "\n    ")
      (insert-button "Read Notes"
                     'action (lambda (_button)
                               (reminders--show-notes notes reminders-current-list external-id title))
                     'follow-link t
                     'help-echo "Click to view and edit notes"))
    (put-text-property start (point) 'reminder-data reminder)
    (put-text-property start (point) 'reminder-id external-id)
    (put-text-property start (point) 'face face)
    ;; Add Emacspeak-specific spoken text
    (when (featurep 'emacspeak)
      (let* ((priority-text (cond
                             ((= priority 1) "high priority")
                             ((= priority 5) "medium priority")
                             ((= priority 9) "low priority")
                             (t "")))
             (status-text (if is-completed "completed" "not completed"))
             (spoken-text (format "%s%s%s%s, item %s"
                                  title
                                  (if (string-empty-p priority-text) "" (concat ", " priority-text))
                                  (if (string-empty-p due-str) "" (concat " due " due-str))
                                  (concat ", " status-text)
                                  index-str)))
        (put-text-property start (point) 'emacspeak-speak spoken-text)
        (put-text-property start (point) 'personality
                           (if is-completed 'voice-monotone 'voice-bolden))))
    (insert "\n")))

(defun reminders--recently-completed-p (reminder)
  "Return t if REMINDER was completed in the last 7 days."
  (when-let* ((is-completed (eq (alist-get 'isCompleted reminder) t))
              (completion-date (alist-get 'completionDate reminder))
              (parsed-date (reminders--parse-iso8601 completion-date))
              (now (current-time))
              (days-ago (- (time-to-days now) (time-to-days parsed-date))))
    (and (<= days-ago 7) (>= days-ago 0))))

(defun reminders--add-display-indices (reminders)
  "Add display index field to each reminder in REMINDERS list.
Returns list with display-index added to each reminder."
  (let ((index 0)
        (result nil))
    (dolist (reminder reminders)
      (let ((indexed (cons (cons 'display-index index) reminder)))
        (push indexed result)
        (setq index (1+ index))))
    (nreverse result)))

(defun reminders--display-reminders (list-name)
  "Display reminders from LIST-NAME."
  (message "Loading reminders from %s..." list-name)
  (let* ((args (list "show" list-name))
         ;; Always include completed to get recently completed items
         (args (append args '("--include-completed")))
         (args (if reminders-sort-by
                   (append args (list "--sort" (symbol-name reminders-sort-by)
                                      "--sort-order" (symbol-name reminders-sort-order)))
                 args))
         (all-reminders (apply 'reminders--run-command-json args))
         ;; Separate active and recently completed
         (active-reminders (seq-filter
                           (lambda (r)
                             (not (eq (alist-get 'isCompleted r) t)))
                           all-reminders))
         (recently-completed (if reminders-show-completed
                                 (seq-filter
                                  (lambda (r)
                                    (eq (alist-get 'isCompleted r) t))
                                  all-reminders)
                               (seq-filter 'reminders--recently-completed-p all-reminders)))
         ;; Add display indices for showing to user
         (indexed-active (reminders--add-display-indices active-reminders))
         (indexed-recent (reminders--add-display-indices recently-completed)))
    (setq reminders-reminders-data all-reminders)
    (let ((inhibit-read-only t)
          (display-index 0))
      (erase-buffer)
      (insert (format "Reminders - %s" list-name))
      (when reminders-show-completed
        (insert " [showing all completed]"))
      (when reminders-sort-by
        (insert (format " [sorted by %s %s]" reminders-sort-by reminders-sort-order)))
      (insert "\n\n")
      ;; Show appropriate keybindings based on whether Evil mode is active
      (if (and (boundp 'evil-mode) evil-mode (eq evil-state 'normal))
          ;; Evil mode keybindings
          (progn
            (insert "Commands: [c/RET] toggle  [a] add  [A] add w/date  [e] edit  [x] delete  [r] refresh  [l] list  [t] toggle completed  [q] quit  [?] help\n")
            (insert "          [N] notes  [P] priority  [D] due date  [s] sort  [L] all lists  [j/k] move\n\n"))
        ;; Emacs keybindings
        (progn
          (insert "Commands: [RET] toggle  [a] add  [e] edit  [d] delete  [g] refresh  [l] switch list  [t] toggle completed  [q] quit  [?] help\n")
          (insert "          [N] notes  [P] priority  [D] due date  [s] sort  [L] all lists\n\n")))
      (if reminders-show-completed
          ;; Show all reminders when in show-completed mode
          (progn
            (if (null all-reminders)
                (insert "No reminders.\n")
              (let ((all-indexed (reminders--add-display-indices all-reminders)))
                (dolist (reminder all-indexed)
                  (reminders--insert-reminder reminder)))))
        ;; Normal mode: show active with numbers, recently completed with letters
        (if (null indexed-active)
            (insert "No active reminders.\n")
          (dolist (reminder indexed-active)
            (reminders--insert-reminder reminder)))
        (when indexed-recent
          (insert "\nRecently Completed (last 7 days):\n")
          (dolist (reminder indexed-recent)
            (reminders--insert-reminder reminder t)))))
    (message "Loaded %d active, %d recently completed from %s"
             (length indexed-active)
             (length indexed-recent)
             list-name)))

;;; Interactive commands

;;;###autoload
(defun reminders-show (&optional list-name)
  "Show reminders from LIST-NAME (or default list)."
  (interactive)
  (let ((list (or list-name reminders-default-list)))
    (with-current-buffer (get-buffer-create (format "*Reminders: %s*" list))
      (reminders-mode)
      (setq reminders-current-list list)
      (reminders--display-reminders list)
      (goto-char (point-min))
      (forward-line 5)
      (switch-to-buffer (current-buffer)))))

(defun reminders-refresh ()
  "Refresh the current reminders buffer."
  (interactive)
  (when reminders-current-list
    (let ((line (line-number-at-pos)))
      (reminders--display-reminders reminders-current-list)
      (goto-char (point-min))
      (forward-line (1- line)))))

(defun reminders-add (title &optional due-date priority notes)
  "Add a new reminder with TITLE, optional DUE-DATE, PRIORITY, and NOTES."
  (interactive "sReminder: ")
  (let ((args (list "add" reminders-current-list title)))
    (when (and due-date (not (string-empty-p due-date)))
      (setq args (append args (list "--due-date" due-date))))
    (when priority
      (setq args (append args (list "--priority" (format "%d" priority)))))
    (when (and notes (not (string-empty-p notes)))
      (setq args (append args (list "--notes" notes))))
    (apply 'reminders--run-command args)
    (message "Added: %s" title)
    (reminders-refresh)))

(defun reminders-add-with-date (title)
  "Add a new reminder with TITLE and prompt for a due date using org-read-date."
  (interactive "sReminder: ")
  (require 'org)
  (let* ((date (org-read-date nil nil nil "Due date: " (current-time)))
         (args (list "add" reminders-current-list title "--due-date" date)))
    (apply 'reminders--run-command args)
    (message "Added: %s (due: %s)" title date)
    (reminders-refresh)))

(defun reminders-toggle-complete ()
  "Toggle completion status of reminder at point."
  (interactive)
  (let* ((reminder (reminders--get-reminder-at-point))
         (reminder-id (get-text-property (point) 'reminder-id)))
    (when reminder
      (if (null reminder-id)
          (progn
            (message "Error: No ID found for '%s'. Please refresh with 'g'."
                     (alist-get 'title reminder))
            (reminders-refresh))
        (let ((is-completed (eq (alist-get 'isCompleted reminder) t)))
          (if is-completed
              (reminders--run-command "uncomplete" reminders-current-list reminder-id)
            (reminders--run-command "complete" reminders-current-list reminder-id))
          (message "%s: %s"
                   (if is-completed "Uncompleted" "Completed")
                   (alist-get 'title reminder))
          (reminders-refresh))))))

(defun reminders-delete ()
  "Delete reminder at point."
  (interactive)
  (let* ((reminder (reminders--get-reminder-at-point))
         (reminder-id (get-text-property (point) 'reminder-id)))
    (when reminder
      (when (y-or-n-p (format "Delete '%s'? " (alist-get 'title reminder)))
        (reminders--run-command "delete" reminders-current-list reminder-id)
        (message "Deleted: %s" (alist-get 'title reminder))
        (reminders-refresh)))))

(defun reminders-edit (new-title)
  "Edit the reminder at point with NEW-TITLE."
  (interactive
   (let* ((reminder (reminders--get-reminder-at-point))
          (current-title (when reminder (alist-get 'title reminder))))
     (list (read-string "New title: " current-title))))
  (let* ((reminder (reminders--get-reminder-at-point))
         (reminder-id (get-text-property (point) 'reminder-id)))
    (when reminder
      (reminders--run-command "edit" reminders-current-list reminder-id new-title)
      (message "Updated: %s" new-title)
      (reminders-refresh))))

(defun reminders-add-notes (notes)
  "Add or update NOTES for reminder at point."
  (interactive
   (let* ((reminder (reminders--get-reminder-at-point))
          (current-notes (when reminder (alist-get 'notes reminder))))
     (list (read-string "Notes: " current-notes))))
  (let* ((reminder (reminders--get-reminder-at-point))
         (reminder-id (get-text-property (point) 'reminder-id)))
    (when reminder
      (reminders--run-command "edit" reminders-current-list reminder-id
                              "--notes" notes (alist-get 'title reminder))
      (message "Updated notes")
      (reminders-refresh))))

(defun reminders-set-priority ()
  "Set priority for reminder at point."
  (interactive)
  (let* ((reminder (reminders--get-reminder-at-point))
         (reminder-id (get-text-property (point) 'reminder-id))
         (priority (read-string "Priority (1=high, 5=medium, 9=low, 0=none): " "0")))
    (when reminder
      (reminders--run-command "add" reminders-current-list (alist-get 'title reminder)
                              "--priority" priority)
      (reminders--run-command "delete" reminders-current-list reminder-id)
      (message "Updated priority")
      (reminders-refresh))))

(defun reminders-set-due-date (date)
  "Set due DATE for reminder at point."
  (interactive "sDue date (YYYY-MM-DD): ")
  (let* ((reminder (reminders--get-reminder-at-point))
         (reminder-id (get-text-property (point) 'reminder-id)))
    (when reminder
      (reminders--run-command "add" reminders-current-list (alist-get 'title reminder)
                              "--due-date" date)
      (reminders--run-command "delete" reminders-current-list reminder-id)
      (message "Updated due date")
      (reminders-refresh))))

(defun reminders-switch-list (list-name)
  "Switch to a different reminder LIST-NAME."
  (interactive
   (list (completing-read "List: " (reminders--get-lists) nil t)))
  (setq reminders-current-list list-name)
  (reminders--display-reminders list-name)
  (goto-char (point-min))
  (forward-line 5))

(defun reminders-toggle-show-completed ()
  "Toggle showing completed reminders."
  (interactive)
  (setq reminders-show-completed (not reminders-show-completed))
  (reminders-refresh))

(defun reminders-sort-by-due-date ()
  "Cycle through sort options: none -> due-date asc -> due-date desc -> creation-date asc -> creation-date desc -> none."
  (interactive)
  (cond
   ;; No sorting -> due-date ascending
   ((null reminders-sort-by)
    (setq reminders-sort-by 'due-date
          reminders-sort-order 'ascending)
    (message "Sorting by due-date (ascending)"))
   ;; due-date ascending -> due-date descending
   ((and (eq reminders-sort-by 'due-date)
         (eq reminders-sort-order 'ascending))
    (setq reminders-sort-order 'descending)
    (message "Sorting by due-date (descending)"))
   ;; due-date descending -> creation-date ascending
   ((and (eq reminders-sort-by 'due-date)
         (eq reminders-sort-order 'descending))
    (setq reminders-sort-by 'creation-date
          reminders-sort-order 'ascending)
    (message "Sorting by creation-date (ascending)"))
   ;; creation-date ascending -> creation-date descending
   ((and (eq reminders-sort-by 'creation-date)
         (eq reminders-sort-order 'ascending))
    (setq reminders-sort-order 'descending)
    (message "Sorting by creation-date (descending)"))
   ;; creation-date descending -> no sorting
   ((and (eq reminders-sort-by 'creation-date)
         (eq reminders-sort-order 'descending))
    (setq reminders-sort-by nil
          reminders-sort-order 'ascending)
    (message "Sorting disabled"))
   ;; fallback
   (t
    (setq reminders-sort-by nil
          reminders-sort-order 'ascending)
    (message "Sorting disabled")))
  (reminders-refresh))

(defun reminders-show-all-lists ()
  "Show all reminder lists."
  (interactive)
  (let ((lists (reminders--get-lists)))
    (with-current-buffer (get-buffer-create "*Reminder Lists*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Reminder Lists\n\n")
        (dolist (list lists)
          (insert (format "  %s\n" list)))
        (insert "\nPress 'q' to close")
        (goto-char (point-min))
        (view-mode)
        (switch-to-buffer (current-buffer))))))

(defun reminders-new-list (list-name)
  "Create a new reminder LIST-NAME."
  (interactive "sNew list name: ")
  (reminders--run-command "new-list" list-name)
  (message "Created list: %s" list-name))

(defun reminders-quick-add ()
  "Quickly add a reminder to the default list."
  (interactive)
  (let ((title (read-string "Quick reminder: ")))
    (when (not (string-empty-p title))
      (reminders--run-command "add" reminders-default-list title)
      (message "Added: %s" title))))

(defun reminders-help ()
  "Show help documentation for reminders-mode."
  (interactive)
  (with-current-buffer (get-buffer-create "*Reminders Help*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "Reminders Mode Help\n")
      (insert "===================\n\n")

      (insert "FEATURES:\n")
      (insert "  • View active and recently completed reminders (last 7 days)\n")
      (insert "  • Toggle completion status\n")
      (insert "  • Add, edit, and delete reminders\n")
      (insert "  • Add and edit notes with clickable links\n")
      (insert "  • Set priorities (high/medium/low) and due dates\n")
      (insert "  • Sort by due date or creation date (ascending/descending)\n")
      (insert "  • Switch between reminder lists\n")
      (insert "  • Full Emacspeak integration for screen readers\n")
      (insert "  • Evil mode support with vim-like keybindings\n\n")

      (insert "EMACS KEYBINDINGS:\n")
      (insert "  RET     - Toggle completion status\n")
      (insert "  a       - Add new reminder\n")
      (insert "  e       - Edit reminder title\n")
      (insert "  d       - Delete reminder\n")
      (insert "  g       - Refresh list\n")
      (insert "  l       - Switch to different list\n")
      (insert "  L       - Show all lists\n")
      (insert "  t       - Toggle showing all completed items\n")
      (insert "  N       - Add/edit notes\n")
      (insert "  P       - Set priority (1=high, 5=medium, 9=low)\n")
      (insert "  D       - Set due date (YYYY-MM-DD)\n")
      (insert "  s       - Cycle through sort options\n")
      (insert "  n       - Next line\n")
      (insert "  p       - Previous line\n")
      (insert "  q       - Quit window\n")
      (insert "  ?       - Show this help\n\n")

      (insert "EVIL MODE KEYBINDINGS (when Evil is active):\n")
      (insert "  RET, c  - Toggle completion status\n")
      (insert "  a       - Add new reminder\n")
      (insert "  A       - Add new reminder with date picker\n")
      (insert "  x, X    - Delete reminder\n")
      (insert "  e, E    - Edit reminder\n")
      (insert "  r, R    - Refresh list\n")
      (insert "  l       - Switch list\n")
      (insert "  L       - Show all lists\n")
      (insert "  t       - Toggle show completed\n")
      (insert "  N       - Add/edit notes\n")
      (insert "  P       - Set priority\n")
      (insert "  D       - Set due date\n")
      (insert "  s       - Cycle sort options\n")
      (insert "  j, k    - Move down/up\n")
      (insert "  h, l    - Move left/right\n")
      (insert "  gg, G   - Jump to top/bottom\n")
      (insert "  0, $    - Jump to line start/end\n")
      (insert "  q, ZZ   - Quit window\n")
      (insert "  ?       - Show this help\n\n")

      (insert "SORTING:\n")
      (insert "  Press 's' to cycle through sort options:\n")
      (insert "    • None (default order)\n")
      (insert "    • Due date (ascending)\n")
      (insert "    • Due date (descending)\n")
      (insert "    • Creation date (ascending)\n")
      (insert "    • Creation date (descending)\n\n")

      (insert "RECENTLY COMPLETED:\n")
      (insert "  Items completed in the last 7 days appear at the bottom\n")
      (insert "  with letter indices (a, b, c...) instead of numbers.\n")
      (insert "  This lets you quickly undo accidental completions and\n")
      (insert "  review what you've accomplished recently.\n\n")

      (insert "EDITING NOTES:\n")
      (insert "  Click 'Read Notes' or press RET on the link to open notes.\n")
      (insert "  Edit the notes, then press C-c C-c or C-x C-s to save.\n")
      (insert "  Press 'q' to quit without saving.\n\n")

      (insert "Press 'q' to close this help buffer.")
      (goto-char (point-min))
      (view-mode)
      (switch-to-buffer (current-buffer)))))

;;; Major mode

(define-derived-mode reminders-mode special-mode "Reminders"
  "Major mode for viewing and managing macOS Reminders.

Emacs keybindings:
\\{reminders-mode-map}

Evil mode keybindings (when Evil is loaded):
  c, RET     - Toggle completion status
  a, A       - Add new reminder
  x, X       - Delete reminder
  e, E       - Edit reminder
  r, R       - Refresh list
  l          - Switch list
  L          - Show all lists
  t          - Toggle show completed
  N          - Add/edit notes
  P          - Set priority
  D          - Set due date
  s          - Cycle sort options
  j/k/h/0/$  - Standard vim motions (use default Evil bindings)
  gg, G      - Jump to top/bottom (use default Evil bindings)
  q, ZZ, ZQ  - Quit window"
  (setq truncate-lines t)
  (setq buffer-read-only t)
  ;; Set up Emacspeak integration
  (when (featurep 'emacspeak)
    (add-hook 'post-command-hook 'reminders--emacspeak-post-command nil t)))

(defun reminders--emacspeak-post-command ()
  "Emacspeak post-command hook for reminders mode."
  (when (and (featurep 'emacspeak)
             (eq major-mode 'reminders-mode)
             (memq this-command '(next-line previous-line)))
    (reminders--emacspeak-speak-line)))

;;; Evil mode integration

(defun reminders--setup-evil-keybindings ()
  "Set up Evil keybindings for reminders-mode."
  (when (and (boundp 'evil-mode) evil-mode)
    ;; Define Evil keybindings in local map
    (evil-local-set-key 'normal (kbd "RET") 'reminders-toggle-complete)
    (evil-local-set-key 'normal (kbd "c") 'reminders-toggle-complete)
    (evil-local-set-key 'normal (kbd "a") 'reminders-add)
    (evil-local-set-key 'normal (kbd "A") 'reminders-add-with-date)
    (evil-local-set-key 'normal (kbd "x") 'reminders-delete)
    (evil-local-set-key 'normal (kbd "X") 'reminders-delete)
    (evil-local-set-key 'normal (kbd "e") 'reminders-edit)
    (evil-local-set-key 'normal (kbd "E") 'reminders-edit)
    (evil-local-set-key 'normal (kbd "r") 'reminders-refresh)
    (evil-local-set-key 'normal (kbd "R") 'reminders-refresh)
    (evil-local-set-key 'normal (kbd "l") 'reminders-switch-list)
    (evil-local-set-key 'normal (kbd "L") 'reminders-show-all-lists)
    (evil-local-set-key 'normal (kbd "t") 'reminders-toggle-show-completed)
    (evil-local-set-key 'normal (kbd "N") 'reminders-add-notes)
    (evil-local-set-key 'normal (kbd "P") 'reminders-set-priority)
    (evil-local-set-key 'normal (kbd "D") 'reminders-set-due-date)
    (evil-local-set-key 'normal (kbd "s") 'reminders-sort-by-due-date)
    (evil-local-set-key 'normal (kbd "q") 'quit-window)
    (evil-local-set-key 'normal (kbd "ZZ") 'quit-window)
    (evil-local-set-key 'normal (kbd "ZQ") 'quit-window)
    (evil-local-set-key 'normal (kbd "?") 'reminders-help)))

(with-eval-after-load 'evil
  ;; Use normal state for vim-like bindings
  (evil-set-initial-state 'reminders-mode 'normal)

  ;; Add hook to setup keybindings when entering reminders-mode
  (add-hook 'reminders-mode-hook 'reminders--setup-evil-keybindings))

;;; Emacspeak advice

(with-eval-after-load 'emacspeak
  ;; Override emacspeak-speak-line for reminders-mode
  (defadvice emacspeak-speak-line (around reminders-mode activate)
    "Use custom line speaking in reminders-mode."
    (if (and (eq major-mode 'reminders-mode)
             (get-text-property (point) 'emacspeak-speak))
        (dtk-speak (get-text-property (point) 'emacspeak-speak))
      ad-do-it))

  (defadvice reminders-show (after emacspeak activate)
    "Provide auditory feedback when opening reminders."
    (when (ems-interactive-p)
      (emacspeak-icon 'open-object)
      (dtk-speak (format "Showing reminders for %s" reminders-current-list))))

  (defadvice reminders-toggle-complete (after emacspeak activate)
    "Provide auditory feedback when toggling completion."
    (when (ems-interactive-p)
      (emacspeak-icon 'select-object)))

  (defadvice reminders-add (after emacspeak activate)
    "Provide auditory feedback when adding a reminder."
    (when (ems-interactive-p)
      (emacspeak-icon 'item)))

  (defadvice reminders-add-with-date (after emacspeak activate)
    "Provide auditory feedback when adding a reminder with date."
    (when (ems-interactive-p)
      (emacspeak-icon 'item)))

  (defadvice reminders-delete (after emacspeak activate)
    "Provide auditory feedback when deleting a reminder."
    (when (ems-interactive-p)
      (emacspeak-icon 'delete-object)))

  (defadvice reminders-refresh (after emacspeak activate)
    "Provide auditory feedback when refreshing."
    (when (ems-interactive-p)
      (emacspeak-icon 'task-done)
      (dtk-speak "Refreshed")))

  (defadvice reminders-switch-list (after emacspeak activate)
    "Provide auditory feedback when switching lists."
    (when (ems-interactive-p)
      (emacspeak-icon 'select-object)
      (dtk-speak (format "Switched to list %s" reminders-current-list))))

  (defadvice reminders-toggle-show-completed (after emacspeak activate)
    "Provide auditory feedback when toggling completed display."
    (when (ems-interactive-p)
      (emacspeak-icon 'button)
      (dtk-speak (if reminders-show-completed
                     "Showing all completed items"
                   "Hiding old completed items"))))

  (defadvice reminders-sort-by-due-date (after emacspeak activate)
    "Provide auditory feedback when changing sort order."
    (when (ems-interactive-p)
      (emacspeak-icon 'select-object)
      (let ((sort-msg (cond
                       ((null reminders-sort-by) "Sorting disabled")
                       (t (format "Sorting by %s %s"
                                  reminders-sort-by
                                  reminders-sort-order)))))
        (dtk-speak sort-msg))))

  (defadvice reminders-edit (after emacspeak activate)
    "Provide auditory feedback when editing a reminder."
    (when (ems-interactive-p)
      (emacspeak-icon 'task-done)
      (dtk-speak "Reminder updated")))

  (defadvice reminders-add-notes (after emacspeak activate)
    "Provide auditory feedback when updating notes."
    (when (ems-interactive-p)
      (emacspeak-icon 'task-done)
      (dtk-speak "Notes updated")))

  (defadvice reminders-set-priority (after emacspeak activate)
    "Provide auditory feedback when setting priority."
    (when (ems-interactive-p)
      (emacspeak-icon 'task-done)
      (dtk-speak "Priority updated")))

  (defadvice reminders-set-due-date (after emacspeak activate)
    "Provide auditory feedback when setting due date."
    (when (ems-interactive-p)
      (emacspeak-icon 'task-done)
      (dtk-speak "Due date updated")))

  (defadvice reminders--save-notes-from-buffer (after emacspeak activate)
    "Provide auditory feedback when saving notes from buffer."
    (when (ems-interactive-p)
      (emacspeak-icon 'save-object)
      (dtk-speak "Notes saved")))

  (defadvice reminders-show-all-lists (after emacspeak activate)
    "Provide auditory feedback when showing all lists."
    (when (ems-interactive-p)
      (emacspeak-icon 'open-object)
      (dtk-speak "Showing all reminder lists")))

  (defadvice reminders-new-list (after emacspeak activate)
    "Provide auditory feedback when creating a new list."
    (when (ems-interactive-p)
      (emacspeak-icon 'item)
      (dtk-speak (format "Created list %s" list-name))))

  (defadvice reminders-quick-add (after emacspeak activate)
    "Provide auditory feedback when quick adding a reminder."
    (when (ems-interactive-p)
      (emacspeak-icon 'item)
      (dtk-speak "Reminder added"))))

(provide 'reminders)

;;; reminders.el ends here
