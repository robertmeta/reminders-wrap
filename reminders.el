;;; reminders.el --- Emacs interface for macOS Reminders CLI -*- lexical-binding: t -*-

;; Author: Robert Melton
;; Version: 1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, tools

;;; Commentary:

;; This package provides an Emacs interface to the macOS `reminders` CLI tool.
;; It allows you to view, add, edit, complete, and delete reminders from within Emacs.

;;; Code:

(require 'json)
(require 'seq)

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
               (index (get-text-property (point) 'reminder-index))
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
                                    index)))
          (dtk-speak speech-text))
        t))))

;;; Display functions

(defun reminders--save-notes-from-buffer ()
  "Save edited notes back to the reminder."
  (interactive)
  (let ((list-name (buffer-local-value 'reminders--notes-list-name (current-buffer)))
        (index (buffer-local-value 'reminders--notes-index (current-buffer)))
        (title (buffer-local-value 'reminders--notes-title (current-buffer)))
        (notes (buffer-substring-no-properties (point-min) (point-max))))
    (when (and list-name index title)
      (reminders--run-command "edit" list-name (number-to-string index)
                              "--notes" notes title)
      (message "Notes saved")
      (quit-window)
      ;; Refresh the reminders buffer if it exists
      (let ((reminders-buffer (get-buffer (format "*Reminders: %s*" list-name))))
        (when reminders-buffer
          (with-current-buffer reminders-buffer
            (reminders-refresh)))))))

(defun reminders--show-notes (notes list-name index title)
  "Display NOTES in an editable popup buffer for reminder at INDEX in LIST-NAME with TITLE."
  (with-current-buffer (get-buffer-create "*Reminder Notes*")
    (erase-buffer)
    (insert notes)
    (goto-char (point-min))
    (text-mode)
    (setq-local reminders--notes-list-name list-name)
    (setq-local reminders--notes-index index)
    (setq-local reminders--notes-title title)
    (local-set-key (kbd "C-c C-c") 'reminders--save-notes-from-buffer)
    (local-set-key (kbd "C-x C-s") 'reminders--save-notes-from-buffer)
    (setq header-line-format "Edit notes - Save with C-c C-c or C-x C-s, quit with q")
    (pop-to-buffer (current-buffer))))

(defun reminders--insert-reminder (reminder index &optional use-letter)
  "Insert REMINDER at INDEX into the buffer.
If USE-LETTER is non-nil, use a letter (a, b, c...) instead of a number."
  (let* ((title (alist-get 'title reminder))
         (due-date (alist-get 'dueDate reminder))
         (priority (or (alist-get 'priority reminder) 0))
         (is-completed (eq (alist-get 'isCompleted reminder) t))
         (notes (alist-get 'notes reminder))
         (due-str (reminders--format-due-date due-date))
         (priority-str (reminders--priority-string priority))
         (checkbox (if is-completed "[X]" "[ ]"))
         (face (if is-completed 'shadow 'default))
         (start (point))
         (index-str (if use-letter
                        (string (+ ?a index))
                      (format "%2d" index))))
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
                               (reminders--show-notes notes reminders-current-list index title))
                     'follow-link t
                     'help-echo "Click to view and edit notes"))
    (put-text-property start (point) 'reminder-data reminder)
    (put-text-property start (point) 'reminder-index index)
    (put-text-property start (point) 'face face)
    ;; Add Emacspeak-specific spoken text
    (when (featurep 'emacspeak)
      (let* ((priority-text (cond
                             ((= priority 1) "high priority")
                             ((= priority 5) "medium priority")
                             ((= priority 9) "low priority")
                             (t "")))
             (status-text (if is-completed "completed" "not completed"))
             (spoken-text (format "%s%s%s%s, item %d"
                                  title
                                  (if (string-empty-p priority-text) "" (concat ", " priority-text))
                                  (if (string-empty-p due-str) "" (concat " due " due-str))
                                  (concat ", " status-text)
                                  index)))
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

(defun reminders--display-reminders (list-name)
  "Display reminders from LIST-NAME."
  (message "Loading reminders from %s..." list-name)
  (let* ((args (list "show" list-name))
         ;; Always include completed to show recently completed
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
         (recently-completed (seq-filter 'reminders--recently-completed-p all-reminders))
         ;; Keep all for data storage
         (display-reminders (if reminders-show-completed
                               all-reminders
                             (append active-reminders recently-completed))))
    (setq reminders-reminders-data all-reminders)
    (let ((inhibit-read-only t)
          (index 0))
      (erase-buffer)
      (insert (format "Reminders - %s" list-name))
      (when reminders-show-completed
        (insert " [showing all completed]"))
      (when reminders-sort-by
        (insert (format " [sorted by %s %s]" reminders-sort-by reminders-sort-order)))
      (insert "\n\n")
      (insert "Commands: [RET] toggle  [a] add  [e] edit  [d] delete  [g] refresh  [l] switch list  [t] toggle completed  [q] quit\n")
      (insert "          [N] notes  [P] priority  [D] due date  [s] sort  [L] all lists\n\n")
      (if reminders-show-completed
          ;; Show all reminders as before when in show-completed mode
          (progn
            (if (null all-reminders)
                (insert "No reminders.\n")
              (dolist (reminder all-reminders)
                (reminders--insert-reminder reminder index)
                (setq index (1+ index)))))
        ;; Normal mode: show active with numbers, recently completed with letters
        (if (null active-reminders)
            (insert "No active reminders.\n")
          (dolist (reminder active-reminders)
            (reminders--insert-reminder reminder index)
            (setq index (1+ index))))
        (when recently-completed
          (insert "\nRecently Completed (last 7 days):\n")
          (setq index 0)
          (dolist (reminder recently-completed)
            (reminders--insert-reminder reminder index t)
            (setq index (1+ index))))))
    (message "Loaded %d active, %d recently completed from %s"
             (length active-reminders)
             (length recently-completed)
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

(defun reminders-toggle-complete ()
  "Toggle completion status of reminder at point."
  (interactive)
  (let* ((reminder (reminders--get-reminder-at-point))
         (index (get-text-property (point) 'reminder-index)))
    (when reminder
      (let ((is-completed (eq (alist-get 'isCompleted reminder) t)))
        (if is-completed
            (reminders--run-command "uncomplete" reminders-current-list (number-to-string index))
          (reminders--run-command "complete" reminders-current-list (number-to-string index)))
        (message "%s: %s"
                 (if is-completed "Uncompleted" "Completed")
                 (alist-get 'title reminder))
        (reminders-refresh)))))

(defun reminders-delete ()
  "Delete reminder at point."
  (interactive)
  (let* ((reminder (reminders--get-reminder-at-point))
         (index (get-text-property (point) 'reminder-index)))
    (when reminder
      (when (y-or-n-p (format "Delete '%s'? " (alist-get 'title reminder)))
        (reminders--run-command "delete" reminders-current-list (number-to-string index))
        (message "Deleted: %s" (alist-get 'title reminder))
        (reminders-refresh)))))

(defun reminders-edit (new-title)
  "Edit the reminder at point with NEW-TITLE."
  (interactive
   (let* ((reminder (reminders--get-reminder-at-point))
          (current-title (when reminder (alist-get 'title reminder))))
     (list (read-string "New title: " current-title))))
  (let* ((reminder (reminders--get-reminder-at-point))
         (index (get-text-property (point) 'reminder-index)))
    (when reminder
      (reminders--run-command "edit" reminders-current-list (number-to-string index) new-title)
      (message "Updated: %s" new-title)
      (reminders-refresh))))

(defun reminders-add-notes (notes)
  "Add or update NOTES for reminder at point."
  (interactive
   (let* ((reminder (reminders--get-reminder-at-point))
          (current-notes (when reminder (alist-get 'notes reminder))))
     (list (read-string "Notes: " current-notes))))
  (let* ((reminder (reminders--get-reminder-at-point))
         (index (get-text-property (point) 'reminder-index)))
    (when reminder
      (reminders--run-command "edit" reminders-current-list (number-to-string index)
                              "--notes" notes (alist-get 'title reminder))
      (message "Updated notes")
      (reminders-refresh))))

(defun reminders-set-priority ()
  "Set priority for reminder at point."
  (interactive)
  (let* ((reminder (reminders--get-reminder-at-point))
         (index (get-text-property (point) 'reminder-index))
         (priority (read-string "Priority (1=high, 5=medium, 9=low, 0=none): " "0")))
    (when reminder
      (reminders--run-command "add" reminders-current-list (alist-get 'title reminder)
                              "--priority" priority)
      (reminders--run-command "delete" reminders-current-list (number-to-string index))
      (message "Updated priority")
      (reminders-refresh))))

(defun reminders-set-due-date (date)
  "Set due DATE for reminder at point."
  (interactive "sDue date (YYYY-MM-DD): ")
  (let* ((reminder (reminders--get-reminder-at-point))
         (index (get-text-property (point) 'reminder-index)))
    (when reminder
      (reminders--run-command "add" reminders-current-list (alist-get 'title reminder)
                              "--due-date" date)
      (reminders--run-command "delete" reminders-current-list (number-to-string index))
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

;;; Major mode

(define-derived-mode reminders-mode special-mode "Reminders"
  "Major mode for viewing and managing macOS Reminders.

\\{reminders-mode-map}"
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

(with-eval-after-load 'evil
  (evil-set-initial-state 'reminders-mode 'emacs))

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

  (defadvice reminders-delete (after emacspeak activate)
    "Provide auditory feedback when deleting a reminder."
    (when (ems-interactive-p)
      (emacspeak-icon 'delete-object))))

(provide 'reminders)

;;; reminders.el ends here
