(require 'eem-mode-mode)

(cl-defstruct editing-ensemble
  "Specification for an editing ensemble."
  name
  ;; TODO: members should be structs implementing an "entity" interface
  (members nil :documentation "A list of members of the editing ensemble.")
  (default nil :documentation "The canonical member of the tower."))

(defvar eem--current-tower-index 0)
(defvar eem--last-tower-index 0)
(defvar eem--tower-index-on-entry 0)
(defvar eem--flashback-tower-index 0)
(defvar eem--current-level 1)
(defvar eem--reference-buffer (current-buffer))
(make-variable-buffer-local 'eem--current-tower-index)
(make-variable-buffer-local 'eem--last-tower-index)
(make-variable-buffer-local 'eem--tower-index-on-entry)
(make-variable-buffer-local 'eem--flashback-tower-index)
(make-variable-buffer-local 'eem--current-level)

(defun eem--get-reference-buffer ()
  "Get the buffer in reference to which epistemic mode is operating."
  (let ((ref-buf (if (string-match (format "^%s"
                                           eem-buffer-prefix)
                                   (buffer-name))
                     eem--reference-buffer
                   (current-buffer))))
    ref-buf))

(defun eem-tower-level-of-mode (tower mode-name)
  (seq-position (editing-ensemble-members tower)
                mode-name))

(defun eem-tower-height (tower)
  "Height of tower."
  (length (editing-ensemble-members tower)))

(defun eem-tower-mode-at-level (tower level)
  "Mode at LEVEL in the TOWER."
  (nth level (editing-ensemble-members tower)))

(defun eem-tower-default-mode (tower)
  "The default mode for tower."
  (editing-ensemble-default tower))

(defun eem--tower (tower-id)
  "The epistemic tower corresponding to the provided index."
  (interactive)
  (nth tower-id eem-towers))

(defun eem--current-tower ()
  "The epistemic editing tower we are currently in."
  (interactive)
  (with-current-buffer (eem--get-reference-buffer)
    ;; TODO: sometimes at this point reference buffer
    ;; is *LV* (hydra menu display) instead of the
    ;; actual buffer
    (eem--tower eem--current-tower-index)))

(defun eem-previous-tower ()
  "Previous tower"
  (interactive)
  (with-current-buffer (eem--get-reference-buffer)
    (let ((tower-id (mod (- eem--current-tower-index
                           1)
                        (length eem-towers))))
     (eem--switch-to-tower tower-id))))

(defun eem-next-tower ()
  "Next tower"
  (interactive)
  (with-current-buffer (eem--get-reference-buffer)
    (let ((tower-id (mod (+ eem--current-tower-index
                           1)
                        (length eem-towers))))
     (eem--switch-to-tower tower-id))))

(defun eem--switch-to-tower (tower-id)
  "Switch to the tower indicated"
  (interactive)
  (let* ((tower (eem--tower tower-id))
         (tower-height (eem-tower-height tower))
         (current-mode-name (symbol-name evil-state))
         (level (or (eem-tower-level-of-mode tower
                                             current-mode-name)
                    (eem-tower-level-of-mode tower
                                             eem-recall)
                    0)))
    (switch-to-buffer (eem--buffer-name tower))
    (let ((start (progn (evil-goto-line 1) (line-beginning-position)))
          (end (progn (evil-goto-line tower-height) (line-end-position))))
      ;; only show the region that can be interacted with, don't show
      ;; the name of the tower
      (narrow-to-region start end))
    (evil-goto-line (- tower-height level))
    (with-current-buffer (eem--get-reference-buffer)
      (setq eem--current-tower-index tower-id))
    (eem--extract-selected-level)))

(defun eem--buffer-name (tower)
  "Buffer name to use for a given tower."
  (concat eem-buffer-prefix "-" (editing-ensemble-name tower)))

(defun eem-serialize-tower (tower)
  "A string representation of a tower."
  (let ((tower-height (eem-tower-height tower))
        (tower-str ""))
    (dolist
        (level-number (reverse
                       (number-sequence 0 (1- tower-height))))
      (let ((mode-name
             (eem-tower-mode-at-level tower
                                      level-number)))
        (setq tower-str
              (concat tower-str
                      "|―――"
                      (number-to-string level-number)
                      "―――|"
                      " " (if (equal mode-name (eem-tower-default-mode tower))
                              (concat "[" mode-name "]")
                              mode-name)
                      "\n"))))
    (concat (string-trim tower-str)
            "\n"
            "\n-" (upcase (editing-ensemble-name tower)) "-")))

(defun eem-parse-tower (tower-str)
  "Derive a tower struct from a string representation."
  (make-editing-ensemble :name (parsec-with-input tower-str
                                 (eem--parse-tower-name))
                         :default (parsec-with-input tower-str
                                    (eem--parse-tower-default-mode))
                         :members (parsec-with-input tower-str
                                    (eem--parse-tower-level-names))))

(defun eem--set-meta-buffer-appearance ()
  "Configure meta mode buffer appearance."
  (buffer-face-set 'eem-face)
  (text-scale-set 5)
  ;;(setq cursor-type nil))
  (internal-show-cursor nil nil)
  (display-line-numbers-mode 'toggle)
  (hl-line-mode))

(defun eem-render-tower (tower)
  "Render a text representation of an epistemic editing tower in a buffer."
  (interactive)
  (let ((tower-buffer
         (my-new-empty-buffer (eem--buffer-name tower))))
    (with-current-buffer tower-buffer
      (eem--set-meta-buffer-appearance)
      (insert (eem-serialize-tower tower)))
    tower-buffer))

(defun eem--set-ui-for-meta-modes ()
  "Set (for now, global) UI parameters for meta modes."
  ;; should ideally be perspective-specific
  (blink-cursor-mode -1))

(defun eem--revert-ui ()
  "Revert buffer appearance to settings prior to entering mode mode."
  (blink-cursor-mode 1))

(defun my-enter-tower-mode ()
  "Enter a buffer containing a textual representation of the
initial epistemic tower."
  (interactive)
  (setq eem--reference-buffer (current-buffer))
  (dolist (tower eem-towers)
    (eem-render-tower tower))
  (with-current-buffer (eem--get-reference-buffer)
    ;; Store "previous" previous tower to support flashback
    ;; feature seamlessly. This is to get around hydra executing
    ;; functions after exiting rather than before, which loses
    ;; information about the previous tower if flashback is
    ;; being invoked. This is a hacky fix, but it works for now.
    ;; Improve this eventually.
    (setq eem--flashback-tower-index eem--tower-index-on-entry)
    (setq eem--tower-index-on-entry eem--current-tower-index)
    (eem--switch-to-tower eem--current-tower-index))
  (eem--set-ui-for-meta-modes))

(defun my-exit-tower-mode ()
  "Exit tower mode."
  (interactive)
  (let ((ref-buf (eem--get-reference-buffer)))
    (with-current-buffer ref-buf
      (setq eem--last-tower-index eem--tower-index-on-entry))
    (eem--revert-ui)
    (kill-matching-buffers (concat "^" eem-buffer-prefix) nil t)
    (switch-to-buffer ref-buf)))

(defun eem-flashback-to-last-tower ()
  "Switch to the last tower used.

Eventually this should be done via strange loop application
of buffer mode when in epistemic mode, or alternatively,
and perhaps equivalently, by treating 'switch tower' as the
monadic verb in the 'switch buffer' navigation."
  (interactive)
  (with-current-buffer (eem--get-reference-buffer)
    ;; setting hydra to exit here would be ideal, but it seems
    ;; the hydra exits prior to this function being run, and there's
    ;; no epistemic buffer to switch to. so for now, options are
    ;; either to manually hit enter to use the selected tower
    ;; or store the "previous" previous tower upon mode mode entry
    ;; to get around the need for that
    ;; (eem--switch-to-tower eem--last-tower-index)
    (let ((original-tower-index eem--current-tower-index))
      (setq eem--current-tower-index eem--flashback-tower-index)
      (setq eem--flashback-tower-index original-tower-index))
    ;; enter the appropriate level in the new tower
    (eem--enter-appropriate-mode)))

(defun eem-enter-selected-level ()
  "Enter selected level"
  (interactive)
  (with-current-buffer (eem--get-reference-buffer)
    (message "entering level %s in tower %s" eem--selected-level eem--current-tower-index)
    (eem--enter-level eem--selected-level)))

(defhydra hydra-tower (:idle 1.0
                       :columns 4
                       :body-pre (my-enter-tower-mode)
                       :post (my-exit-tower-mode))
  "Tower mode"
  ;; Need a textual representation of the mode tower for these to operate on
  ("h" eem-previous-tower "previous tower")
  ("j" eem-select-next-level "lower level")
  ("k" eem-select-previous-level "higher level")
  ("l" eem-next-tower "next tower")
  ;; ("H" eem-highest-level "first level (recency)")
  ;; ("J" eem-lowest-level "lowest level")
  ;; ("K" eem-highest-level "highest level")
  ;; ("L" eem-lowest-level "last level (recency)")
  ;; different towers for different "major modes"
  ;; ("s-o" eem-mode-mru "Jump to most recent (like Alt-Tab)" :exit t)
  ;; ("o" eem-mode-mru :exit t)
  ;; with delete / change etc. we could construct towers and then select towers
  ;; there could be a maximal tower containing all the levels
  ;; ("/" eem-search "search")
  ;; move to change ordering of levels, an alternative to recency
  ;;
  ;; ffap other window -- open file with this other mode/tower: a formal "major mode"
  ;; the mode mode, tower mode, and so on recursively makes more sense
  ;; if we assume that keyboard shortcuts are scarce. this gives us ways to use
  ;; a small number of keys in any arbitrary configuration
  ("s-m" eem-flashback-to-last-tower "flashback" :exit t)  ; canonical action
  ("<return>" eem-enter-selected-level "enter selected level" :exit t)
  ("s-<return>" eem-enter-selected-level "enter selected level" :exit t)
  ("i" nil "exit" :exit t)
  ("<escape>" nil "exit" :exit t))
  ;("s-<return>" eem-enter-lower-level "enter lower level" :exit t)
  ;("s-<escape>" eem-enter-higher-level "escape to higher level" :exit t))


(provide 'eem-tower-mode)
