(require 'eem-text-parsers)
(require 'eem-meta)

(evil-define-state mode
  "Mode state."
  :tag " <M> "
  :message "-- MODE --"
  :enable (normal))

;; recall mode in each buffer, default to nil so it isn't undefined
(defvar-local eem-recall nil)

;; registry of known modes
(defvar eem-modes
  (ht))

(defun eem-register-mode (mode)
  "Register MODE-NAME for use with epistemic mode."
  (let ((name (chimera-mode-name mode))
        (entry-hook (chimera-mode-entry-hook mode))
        (exit-hook (chimera-mode-exit-hook mode)))
    (ht-set! eem-modes name mode)
    (add-hook exit-hook #'eem-remember-for-recall)
    (add-hook entry-hook #'eem-reconcile-level)))

(defun eem-unregister-mode (mode)
  "Unregister MODE-NAME."
  (let ((name (chimera-mode-name mode))
        (entry-hook (chimera-mode-entry-hook mode))
        (exit-hook (chimera-mode-exit-hook mode)))
    (ht-remove! eem-modes name)
    (remove-hook exit-hook #'eem-remember-for-recall)
    (remove-hook entry-hook #'eem-reconcile-level)))

(defun eem-enter-mode (mode-name)
  "Enter mode MODE-NAME."
  (chimera-enter-mode (ht-get eem-modes mode-name)))

(defun eem--enter-level (level-number)
  "Enter level LEVEL-NUMBER"
  (let* ((tower (eem--local-tower))
         (tower-height (eem-ensemble-size tower))
         (level-number (max (min level-number
                                 (1- tower-height))
                            0)))
    (let ((mode-name (eem-editing-entity-name
                      (eem-ensemble-member-at-position tower level-number))))
      (eem-enter-mode mode-name)
      (setq eem--current-level level-number))))

(defun eem-enter-lower-level ()
  "Enter lower level."
  (interactive)
  (message "entering lower level")
  (let ((mode-name (symbol-name evil-state)))
    (if (eem-ensemble-member-position-by-name (eem--local-tower)
                                              mode-name)
        (when (> eem--current-level 0)
          (eem--enter-level (1- eem--current-level)))
      ;; "not my tower, not my problem"
      ;; if we exited a buffer via a state that isn't in its tower, then
      ;; returning to it "out of band" would find it still that way,
      ;; and Enter/Escape would a priori do nothing since the mode is still
      ;; outside the local tower. Ordinarily, we would return to this
      ;; buffer in an epistemic mode such as buffer mode, which upon
      ;; exiting would look for a recall. Since that isn't the case
      ;; here, nothing would happen at this point, and this is the spot
      ;; where we could have taken some action had we been more civic
      ;; minded. So preemptively go to a safe "default" as a failsafe,
      ;; which would be overridden by a recall if there is one.
      (message "Not in tower, couldn't take the stairs")
      (eem--enter-appropriate-mode))))

(defun eem--enter-appropriate-mode (&optional buffer)
  "Enter the most appropriate mode in BUFFER.

Priority: (1) provided mode if admissible (i.e. present in tower) [TODO]
          (2) recall if present
          (3) default level for tower (which could default to lowest
              if unspecified - TODO)."
  (with-current-buffer (or buffer (current-buffer))
    (let ((recall-mode (eem--local-recall-mode))
          (default-mode (editing-ensemble-default (eem--local-tower))))
      (if recall-mode
          ;; recall if available
          (progn (eem--clear-local-recall)
                 (eem-enter-mode recall-mode)
                 (message "Invoked RECALL to %s. Level is %s."
                          recall-mode
                          eem--current-level))
        ;; otherwise default for tower
        (eem-enter-mode default-mode)
        (message "Entered tower DEFAULT: %s. Level is %s."
                 default-mode
                 eem--current-level)))))

(defun eem-enter-higher-level ()
  "Enter higher level."
  (interactive)
  (message "entering higher level")
  (let ((mode-name (symbol-name evil-state)))
    (if (eem-ensemble-member-position-by-name (eem--local-tower)
                                              mode-name)
        (when (< eem--current-level
                 (1- (eem-ensemble-size (eem--local-tower))))
          (eem--enter-level (1+ eem--current-level)))
      ;; see note for eem-enter-lower-level
      (eem--enter-appropriate-mode))))

(defun eem-enter-lowest-level ()
  "Enter lowest (manual) level."
  (interactive)
  (eem--enter-level 0))

(defun eem-enter-highest-level ()
  "Enter highest level."
  (interactive)
  (let* ((tower (eem--local-tower))
         (tower-height (eem-ensemble-size tower)))
    (eem--enter-level (- tower-height
                         1))))

(defun eem--extract-selected-level ()
  "Extract the selected level from the current representation"
  (interactive)
  (let* ((level-str (thing-at-point 'line t)))
    (let ((num (string-to-number (eem--parse-level-number level-str))))
      num)))

(defun eem-enter-selected-level ()
  "Enter selected level"
  (interactive)
  (let ((selected-level (eem--extract-selected-level)))
    (with-current-buffer (eem--get-ground-buffer)
      (message "entering level %s in tower %s in buffer %s"
               selected-level
               eem--current-tower-index
               (current-buffer))
      (eem--enter-level selected-level))))

(defun eem-reconcile-level ()
  "Adjust level to match current mode.

If the current mode is present in the current tower, ensure that the
current level reflects the mode's position in the tower."
  (interactive)
  (let* ((mode-name (symbol-name evil-state))
         (level-number
          (eem-ensemble-member-position-by-name (eem--local-tower)
                                                mode-name)))
    (when level-number
      (setq eem--current-level level-number)
      (message "mode %s is in tower; updated level number to %s"
               mode-name
               eem--current-level))))

(defun eem--clear-local-recall (&optional buffer)
  "Clear recall flag if any."
  (with-current-buffer (or buffer (current-buffer))
    (setq-local eem-recall nil)
    (message "cleared recall!")))

(defun eem--local-recall-mode (&optional buffer)
  "Get the recall mode (if any) in the BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    eem-recall))

(defun eem--enter-local-recall-mode (&optional buffer)
  "Enter the recall mode (if any) in the BUFFER.

This should generally not be called directly but rather via
hooks. Only call it directly when entering a recall mode
is precisely the thing to be done."
  (with-current-buffer (or buffer (current-buffer))
    (let ((recall eem-recall))
      (eem--clear-local-recall)
      (when recall
        (eem-enter-mode recall)))))

(defun eem-remember-for-recall (&optional buffer)
  "Remember the current mode for future recall."
  ;; we're relying on the evil state here even though the
  ;; delegation is hydra -> evil. Probably introduce an
  ;; independent state variable, for which the evil state
  ;; variable can be treated as a proxy for now
  (with-current-buffer (or buffer (current-buffer))
    (let ((mode-name (symbol-name evil-state))
          ;; recall should probably be tower-specific and
          ;; meta-level specific, so that
          ;; we can set it upon entry to a meta mode
          (recall eem-recall))
      ;; only set recall here if it is currently in the tower AND
      ;; going to a state outside the tower
      (when (and (eem-ensemble-member-position-by-name (eem--local-tower)
                                                       mode-name)
                 (not (eem-ensemble-member-position-by-name
                       (eem--local-tower)
                       (symbol-name evil-next-state))))
        (eem-set-mode-recall mode-name)
        (message "set recall to %s; next state is %s" mode-name evil-next-state)))))

(defun eem-set-mode-recall (mode-name)
  "Remember the current state to 'recall' it later."
  (setq-local eem-recall mode-name))

(defun eem-serialize-mode (mode)
  "A string representation of a mode."
  (let ((name (eem-editing-entity-name mode)))
    (concat "|―――"
            (number-to-string level-number)
            "―――|"
            " " (if (equal name (editing-ensemble-default tower))
                    (concat "[" name "]")
                  name))))

(defun eem--update-tower (name value)
  "Update tower NAME to VALUE."
  (set (intern (concat "eem-" name "-tower")) value)
  ;; update complex too
  ;; TODO: this seems hacky, should be a "formalized" way of updating
  ;; editing structures so that all containing ones are aware,
  ;; maybe as part of "state modeling"
  (with-current-buffer (eem--get-ground-buffer)
    (setf (nth (seq-position (seq-map #'eem-editing-entity-name
                                      (editing-ensemble-members eem--complex))
                             name)
               (editing-ensemble-members eem--complex))
          value)))

(defun eem--reload-tower ()
  "Reparse and reload tower."
  (interactive)
  (message "reloading tower")
  (condition-case err
      (let* ((fresh-tower (eem-parse-tower-from-buffer))
             (name (eem-editing-entity-name fresh-tower))
             (original-line-number (line-number-at-pos)))
        (eem--update-tower name fresh-tower)
        (setf (buffer-string) "")
        (insert (eem-serialize-tower fresh-tower))
        (eem--tower-view-narrow fresh-tower)
        (evil-goto-line original-line-number))
    (error (message "parse error %s. Reverting tower..." err)
           (eem--tower-view-narrow (eem--ground-tower))
           (eem--tower-view-reflect-ground (eem--ground-tower)))))

(defun eem--add-meta-side-effects ()
  "Add side effects for primitive mode operations while in meta mode."
  ;; this should lookup the appropriate side-effect based on the coordinates
  (advice-add #'my-move-line-down :after #'eem--reload-tower)
  (advice-add #'my-move-line-up :after #'eem--reload-tower))

(defun eem--remove-meta-side-effects ()
  "Remove side effects for primitive mode operations that were added for meta modes."
  (advice-remove #'my-move-line-down #'eem--reload-tower)
  (advice-remove #'my-move-line-up #'eem--reload-tower))

;; TODO: should have a single function that enters
;; any meta-level, incl. mode, tower, etc.
;; this is the function that does the "vertical" escape
;; some may enter new buffers while other may enter new perspectives
;; for now we can just do a simple dispatch here
(defun my-enter-mode-mode ()
  "Enter a buffer containing a textual representation of the
current epistemic tower."
  (interactive)
  (eem-render-tower (eem--local-tower))
  (eem--switch-to-tower eem--current-tower-index) ; TODO: base this on "state" instead
  (eem--set-ui-for-meta-modes)
  (eem--add-meta-side-effects))

(defun my-exit-mode-mode ()
  "Exit mode mode."
  (interactive)
  (let ((ref-buf (eem--get-ground-buffer)))
    (eem--revert-ui)
    (eem--remove-meta-side-effects)
    (kill-matching-buffers (concat "^" eem-buffer-prefix) nil t)
    (switch-to-buffer ref-buf)))


(defhydra hydra-mode (:columns 4
                      :body-pre (evil-mode-state))
  "Mode mode"
  ("j" eem-mode-down "down")
  ("k" eem-mode-up "up")
  ;; TODO: superlatives for selection
  ("J" eem-move-down "move down")
  ("K" eem-move-up "move up")
  ;; TODO: superlatives for moving
  ("+" eem-make-mode "make")  ; allow ivy selection from all registered modes
  ("x" eem-delete-mode "delete")
  ("c" eem-change-mode "change")
  ("m" eem-other-mode "Return to most recent (like Alt-Tab)" :exit t)
  ("u" eem-mode-undo "undo")
  ("C-r" eem-mode-redo "redo")
  ("/" eem-mode-search "search") ; ivy search full list of modes (alternative to s-<> entry)
  ("=" eem-restore-mode "restore") ; to factory defaults?
  ("i" nil "exit" :exit t)
  ("<return>" eem-enter-lower-level "enter lower level" :exit t) ;; ?
  ("<escape>" eem-enter-higher-level "escape to higher level" :exit t)) ;; ?

(global-set-key (kbd "s-k") (lambda ()
                              (interactive)
                              (eem-jump-to-level "mode")))

;; mode mode as the lowest level upon s-Esc, with tower mode above that achieved via s-Esc again, and so on...
;; i.e. once in any meta mode, you should be able to use the usual L00 machinery incl. e.g. line mode
;; maybe tower mode should only operate on towers - and mode mode could take advantage of a similar (but more minimal) representation as tower mode currently has

(provide 'eem-mode-mode)
;;; eem-mode-mode.el ends here
