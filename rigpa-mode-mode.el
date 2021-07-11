;;; rigpa-mode-mode.el --- Self-reflective editing modes -*- lexical-binding: t -*-

;; URL: https://github.com/countvajhula/rigpa

;; This program is "part of the world," in the sense described at
;; http://drym.org.  From your perspective, this is no different than
;; MIT or BSD or other such "liberal" licenses that you may be
;; familiar with, that is to say, you are free to do whatever you like
;; with this program.  It is much more than BSD or MIT, however, in
;; that it isn't a license at all but an idea about the world and how
;; economic systems could be set up so that everyone wins.  Learn more
;; at drym.org.
;;
;; This work transcends traditional legal and economic systems, but
;; for the purposes of any such systems within which you may need to
;; operate:
;;
;; This is free and unencumbered software released into the public domain.
;; The authors relinquish any copyright claims on this work.
;;

;;; Commentary:
;;
;; A mode to refer to modes
;;

;;; Code:

(require 'ht)
(require 'evil)
(require 'rigpa-text-parsers)
(require 'rigpa-meta)

(evil-define-state mode
  "Mode state."
  :tag " <M> "
  :message "-- MODE --"
  :enable (normal))

;; recall mode in each buffer, default to nil so it isn't undefined
(defvar-local rigpa-recall nil)

;; registry of known modes
(defvar rigpa-modes
  (ht))

(defun rigpa--minor-mode-enable-hook (name)
  "Return a function to enable the minor mode for the mode named NAME.

We modulate keybindings in evil states (e.g. in particular visual and
operator states) via a minor mode. As it looks like this only works if
the minor mode is activated *before* entering the evil state, we need
to define pre-entry hooks at the chimera level and can't just use the
evil entry hooks.

We need this extra layer of indirection because lambdas as hooks
can't be removed since they are anonymous. This just gives us a way
to parametrize the hook but still be able to remove it."
  (let ((enable-mode
         (intern
          (concat "rigpa--enable-" name "-minor-mode"))))
    enable-mode))

(defun rigpa--on-mode-entry (name)
  "Return the function that takes actions upon mode entry."
  (let ((on-entry
         (intern
          (concat "rigpa--on-" name "-mode-entry"))))
    on-entry))

(defun rigpa--on-mode-exit (name)
  "Return the function that takes actions upon mode exit."
  (let ((on-exit
         (intern
          (concat "rigpa--on-" name "-mode-exit"))))
    on-exit))

(defun rigpa--disable-other-minor-modes ()
  "Disable all rigpa mode minor modes.

This is called on state transitions to ensure that all minor modes are
first disabled prior to the minor mode for new state being enabled."
  (dolist (name (ht-keys rigpa-modes))
    (let ((disable-mode
           (intern
            (concat "rigpa--disable-" name "-minor-mode"))))
      (when (fboundp disable-mode)
        (funcall disable-mode)))))

(defun rigpa-register-mode (mode)
  "Register MODE for use with rigpa.

This registers callbacks with the hooks provided by the chimera MODE
to ensure, upon state transitions, that:
(1) the correct state is reflected,
(2) any lingering config from prior states is cleaned, and
(3) the previous state is remembered for possible recall."
  (let ((name (chimera-mode-name mode))
        (pre-entry-hook (chimera-mode-pre-entry-hook mode))
        (entry-hook (chimera-mode-entry-hook mode))
        (exit-hook (chimera-mode-exit-hook mode)))
    (ht-set! rigpa-modes name mode)
    (let ((minor-mode-entry (rigpa--minor-mode-enable-hook name)))
      (when (fboundp minor-mode-entry)
        (add-hook pre-entry-hook minor-mode-entry)))
    (let ((on-mode-entry (rigpa--on-mode-entry name)))
      (when (fboundp on-mode-entry)
        (add-hook pre-entry-hook on-mode-entry)))
    (let ((on-mode-exit (rigpa--on-mode-exit name)))
      (when (fboundp on-mode-exit)
        (add-hook exit-hook on-mode-exit)))
    (add-hook entry-hook #'rigpa-reconcile-level)
    (add-hook pre-entry-hook #'rigpa--disable-other-minor-modes)
    (add-hook exit-hook #'rigpa-remember-for-recall)))

(defun rigpa-unregister-mode (mode)
  "Unregister MODE."
  (let ((name (chimera-mode-name mode))
        (pre-entry-hook (chimera-mode-pre-entry-hook mode))
        (entry-hook (chimera-mode-entry-hook mode))
        (exit-hook (chimera-mode-exit-hook mode)))
    (ht-remove! rigpa-modes name)
    (let ((minor-mode-entry (rigpa--minor-mode-enable-hook name)))
      (when (fboundp minor-mode-entry)
        (remove-hook pre-entry-hook minor-mode-entry)))
    (let ((on-mode-entry (rigpa--on-mode-entry name)))
      (when (fboundp on-mode-entry)
        (remove-hook pre-entry-hook on-mode-entry)))
    (let ((on-mode-exit (rigpa--on-mode-exit name)))
      (when (fboundp on-mode-exit)
        (remove-hook exit-hook on-mode-exit)))
    (remove-hook entry-hook #'rigpa-reconcile-level)
    (remove-hook pre-entry-hook #'rigpa--disable-other-minor-modes)
    (remove-hook exit-hook #'rigpa-remember-for-recall)))

(defun rigpa-current-mode ()
  "Current rigpa mode."
  (chimera--mode-for-state (symbol-name evil-state)))

(defun rigpa-enter-mode (mode-name)
  "Enter mode MODE-NAME."
  (chimera-enter-mode (ht-get rigpa-modes mode-name)))

(defun rigpa--enter-level (level-number)
  "Enter level LEVEL-NUMBER"
  (let* ((tower (rigpa--local-tower))
         (tower-height (rigpa-ensemble-size tower))
         (level-number (max (min level-number
                                 (1- tower-height))
                            0)))
    (let ((mode-name (rigpa-editing-entity-name
                      (rigpa-ensemble-member-at-position tower level-number))))
      (rigpa-enter-mode mode-name)
      (setq rigpa--current-level level-number))))

(defun rigpa-enter-lower-level ()
  "Enter lower level."
  (interactive)
  (let ((mode-name (symbol-name evil-state)))
    (if (rigpa-ensemble-member-position-by-name (rigpa--local-tower)
                                                mode-name)
        (when (> rigpa--current-level 0)
          (rigpa--enter-level (1- rigpa--current-level)))
      ;; "not my tower, not my problem"
      ;; if we exited a buffer via a state that isn't in its tower, then
      ;; returning to it "out of band" would find it still that way,
      ;; and Enter/Escape would a priori do nothing since the mode is still
      ;; outside the local tower. Ordinarily, we would return to this
      ;; buffer in a rigpa mode such as buffer mode, which upon
      ;; exiting would look for a recall. Since that isn't the case
      ;; here, nothing would happen at this point, and this is the spot
      ;; where we could have taken some action had we been more civic
      ;; minded. So preemptively go to a safe "default" as a failsafe,
      ;; which would be overridden by a recall if there is one.
      (rigpa--enter-appropriate-mode))))

(defun rigpa--enter-appropriate-mode (&optional buffer)
  "Enter the most appropriate mode in BUFFER.

Priority: (1) provided mode if admissible (i.e. present in tower) [TODO]
          (2) recall if present
          (3) default level for tower (which could default to lowest
              if unspecified - TODO)."
  (with-current-buffer (or buffer (current-buffer))
    (let* ((current-mode (rigpa-current-mode))
           (current-mode-name (chimera-mode-name current-mode))
           (recall-mode-name (rigpa--local-recall-mode))
           (default-mode-name (editing-ensemble-default (rigpa--local-tower))))
      (cond ((rigpa--member-of-ensemble-p current-mode
                                          (rigpa--local-tower))
             ;; we don't want to do anything in this case,
             ;; but re-enter the current mode to ensure
             ;; that it reconciles state with the new tower
             (rigpa-enter-mode current-mode-name))
            (recall-mode-name
             ;; recall if available
             (rigpa--clear-local-recall)
             (rigpa-enter-mode recall-mode-name))
            ;; otherwise default for tower
            (t (rigpa-enter-mode default-mode-name))))))

(defun rigpa-enter-higher-level ()
  "Enter higher level."
  (interactive)
  (let ((mode-name (symbol-name evil-state)))
    (if (rigpa-ensemble-member-position-by-name (rigpa--local-tower)
                                                mode-name)
        (when (< rigpa--current-level
                 (1- (rigpa-ensemble-size (rigpa--local-tower))))
          (rigpa--enter-level (1+ rigpa--current-level)))
      ;; see note for rigpa-enter-lower-level
      (rigpa--enter-appropriate-mode))))

(defun rigpa-enter-lowest-level ()
  "Enter lowest (manual) level."
  (interactive)
  (rigpa--enter-level 0))

(defun rigpa-enter-highest-level ()
  "Enter highest level."
  (interactive)
  (let* ((tower (rigpa--local-tower))
         (tower-height (rigpa-ensemble-size tower)))
    (rigpa--enter-level (- tower-height
                         1))))

(defun rigpa--extract-selected-level ()
  "Extract the selected level from the current representation"
  (interactive)
  (let* ((level-str (thing-at-point 'line t)))
    (let ((num (string-to-number (rigpa--parse-level-number level-str))))
      num)))

(defun rigpa-enter-selected-level ()
  "Enter selected level"
  (interactive)
  (let ((selected-level (rigpa--extract-selected-level)))
    (with-current-buffer (rigpa--get-ground-buffer)
      (rigpa--enter-level selected-level))))

(defun rigpa-reconcile-level ()
  "Adjust level to match current mode.

If the current mode is present in the current tower, ensure that the
current level reflects the mode's position in the tower."
  (interactive)
  (let* ((mode-name (symbol-name evil-state))
         (level-number
          (rigpa-ensemble-member-position-by-name (rigpa--local-tower)
                                                  mode-name)))
    (when level-number
      (setq rigpa--current-level level-number))))

(defun rigpa--clear-local-recall (&optional buffer)
  "Clear recall flag if any."
  (with-current-buffer (or buffer (current-buffer))
    (setq-local rigpa-recall nil)))

(defun rigpa--local-recall-mode (&optional buffer)
  "Get the recall mode (if any) in the BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    rigpa-recall))

(defun rigpa--enter-local-recall-mode (&optional buffer)
  "Enter the recall mode (if any) in the BUFFER.

This should generally not be called directly but rather via
hooks. Only call it directly when entering a recall mode
is precisely the thing to be done."
  (with-current-buffer (or buffer (current-buffer))
    (let ((recall rigpa-recall))
      (rigpa--clear-local-recall)
      (when recall
        (rigpa-enter-mode recall)))))

(defun rigpa-remember-for-recall (&optional buffer)
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
          (recall rigpa-recall))
      ;; only set recall here if it is currently in the tower AND
      ;; going to a state outside the tower
      (when (and (rigpa-ensemble-member-position-by-name (rigpa--local-tower)
                                                       mode-name)
                 (not (rigpa-ensemble-member-position-by-name
                       (rigpa--local-tower)
                       (symbol-name evil-next-state))))
        (rigpa-set-mode-recall mode-name)))))

(defun rigpa-set-mode-recall (mode-name)
  "Remember the current state to 'recall' it later."
  (setq-local rigpa-recall mode-name))

(defun rigpa-serialize-mode (mode tower level-number)
  "A string representation of a mode."
  (let ((name (rigpa-editing-entity-name mode)))
    (concat "|―――"
            (number-to-string level-number)
            "―――|"
            " " (if (equal name (editing-ensemble-default tower))
                    (concat "[" name "]")
                  name))))

(defun rigpa--mode-mode-change (orig-fn &rest args)
  "Change mode."
  (interactive)
  (beginning-of-line)
  (evil-forward-WORD-begin)
  (evil-change-line (point) (line-end-position)))

(defun rigpa--update-tower (name value)
  "Update tower NAME to VALUE."
  (set (intern (concat "rigpa-" name "-tower")) value)
  ;; update complex too
  ;; TODO: this seems hacky, should be a "formalized" way of updating
  ;; editing structures so that all containing ones are aware,
  ;; maybe as part of "state modeling"
  (with-current-buffer (rigpa--get-ground-buffer)
    (setf (nth (seq-position (seq-map #'rigpa-editing-entity-name
                                      (editing-ensemble-members rigpa--complex))
                             name)
               (editing-ensemble-members rigpa--complex))
          value)))

(defun rigpa--reload-tower ()
  "Reparse and reload tower."
  (interactive)
  (condition-case err
      (let* ((fresh-tower (rigpa-parse-tower-from-buffer))
             (name (rigpa-editing-entity-name fresh-tower))
             (original-line-number (line-number-at-pos)))
        (rigpa--update-tower name fresh-tower)
        (setf (buffer-string) "")
        (insert (rigpa-serialize-tower fresh-tower))
        (rigpa--tower-view-narrow fresh-tower)
        (evil-goto-line original-line-number))
    (error (message "parse error %s. Reverting tower..." err)
           (rigpa--tower-view-narrow (rigpa--ground-tower))
           (rigpa--tower-view-reflect-ground (rigpa--ground-tower)))))

(defun rigpa--reload-tower-wrapper (orig-fn count &rest args)
  "Wrap interactive commands and reload the tower.

For interactive commands accepting a count argument, we can't use just
any function as advice since the underying command expects to receive
an interactive argument from the user. The advising function needs to
be interactive itself."
  (interactive "p")
  (let ((result (apply orig-fn count args)))
    (rigpa--reload-tower)
    result))

(defun rigpa--add-meta-side-effects ()
  "Add side effects for primitive mode operations while in meta mode."
  ;; this should lookup the appropriate side-effect based on the
  ;; coordinates and the ground level mode being employed
  (advice-add #'rigpa-line-move-down :around #'rigpa--reload-tower-wrapper)
  (advice-add #'rigpa-line-move-up :around #'rigpa--reload-tower-wrapper)
  (advice-add #'rigpa-line-change :around #'rigpa--mode-mode-change)
  (advice-add #'switch-to-buffer :around #'rigpa--view-tower-wrapper))

(defun rigpa--remove-meta-side-effects ()
  "Remove side effects for primitive mode operations that were added for meta modes."
  (advice-remove #'rigpa-line-move-down #'rigpa--reload-tower-wrapper)
  (advice-remove #'rigpa-line-move-up #'rigpa--reload-tower-wrapper)
  (advice-remove #'rigpa-line-change #'rigpa--mode-mode-change)
  (advice-remove #'switch-to-buffer #'rigpa--view-tower-wrapper))

;; TODO: should have a single function that enters
;; any meta-level, incl. mode, tower, etc.
;; this is the function that does the "vertical" escape
;; some may enter new buffers while other may enter new perspectives
;; for now we can just do a simple dispatch here
(defun rigpa-enter-mode-mode ()
  "Enter a buffer containing a textual representation of the
current editing tower."
  (interactive)
  (rigpa--set-ui-for-meta-modes) ; TODO: do this only for meta buffers
  (rigpa-render-tower (rigpa--local-tower)
                      rigpa--current-tower-index
                      (current-buffer))
  (rigpa--add-meta-side-effects)
  (let ((tower-index (with-current-buffer (rigpa--get-ground-buffer)
                       rigpa--current-tower-index)))
    (switch-to-buffer             ; TODO: base this on "state" instead
     (rigpa--buffer-name
      (rigpa--tower tower-index)))))

(defun rigpa-exit-mode-mode ()
  "Exit mode mode."
  (interactive)
  (let ((ref-buf (rigpa--get-ground-buffer)))
    (rigpa--revert-ui)
    (rigpa--remove-meta-side-effects)
    (when (eq (with-current-buffer ref-buf
                (rigpa--get-ground-buffer))
              ref-buf)
      (kill-matching-buffers (concat "^" rigpa-buffer-prefix) nil t))
    (switch-to-buffer ref-buf)))


(provide 'rigpa-mode-mode)
;;; rigpa-mode-mode.el ends here
