;;; rigpa-tower-mode.el --- Self-reflective editing modes -*- lexical-binding: t -*-

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
;; A mode to refer to editing towers
;;

;;; Code:

(require 'evil)
(require 'hydra)
(require 'rigpa-mode-mode)
(require 'rigpa-types)

;; make these buffer-local first before abstracting "state"
(defvar rigpa--current-tower-index 0)
(defvar rigpa--last-tower-index 0)
(defvar rigpa--tower-index-on-entry 0)
(defvar rigpa--flashback-tower-index 0)
(defvar rigpa--current-level 0)
;; the "ground" of buffers is a priori themselves,
;; representing a termination of the chain of reference
(defvar-local rigpa--ground-buffer nil)
(make-variable-buffer-local 'rigpa--current-tower-index)
(make-variable-buffer-local 'rigpa--last-tower-index)
(make-variable-buffer-local 'rigpa--tower-index-on-entry)
(make-variable-buffer-local 'rigpa--flashback-tower-index)
(make-variable-buffer-local 'rigpa--current-level)

(defun rigpa--tower (tower-id)
  "The editing tower corresponding to the provided index."
  (interactive)
  (nth tower-id (editing-ensemble-members rigpa--complex)))

(defun rigpa--ground-tower ()
  "The editing tower we are currently in, in relation to the ground buffer."
  (interactive)
  (with-current-buffer (rigpa--get-ground-buffer)
    ;; TODO: sometimes at this point reference buffer
    ;; is *LV* (hydra menu display) instead of the
    ;; actual buffer
    (rigpa--tower rigpa--current-tower-index)))

(defun rigpa--local-tower (&optional buffer)
  "The editing tower we are in, in relation to the present buffer."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (rigpa--tower rigpa--current-tower-index)))

(defun rigpa--tower-view-reflect-ground (tower)
  "Assuming a representation of TOWER in the present buffer, reflect ground state in it."
  (let* ((ground-buffer (rigpa--get-ground-buffer))
         (ground-mode-name (with-current-buffer ground-buffer
                             (symbol-name evil-state)))
         (ground-recall (with-current-buffer ground-buffer
                          rigpa-recall))
         (level (or (rigpa-ensemble-member-position-by-name tower
                                                          ground-mode-name)
                    (and ground-recall
                         (rigpa-ensemble-member-position-by-name tower
                                                               ground-recall))
                    0))
         (tower-height (rigpa-ensemble-size tower)))
    (evil-goto-line (- tower-height level))))

(defun rigpa--tower-view-narrow (tower)
  "Narrow view to actionable part, and reflect ground state."
  (let* ((tower-height (rigpa-ensemble-size tower))
         (start (save-excursion (goto-char (point-min))
                                (point)))
         (end (save-excursion (goto-char (point-min))
                              (forward-line (1- tower-height))
                              (line-end-position))))
    ;; only show the region that can be interacted with, don't show
    ;; the name of the tower
    (narrow-to-region start end)))

(defun rigpa-serialize-tower (tower)
  "A string representation of a tower."
  (let ((tower-height (rigpa-ensemble-size tower))
        (tower-str ""))
    (dolist
        (level-number (reverse
                       (number-sequence 0 (1- tower-height))))
      (let ((mode (rigpa-ensemble-member-at-position tower
                                                   level-number)))
        (setq tower-str
              (concat tower-str
                      (rigpa-serialize-mode mode tower level-number)
                      "\n"))))
    (concat (string-trim tower-str)
            "\n"
            "\n" ":" (upcase (editing-ensemble-name tower)) ":")))

(defun rigpa-parse-tower (tower-str)
  "Derive a tower struct from a string representation."
  (let* ((level-names (rigpa--parse-level-names tower-str))
         (levels (seq-map (lambda (name)
                            (ht-get rigpa-modes name))
                          level-names)))
    (make-editing-ensemble :name (rigpa--parse-tower-name tower-str)
                           :default (rigpa--parse-tower-default-name tower-str)
                           :members levels)))

(defun rigpa-parse-tower-from-buffer (&optional buffer)
  "Parse a tower struct from a BUFFER containing a text representation of it."
  (with-current-buffer (or buffer (current-buffer))
    (widen)
    (let ((tower (rigpa-parse-tower (buffer-string))))
      (rigpa--tower-view-narrow tower)
      tower)))

(defun rigpa-render-tower (tower tower-index ground &optional major-mode)
  "Render a text representation of an editing tower in a buffer."
  (interactive)
  (let* ((major-mode (or major-mode #'rigpa-meta-mode))
         (buffer (rigpa-buffer-create (rigpa--buffer-name tower)
                                      major-mode)))
    (with-current-buffer buffer
      ;; in tower mode, ground buffer is inherited from the original
      ;; to define the chain of reference
      ;; in mode mode, the ground isn't inherited from the
      ;; current buffer; instead, the current buffer itself becomes
      ;; the new ground
      (setq rigpa--ground-buffer ground)
      (rigpa--set-meta-buffer-appearance)
      (insert (rigpa-serialize-tower tower))
      (rigpa--enter-appropriate-mode)
      ;; store the tower index in the buffer so it can be read
      ;; in the tower buffer navigation side effect
      (setq-local rigpa--tower-index tower-index))
    buffer))

(defun rigpa-flashback-to-last-tower ()
  "Switch to the last tower used.

Eventually this should be done via strange loop application
of buffer mode when in a rigpa meta mode, or alternatively,
and perhaps equivalently, by treating 'switch tower' as the
monadic verb in the 'switch buffer' navigation."
  (interactive)
  (with-current-buffer (rigpa--get-ground-buffer)
    ;; setting hydra to exit here would be ideal, but it seems
    ;; the hydra exits prior to this function being run, and there's
    ;; no meta buffer to switch to. so for now, options are
    ;; either to manually hit enter to use the selected tower
    ;; or store the "previous" previous tower upon mode mode entry
    ;; to get around the need for that
    ;; (rigpa--switch-to-tower rigpa--last-tower-index)
    (let ((original-tower-index rigpa--current-tower-index))
      (setq rigpa--current-tower-index rigpa--flashback-tower-index)
      (setq rigpa--flashback-tower-index original-tower-index))
    ;; enter the appropriate level in the new tower
    (rigpa--enter-appropriate-mode)))

(defun rigpa--view-tower-wrapper (orig-fn &rest args)
  "Focus and contextualize the view of the tower, and update the tower
index in the ground buffer to reflect the selected one."
  (let ((result (apply orig-fn args)))
    (let* ((tower-id rigpa--tower-index) ; read from the buffer-local variable
           (tower (with-current-buffer (rigpa--get-ground-buffer)
                    (rigpa--tower tower-id))))
      (rigpa--view-tower tower)
      (rigpa--update-ground-tower-index tower-id))
    result))

(defun rigpa--view-tower (tower)
  "Focus and contextualize the view of the tower."
  (rigpa--tower-view-narrow tower)
  (rigpa--tower-view-reflect-ground tower))

(defun rigpa--update-ground-tower-index (tower-id)
  "Update ground tower index to TOWER-ID."
  (with-current-buffer (rigpa--get-ground-buffer)
    (setq rigpa--current-tower-index tower-id)))

(defun rigpa--add-meta-tower-side-effects ()
  "Add side effects for primitive mode operations while in meta mode."
  ;; this should lookup the appropriate side-effect based on the coordinates
  (advice-add #'switch-to-buffer :around #'rigpa--view-tower-wrapper))

;; ensure the meta and meta-tower's are straight
(defun rigpa--remove-meta-tower-side-effects ()
  "Remove side effects for primitive mode operations that were added for meta modes."
  (advice-remove #'switch-to-buffer #'rigpa--view-tower-wrapper))

(defun rigpa-enter-tower-mode ()
  "Enter a buffer containing a textual representation of the
initial editing tower."
  (interactive)
  (let* ((ground (rigpa--get-ground-buffer))
         (towers (with-current-buffer ground
                   (editing-ensemble-members rigpa--complex))))
    (dotimes (i (length towers))
      (let ((tower (nth i towers)))
        (rigpa-render-tower tower i ground #'rigpa-meta-tower-mode))))
  ;; Store "previous" previous tower to support flashback
  ;; feature seamlessly. This is to get around hydra executing
  ;; functions after exiting rather than before, which loses
  ;; information about the previous tower if flashback is
  ;; being invoked. This is a hacky fix, but it works for now.
  ;; Improve this eventually.
  (setq rigpa--flashback-tower-index rigpa--tower-index-on-entry)
  (setq rigpa--tower-index-on-entry rigpa--current-tower-index)
  (rigpa--add-meta-tower-side-effects)
  (rigpa--set-ui-for-meta-modes)
  (let ((tower-index (with-current-buffer (rigpa--get-ground-buffer)
                       rigpa--current-tower-index)))
    (switch-to-buffer
     (rigpa--buffer-name
      (rigpa--tower tower-index)))))

(defun rigpa-exit-tower-mode ()
  "Exit tower mode."
  (interactive)
  (let ((ref-buf (rigpa--get-ground-buffer)))
    (with-current-buffer ref-buf
      (setq rigpa--last-tower-index rigpa--tower-index-on-entry))
    (rigpa--revert-ui)
    (rigpa--remove-meta-tower-side-effects)
    (kill-matching-buffers (concat "^" rigpa-buffer-prefix) nil t)
    (switch-to-buffer ref-buf)))


(provide 'rigpa-tower-mode)
;;; rigpa-tower-mode.el ends here
