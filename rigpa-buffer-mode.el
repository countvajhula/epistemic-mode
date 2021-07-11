;;; rigpa-buffer-mode.el --- Self-reflective editing modes -*- lexical-binding: t -*-

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
;; A mode to refer to buffers
;;

;;; Code:


(require 'cl-lib)
(require 'evil)
(require 'hydra)
(require 'ivy)
(require 'chimera)
(require 'chimera-hydra)
(require 's)

(defconst rigpa-buffer-ring-name-prefix "rigpa-buffer-ring")

(evil-define-state buffer
  "Buffer state."
  :tag " <B> "
  :message "-- BUFFER --"
  :enable (normal))

(cl-defun rigpa-buffer-create (&optional
                               buffer-name
                               major-mode-to-use
                               &key
                               switch-p)
  "Create a new empty buffer.

If BUFFER-NAME is not provided, the new buffer will be named
“untitled” or “untitled<2>”, “untitled<3>”, etc. The buffer will be
created in the currently active (at the time of command execution)
major mode.
If SWITCH-P is true, switch to the newly created buffer.

Modified from:
URL `http://ergoemacs.org/emacs/emacs_new_empty_buffer.html'
Version 2017-11-01"
  (interactive)
  (let* ((buffer-name (or buffer-name "untitled"))
         (major-mode-to-use (or major-mode-to-use major-mode))
         ($buf (generate-new-buffer buffer-name)))
    (with-current-buffer $buf
      (funcall major-mode-to-use)
      (setq buffer-offer-save t))
    (when switch-p
      (switch-to-buffer $buf))
    $buf))

(defun rigpa-buffer--count-lines-page ()
  "Modified from emacs's built-in count-lines-page to return a list of
   values corresponding to the position in the page."
  (interactive)
  (save-excursion
    (let ((opoint (point)) beg end
	  total before after)
      (forward-page)
      (beginning-of-line)
      (or (looking-at page-delimiter)
	  (end-of-line))
      (setq end (point))
      (backward-page)
      (setq beg (point))
      (setq total (count-lines beg end)
	    before (count-lines beg opoint)
	    after (count-lines opoint end))
      (list total before after))))

(defun rigpa-buffer-info ()
  "get info on current buffer -- similar to Vim's C-g"
  (interactive)
  (-let (((total before after) (rigpa-buffer--count-lines-page))
         bufinfo percentage page-position total-lines)
    (if (= total 0)
        (setq bufinfo (list "-- No lines in buffer --"))
      (setq percentage (floor (* (/ (float before)
                                    total)
                                 100)))
      (setq page-position (concat
                           "-- "
                           (number-to-string percentage)
                           "%"
                           " --"))
      (setq total-lines (concat
				         (number-to-string total)
				         " lines"))
	  (setq bufinfo (list total-lines page-position)))
    (push (buffer-file-name) bufinfo)
    (message "%s" (string-join bufinfo " "))))

(defun rigpa-buffer-set-mark (mark-name)
  "Set a mark"
  (interactive "cMark name?")
  (puthash mark-name (current-buffer) rigpa-buffer-marks-hash)
  (message "Mark '%c' set." mark-name))

(defun rigpa-buffer-get-mark (mark-name)
  "Retrieve a mark"
  (gethash mark-name rigpa-buffer-marks-hash))

(defun rigpa-buffer-return-to-mark (mark-name)
  "Return to mark"
  (interactive "cMark name?")
  (condition-case nil
      (switch-to-buffer (rigpa-buffer-get-mark mark-name))
    (error (message "Buffer no longer exists!"))))

(defun rigpa-buffer-return-to-original ()
  "Return to the buffer we were in at the time of entering
buffer mode."
  (interactive)
  (condition-case nil
      (switch-to-buffer (rigpa-buffer-original-buffer))
    (error (message "Buffer no longer exists!"))))

(defun rigpa-buffer-flash-to-original ()
  "Go momentarily to original buffer and return.

This 'flash' allows the original buffer, rather than the previous one
encountered while navigating to the present one, to be treated as the
last buffer for 'flashback' ('Alt-tab') purposes. The flash should
happen quickly enough not to be noticeable."
  (interactive)
  (unless (equal (current-buffer) (rigpa-buffer-original-buffer))
    (let ((inhibit-redisplay t)) ;; not sure if this is doing anything but FWIW
      (rigpa-buffer-return-to-original)
      (evil-switch-to-windows-last-buffer))))

(defun rigpa-buffer--active-buffers ()
  "Get active buffers."
  (if (eq rigpa--complex rigpa-meta-tower-complex)
      (seq-filter (lambda (buf)
                    (s-starts-with-p rigpa-buffer-prefix (buffer-name buf)))
                  (buffer-list))
    (seq-filter (lambda (buf)
                  (not (s-starts-with-p " " (buffer-name buf))))
                (buffer-list))))

(defun rigpa-buffer-create-ring ()
  "Create the buffer ring upon entry into buffer mode."
  (interactive)
  ;; delete buffer ring and rebuild from scratch each time, for now,
  ;; instead of maintaining a persistent buffer ring via hooks
  (message "CREATING RING...")
  (let* ((ring-name (if (eq rigpa--complex rigpa-meta-tower-complex)
                        "2"
                      "0")) ; TODO: derive from coordinates later
         (buffer-ring-name (concat rigpa-buffer-ring-name-prefix
                                   "-"
                                   ring-name)))
    (buffer-ring-torus-delete-ring buffer-ring-name)
    (dolist (buf (rigpa-buffer--active-buffers))
      (buffer-ring-add buffer-ring-name buf))))

(defun rigpa-buffer--setup-buffer-marks-table ()
  "Initialize the buffer marks hashtable and add an entry for the
current ('original') buffer."
  (interactive)
  (defvar rigpa-buffer-marks-hash
    (make-hash-table :test 'equal))
  (rigpa-buffer--save-original-buffer))

(defun rigpa-buffer--save-original-buffer ()
  "Save current buffer as original buffer."
  (interactive)
  (rigpa-buffer-set-mark ?0))

(defun rigpa-buffer-original-buffer ()
  "Get original buffer identifier"
  (interactive)
  (rigpa-buffer-get-mark ?0))

(defun rigpa-buffer-yank ()
  "Save current buffer identifier."
  (interactive)
  (rigpa-buffer-set-mark ?1))

(defun rigpa-buffer-paste ()
  "Return to yanked buffer."
  (interactive)
  (rigpa-buffer-return-to-mark ?1))

(defun rigpa-buffer-search ()
  "Search for buffer."
  (interactive)
  (rigpa-buffer-return-to-original)
  (ivy-switch-buffer))

(defhydra hydra-buffer (:columns 3
                        :body-pre (progn (rigpa-buffer--setup-buffer-marks-table)
                                         (chimera-hydra-signal-entry chimera-buffer-mode)
                                         (rigpa-buffer-create-ring)) ; maybe put in ad-hoc entry
                        :post (progn (rigpa-buffer-flash-to-original)
                                     (chimera-hydra-portend-exit chimera-buffer-mode t))
                        :after-exit (chimera-hydra-signal-exit chimera-buffer-mode
                                                               #'chimera-handle-hydra-exit))
  "Buffer mode"
  ("s-b" evil-switch-to-windows-last-buffer "switch to last" :exit t)
  ("b" evil-switch-to-windows-last-buffer "switch to last" :exit t)
  ("h" buffer-ring-prev-buffer "previous")
  ("j" ignore nil)
  ("k" ignore nil)
  ("l" buffer-ring-next-buffer "next")
  ("y" rigpa-buffer-yank "yank")
  ("p" rigpa-buffer-paste "paste")
  ("n" (lambda ()
         (interactive)
         (rigpa-buffer-create nil nil :switch-p t))
   "new" :exit t)
  ("m" rigpa-buffer-set-mark "set mark")
  ("'" rigpa-buffer-return-to-mark "return to mark" :exit t)
  ("`" rigpa-buffer-return-to-mark "return to mark" :exit t)
  ("s" rigpa-buffer-search "search" :exit t)
  ("/" rigpa-buffer-search "search" :exit t)
  ("i" ibuffer "list (ibuffer)" :exit t)
  ("x" kill-buffer "delete")
  ("?" rigpa-buffer-info "info" :exit t)
  ("q" rigpa-buffer-return-to-original "return to original" :exit t)
  ("H-m" rigpa-toggle-menu "show/hide this menu")
  ("<return>" rigpa-enter-lower-level "enter lower level" :exit t)
  ("<escape>" rigpa-enter-higher-level "escape to higher level" :exit t))

(defvar chimera-buffer-mode-entry-hook nil
  "Entry hook for rigpa buffer mode.")

(defvar chimera-buffer-mode-exit-hook nil
  "Exit hook for rigpa buffer mode.")

(defvar chimera-buffer-mode
  (make-chimera-mode :name "buffer"
                     :enter #'hydra-buffer/body
                     :pre-entry-hook 'chimera-buffer-mode-entry-hook
                     :post-exit-hook 'chimera-buffer-mode-exit-hook
                     :entry-hook 'evil-buffer-state-entry-hook
                     :exit-hook 'evil-buffer-state-exit-hook))


(provide 'rigpa-buffer-mode)
;;; rigpa-buffer-mode.el ends here
