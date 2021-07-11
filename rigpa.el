;;; rigpa.el --- Self-reflective editing modes -*- lexical-binding: t -*-

;; Author: Siddhartha Kasivajhula <sid@countvajhula.com>
;; URL: https://github.com/countvajhula/rigpa
;; Version: 0.1
;; Package-Requires: ((emacs "26.1") (evil "1.2.14") (hydra "0.15.0") (symex "0.8.1") (ivy "0.13.0") (centaur-tabs "3.1") (beacon "1.3.4") (dictionary "1.11") (ace-window "0.9.0") (git-timemachine "4.11") (parsec "0.1.3") (ht "2.0") (s "1.12.0") (dash "2.18.0"))
;; Keywords: emulations, frames, convenience

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

;;; Commentary:
;;
;; Rigpa allows you to construct "towers" of editing modes
;; (in principle, these could be any evil modes) and manipulate which
;; tower is active at any point in time.  It generalizes both vim's
;; notion of editing mode, as well as Emacs's notion of a major mode
;; into a unified way of looking at things, that is, as towers of
;; modes which can be swapped and themselves edited using the very
;; modes they contain.
;;
;; In addition, rigpa also defines conventions that modes
;; should follow in order to be seamlessly integrated
;; into editing towers.  This includes conventions around keybindings
;; for moving up and down the hierarchy of editing levels, standard
;; semantics of modifier keys, defining a canonical action for each
;; mode, and other such conventions to ensure semantic uniformity across
;; editing levels.

;;; Code:

(require 'evil)
(require 'hydra)
(require 'chimera)
(require 'ht)
(require 'rigpa-char-mode)
(require 'rigpa-word-mode)
(require 'rigpa-line-mode)
(require 'symex)
(require 'rigpa-symex-mode)
(require 'rigpa-view-mode)
(require 'rigpa-window-mode)
(require 'rigpa-file-mode)
(require 'rigpa-buffer-mode)
(require 'rigpa-system-mode)
(require 'rigpa-application-mode)
(require 'rigpa-activity-mode)
(require 'rigpa-text-mode)
(require 'rigpa-tab-mode)
(require 'rigpa-history-mode)
(require 'rigpa-tower-mode)

;; define face for use in meta modes
(make-face 'rigpa-face)
(set-face-font 'rigpa-face "-*-Consolas-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1")
(set-face-foreground 'rigpa-face "tomato")

;; the prefix that will be used in naming all buffers used
;; in meta mode representations
(defvar rigpa-buffer-prefix "*RIGPA-META*")

(defun rigpa-hide-menu (mode-name)
  "Hide current mode menu."
  (unless (member mode-name chimera-evil-states)
    ;; only supported for hydra
    (let ((mode-hydra (intern (concat "hydra-" mode-name))))
      (hydra-set-property mode-hydra :verbosity 0))))

(defun rigpa-show-menu (mode-name)
  "Show current mode menu."
  (unless (member mode-name chimera-evil-states)
    ;; only supported for hydra
    (let ((mode-hydra (intern (concat "hydra-" mode-name))))
      (hydra-set-property mode-hydra :verbosity 2))))

(defun rigpa-toggle-menu ()
  "Show/hide the current mode menu.

Note that hiding the menu still retains the current editing mode,
and simply toggles whether the menu is visible or not."
  (interactive)
  (let* ((mode-name (symbol-name evil-state))
         (mode-hydra (intern (concat "hydra-" mode-name))))
    (let ((visibility (hydra-get-property mode-hydra :verbosity)))
      (if (or (eq nil visibility)
              (> visibility 0))
          (rigpa-hide-menu mode-name)
        (rigpa-show-menu mode-name)))))

(define-derived-mode rigpa-meta-mode
  text-mode "Meta"
  "Major mode for meta modes"
  (define-key rigpa-meta-mode-map (kbd "g r") 'rigpa--reload-tower))

(define-derived-mode rigpa-meta-tower-mode
  text-mode "Tower"
  "Major mode for meta modes"
  (define-key rigpa-meta-tower-mode-map (kbd "g r") 'rigpa--reload-tower))

;; wrap native evil states in chimera modes
(defvar chimera-normal-mode-entry-hook nil
  "Entry hook for rigpa normal mode.")

(defvar chimera-normal-mode-exit-hook nil
  "Exit hook for rigpa normal mode.")

(defvar chimera-normal-mode
  (make-chimera-mode :name "normal"
                     :enter #'evil-normal-state
                     :pre-entry-hook 'chimera-normal-mode-entry-hook
                     :post-exit-hook 'chimera-normal-mode-exit-hook
                     :entry-hook 'evil-normal-state-entry-hook
                     :exit-hook 'evil-normal-state-exit-hook))

(defvar chimera-insert-mode-entry-hook nil
  "Entry hook for rigpa insert mode.")

(defvar chimera-insert-mode-exit-hook nil
  "Exit hook for rigpa insert mode.")

(defvar chimera-insert-mode
  (make-chimera-mode :name "insert"
                     :enter #'evil-insert-state
                     :pre-entry-hook 'chimera-insert-mode-entry-hook
                     :post-exit-hook 'chimera-insert-mode-exit-hook
                     :entry-hook 'evil-insert-state-entry-hook
                     :exit-hook 'evil-insert-state-exit-hook))

(defvar chimera-emacs-mode-entry-hook nil
  "Entry hook for rigpa emacs mode.")

(defvar chimera-emacs-mode-exit-hook nil
  "Exit hook for rigpa emacs mode.")

(defvar chimera-emacs-mode
  (make-chimera-mode :name "emacs"
                     :enter #'evil-emacs-state
                     :pre-entry-hook 'chimera-emacs-mode-entry-hook
                     :post-exit-hook 'chimera-emacs-mode-exit-hook
                     :entry-hook 'evil-emacs-state-entry-hook
                     :exit-hook 'evil-emacs-state-exit-hook))

(defvar chimera-visual-mode-entry-hook nil
  "Entry hook for rigpa visual mode.")

(defvar chimera-visual-mode-exit-hook nil
  "Exit hook for rigpa visual mode.")

(defvar chimera-visual-mode
  (make-chimera-mode :name "visual"
                     :enter #'evil-visual-state
                     :pre-entry-hook 'chimera-visual-mode-entry-hook
                     :post-exit-hook 'chimera-visual-mode-exit-hook
                     :entry-hook 'evil-visual-state-entry-hook
                     :exit-hook 'evil-visual-state-exit-hook))

(defvar chimera-replace-mode-entry-hook nil
  "Entry hook for rigpa replace mode.")

(defvar chimera-replace-mode-exit-hook nil
  "Exit hook for rigpa replace mode.")

(defvar chimera-replace-mode
  (make-chimera-mode :name "replace"
                     :enter #'evil-replace-state
                     :pre-entry-hook 'chimera-replace-mode-entry-hook
                     :post-exit-hook 'chimera-replace-mode-exit-hook
                     :entry-hook 'evil-replace-state-entry-hook
                     :exit-hook 'evil-replace-state-exit-hook))

(defvar chimera-operator-mode-entry-hook nil
  "Entry hook for rigpa operator mode.")

(defvar chimera-operator-mode-exit-hook nil
  "Exit hook for rigpa operator mode.")

(defvar chimera-operator-mode
  (make-chimera-mode :name "operator"
                     :enter #'evil-operator-state
                     :pre-entry-hook 'chimera-operator-mode-entry-hook
                     :post-exit-hook 'chimera-operator-mode-exit-hook
                     :entry-hook 'evil-operator-state-entry-hook
                     :exit-hook 'evil-operator-state-exit-hook))

(defun rigpa--enter-lower-or-pass-through ()
  "Enter a lower level, or pass through to underlying keymap."
  (interactive)
  (if buffer-read-only
      ;; ideally should find a way to "pass through"
      ;; to lower-priority keymaps instead of specifying
      ;; what should happen on a per-mode basis here
      (cond ((eq major-mode 'dired-mode)
             (dired-find-file))
            ((eq major-mode 'help-mode)
             (push-button))
            ((eq major-mode 'debugger-mode)
             (push-button))
            ((eq major-mode 'ert-results-mode)
             (push-button))
            ((eq major-mode 'occur-mode)
             (occur-mode-goto-occurrence))
            ((eq major-mode 'grep-mode)
             (compile-goto-error))
            ((eq major-mode 'ivy-occur-grep-mode)
             (ivy-occur-press-and-switch))
            ((eq major-mode 'dictionary-mode)
             (link-selected))
            ((eq major-mode 'ripgrep-search-mode)
             (compile-goto-error))
            ((eq major-mode 'magit-status-mode)
             (magit-diff-visit-file (magit-current-file) t))
            ((eq major-mode 'magit-log-mode)
             (magit-show-commit (magit-thingatpt--git-revision)))
            ((eq major-mode 'Info-mode)
             (Info-follow-nearest-node))
            ((eq major-mode 'xref--xref-buffer-mode)
             (xref-goto-xref))
            ((eq major-mode 'ibuffer-mode)
             (ibuffer-visit-buffer))
            (t (rigpa-enter-lower-level)))
    (cond ((eq major-mode 'Custom-mode)
           (Custom-newline (point)))
          (t (rigpa-enter-lower-level)))))

(defun rigpa--integrate-evil-states ()
  "Map standard evil state entry and exit points so they're managed by rigpa."
  ;; evil interop keybindings
  ;; TODO: these (Esc/Ret) should be dependent on whether there are any
  ;; other modes in the tower. If not, then this shouldn't be bound
  ;; IOW this keybinding (and some class of bindings more generally)
  ;; is tower-specific
  (dolist (state chimera-evil-states)
    (let ((keymap (symbol-value
                   (intern
                    (concat "evil-" state "-state-map")))))
      (define-key keymap [escape] #'rigpa-enter-higher-level)
      (unless (member state chimera-insertion-states)
        (define-key keymap [return] #'rigpa--enter-lower-or-pass-through))))
  ;; exit visual state gracefully
  (define-key evil-visual-state-map [escape] (lambda ()
                                               (interactive)
                                               (evil-exit-visual-state)
                                               (rigpa-enter-higher-level)))
  (define-key evil-visual-state-map [return] (lambda ()
                                               (interactive)
                                               (evil-exit-visual-state)
                                               (rigpa-enter-lower-level)))
  ;; interrupting operator state should unconditionally "escape"
  ;; but by default an operator enters insert state as a follow-on.
  ;; we use the default normal override binding here to avoid this.
  ;; This will bypass any rigpa-specific behavior, but as it seems
  ;; unlikely that we'd want to incorporate operator state formally
  ;; as part of any structures, this seems a reasonable hack
  (define-key evil-operator-state-map [escape] #'evil-force-normal-state))

(defun rigpa--register-modes ()
  "Register the standard modes with the framework."
  ;; register evil chimera states with the rigpa framework
  (rigpa-register-mode chimera-normal-mode)
  (rigpa-register-mode chimera-insert-mode)
  (rigpa-register-mode chimera-emacs-mode)
  (rigpa-register-mode chimera-visual-mode)
  (rigpa-register-mode chimera-replace-mode)
  (rigpa-register-mode chimera-operator-mode)

  ;; register all the other modes
  (rigpa-register-mode chimera-application-mode)
  (rigpa-register-mode chimera-line-mode)
  (rigpa-register-mode chimera-view-mode)
  (rigpa-register-mode chimera-activity-mode)
  (rigpa-register-mode chimera-history-mode)
  (rigpa-register-mode chimera-tab-mode)
  (rigpa-register-mode chimera-word-mode)
  (rigpa-register-mode chimera-window-mode)
  (rigpa-register-mode chimera-char-mode)
  (rigpa-register-mode chimera-system-mode)
  (rigpa-register-mode chimera-buffer-mode)
  (rigpa-register-mode chimera-file-mode)
  (rigpa-register-mode chimera-text-mode)
  (rigpa-register-mode chimera-symex-mode))

(defun rigpa--create-editing-structures ()
  "Create standard editing structures."

  ;; towers in base editing levels
  (setq rigpa-complete-tower
        (make-editing-ensemble :name "complete"
                               :default "normal"
                               :members (list chimera-insert-mode
                                              chimera-char-mode
                                              chimera-word-mode
                                              chimera-line-mode
                                              chimera-activity-mode
                                              chimera-normal-mode
                                              chimera-view-mode
                                              chimera-window-mode
                                              chimera-file-mode
                                              chimera-buffer-mode
                                              chimera-system-mode
                                              chimera-application-mode)))

  (setq rigpa-vim-tower
        (make-editing-ensemble :name "vim"
                               :default "normal"
                               :members (list chimera-insert-mode
                                              chimera-normal-mode)))
  (setq rigpa-emacs-tower
        (make-editing-ensemble :name "emacs"
                               :default "emacs"
                               :members (list chimera-emacs-mode)))
  (setq rigpa-lisp-tower
        (make-editing-ensemble :name "lisp"
                               :default "symex"
                               :members (list chimera-insert-mode
                                              chimera-symex-mode
                                              chimera-normal-mode)))

  ;; complexes for base editing levels
  (setq rigpa-general-complex
        (make-editing-ensemble
         :name "general"
         :default "vim"
         :members (list rigpa-vim-tower
                        rigpa-complete-tower
                        rigpa-lisp-tower
                        rigpa-emacs-tower)))

  ;; towers and complexes for meta levels
  (setq rigpa-meta-mode-tower
        (make-editing-ensemble :name "meta-mode"
                               :default "line"
                               :members (list chimera-line-mode)))
  (setq rigpa-meta-complex
        (make-editing-ensemble
         :name "meta"
         :default "meta-mode"
         :members (list rigpa-meta-mode-tower)))

  (setq rigpa-meta-tower-mode-tower
        (make-editing-ensemble :name "meta-tower"
                               :default "buffer"
                               :members (list chimera-buffer-mode)))
  (setq rigpa-meta-tower-complex
        (make-editing-ensemble
         :name "meta-tower"
         :default "meta-tower"
         :members (list rigpa-meta-tower-mode-tower))))

(defun rigpa--provide-editing-structures ()
  "Register editing structures so they're used in relevant major modes."
  ;; change these to use a state struct instead of globals
  ;; that state struct must specify
  ;;  (1) the complex, as an object (this should take care of default tower
  ;;      so there's no need to set the tower index initially)
  ;;  (2) the tower index
  ;;  (3) the level index
  ;; and eventually make these "coordinates" generic
  (when (boundp 'symex-mode)
    (dolist (mode-name symex-lisp-modes)
      (let ((mode-hook (intern (concat (symbol-name mode-name)
                                       "-hook"))))
        (add-hook mode-hook (lambda ()
                              (setq rigpa--current-tower-index 2)
                              (setq rigpa--current-level 2))))))
  (add-hook 'rigpa-meta-mode-hook
            ;; TODO: dispatch here based on meta level. If the level
            ;; is 1 use line mode / buffers, if it's 2,
            ;; use buffer mode / perspectives?
            (lambda ()
              (setq rigpa--complex rigpa-meta-complex)))
  (add-hook 'rigpa-meta-tower-mode-hook
            ;; TODO: dispatch here based on meta level. If the level
            ;; is 1 use line mode / buffers, if it's 2,
            ;; use buffer mode / perspectives?
            (lambda ()
              (setq rigpa--complex rigpa-meta-tower-complex))))

(defun rigpa-initialize ()
  "Initialize rigpa."
  (interactive)
  (rigpa--register-modes)
  ;; should make this optional via a defcustom flag
  ;; or potentially even have it in a separate evil-adapter package
  (when (boundp 'evil-mode)
    (rigpa--integrate-evil-states))
  (rigpa--create-editing-structures)
  (rigpa--provide-editing-structures)
  (if (and (boundp 'rigpa-show-menus) rigpa-show-menus)
      (dolist (mode (ht-values rigpa-modes))
        (rigpa-show-menu (chimera-mode-name mode)))
    (dolist (mode (ht-values rigpa-modes))
      (rigpa-hide-menu (chimera-mode-name mode)))))

(defun rigpa-disable ()
  "Disable rigpa."
  (interactive)
  ;; TODO:
  ;; remap evil keybindings
  ;; unregister major mode hooks
  ;; unregister all modes (including esp evil states)
  nil)


(rigpa-initialize)

;; the editing complex to use in a buffer
;; by default this is general complex unless a more tailored one
;; has been set (e.g. via major mode hook)
(defvar-local rigpa--complex rigpa-general-complex)


(provide 'rigpa)
;;; rigpa.el ends here
