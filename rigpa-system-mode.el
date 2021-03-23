(require 'chimera)
(require 'chimera-hydra)

(evil-define-state system
  "System state."
  :tag " <S> "
  :message "-- SYSTEM --"
  :enable (normal))

(defun rigpa-system-battery-life ()
  "Show power info including battery life
   (Mac-specific, at the moment)."
  (interactive)
  (display-message-or-buffer (shell-command-to-string "pmset -g batt")))

(defhydra hydra-system (:exit t
                        :body-pre (chimera-hydra-signal-entry chimera-system-mode)
                        :post (chimera-hydra-portend-exit chimera-system-mode t)
                        :after-exit (chimera-hydra-signal-exit chimera-system-mode
                                                               #'chimera-handle-hydra-exit))
  "System information"
  ("b" rigpa-system-battery-life "show power info including battery life")
  ("s-i" rigpa-system-battery-life "show power info including battery life")
  ("H-m" rigpa-toggle-menu "show/hide this menu" :exit nil)
  ("<return>" rigpa-enter-lower-level "enter lower level" :exit t)
  ("<escape>" rigpa-enter-higher-level "escape to higher level" :exit t))

(defvar chimera-system-mode-entry-hook nil
  "Entry hook for rigpa system mode.")

(defvar chimera-system-mode-exit-hook nil
  "Exit hook for rigpa system mode.")

(defvar chimera-system-mode
  (make-chimera-mode :name "system"
                     :enter #'hydra-system/body
                     :pre-entry-hook 'chimera-system-mode-entry-hook
                     :post-exit-hook 'chimera-system-mode-exit-hook
                     :entry-hook 'evil-system-state-entry-hook
                     :exit-hook 'evil-system-state-exit-hook))


(provide 'rigpa-system-mode)
