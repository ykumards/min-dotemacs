;;; Emacs Bedrock
;;;
;;; Extra config: Vim emulation

;;; Usage: Append or require this file from init.el for bindings in Emacs.

;;; Contents:
;;;
;;;  - Core Packages

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Core Packages
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Evil: vi emulation
(use-package evil
  :ensure t

  :init
  (setq evil-respect-visual-line-mode t)
  (setq evil-undo-system 'undo-redo)

  ;; Enable this if you want C-u to scroll up, more like pure Vim
  ;(setq evil-want-C-u-scroll t)

  :config
  (evil-mode 1)

  ;; Configuring initial major mode for some modes
  (evil-set-initial-state 'vterm-mode 'emacs)
  ;; Set cursor styles for different states
  (setq evil-normal-state-cursor '(box "#FF0000")    ; Block cursor with red color
        evil-insert-state-cursor '(bar "#00FF00")    ; Bar cursor with green color
        evil-visual-state-cursor '(hollow "#0000FF") ; Hollow cursor with blue color
        evil-replace-state-cursor '(hbar "#FFFF00")  ; Horizontal bar cursor with yellow color
        evil-motion-state-cursor '(box "#FF00FF")    ; Block cursor with magenta color
        evil-operator-state-cursor '(box "#00FFFF")) ; Block cursor with cyan color
  (setq evil-esc-delay 0.0001)
)
(unless (display-graphic-p)
  (add-hook 'evil-normal-state-entry-hook (lambda () (send-string-to-terminal "\e[2 q")))  ; Block cursor
  (add-hook 'evil-insert-state-entry-hook (lambda () (send-string-to-terminal "\e[6 q")))  ; Vertical bar cursor
  (add-hook 'evil-visual-state-entry-hook (lambda () (send-string-to-terminal "\e[2 q")))  ; Block cursor
  (add-hook 'evil-replace-state-entry-hook (lambda () (send-string-to-terminal "\e[4 q"))) ; Underline cursor
  (add-hook 'evil-motion-state-entry-hook (lambda () (send-string-to-terminal "\e[2 q")))  ; Block cursor
  (add-hook 'evil-operator-state-entry-hook (lambda () (send-string-to-terminal "\e[2 q"))) ; Block cursor
)
