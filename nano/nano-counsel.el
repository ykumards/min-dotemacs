;; ---------------------------------------------------------------------
;; GNU Emacs / N Λ N O - Emacs made simple
;; Copyright (C) 2020 - N Λ N O developers
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;; ---------------------------------------------------------------------

;; We have a local copy of smex (that has not changed since 2014)
(use-package smex
  :straight t
  :config
  (smex-initialize))  ; Ensure smex is initialized

(use-package counsel
  :straight t
  :bind (("M-x" . counsel-M-x)
         ("C-c r" . counsel-recentf)
         ("C-c b" . counsel-bookmark)
         ("C-x C-b" . counsel-switch-buffer)
         ("C-c c" . counsel-org-capture))
  :init
  (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t
        enable-recursive-minibuffers t
        ivy-height 8
        ivy-count-format "")
  (setq ivy-initial-inputs-alist '((counsel-minor . "^+")
                                   (counsel-package . "^+")
                                   (counsel-org-capture . "^")
                                   (counsel-M-x . "^")
                                   (counsel-describe-symbol . "^")
                                   (org-refile . "")
                                   (org-agenda-refile . "")
                                   (org-capture-refile . "")
                                   (Man-completion-table . "^")
                                   (woman . "^")))
  (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)  ; Default fuzzy matching globally
                                (counsel-M-x . ivy--regex-fuzzy))))  ; Ensure fuzzy matching for `counsel-M-x`

(setq ivy-re-builders-alist
      '((t . ivy--regex-fuzzy)))  ; use fuzzy matching everywhere

(use-package counsel-projectile
  :straight t
  :config
  (counsel-projectile-mode))

(use-package prescient
  :straight t
  :config
  (prescient-persist-mode 1))

(use-package ivy-prescient
  :after ivy
  :straight t
  :config
  (ivy-prescient-mode 1)
  ;; Optionally, configure the sorting algorithm more aggressively:
  (setq ivy-prescient-retain-classic-highlighting t)
  (setq prescient-sort-length-enable nil))


(global-set-key (kbd "M-x")     'counsel-M-x)


(global-set-key (kbd "C-c r")   'counsel-recentf)
(global-set-key (kbd "C-c b")   'counsel-bookmark)
(global-set-key (kbd "C-x C-b") 'counsel-switch-buffer)
(global-set-key (kbd "C-c c")   'counsel-org-capture)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
