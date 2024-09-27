;;; Emacs Bedrock
;;;
;;; Extra config: Researcher

;;; Usage: Append or require this file from init.el for research helps. If you
;;; write papers in LaTeX and need to manage your citations or keep track of
;;; notes, this set of packages is for you.
;;;
;;; Denote is a simpler alternative to Org-roam: instead of maintaining a
;;; database along side your files, Denote works by enforcing a particular file
;;; naming strategy. This makes it easy to link and tag notes, much in the same
;;; way that Org-roam does. It's generally advisable to not mix Org-roam and
;;; Denote in the same directory.
;;;
;;; NOTE: the packages Citar and Org-roam live on the MELPA repository; you will
;;; need to update the `package-archives' variable in init.el before before
;;; loading this; see the comment in init.el under "Package initialization".
;;;
;;; Highly recommended to enable this file with the UI enhancements in
;;; `base.el', as Citar works best with the Vertico completing-read interface.
;;; Also recommended is the `writer.el' extra config, which adds some nice features for
;;; spell-checking etc.

;;; Contents:
;;;
;;;  - Citation Management
;;;  - Authoring
;;;  - Note Taking: Org-roam
;;;  - Note Taking: Denote

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Critical variables
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; These variables must be set for Citar to work properly!

(setopt citar-bibliography '("~/Documents/org/refs.bib")) ; paths to your bibtex files

;;; These variables are needed for Denote
(setopt denote-directory (expand-file-name "~/Documents/org/denote-notes/"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Citation Management
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package citar
  :ensure t
  :bind (("C-c b" . citar-insert-citation)
         :map minibuffer-local-map
         ("M-b" . citar-insert-preset))
  :custom
  ;; Allows you to customize what citar-open does
  (citar-file-open-functions '(("html" . citar-file-open-external)
                               ;; ("pdf" . citar-file-open-external)
                               (t . find-file))))

;; Optional: if you have the embark package installed, enable the ability to act
;; on citations with Citar by invoking `embark-act'.
;(use-package citar-embark
;  :after citar embark
;  :diminish ""
;  :no-require
;  :config (citar-embark-mode))
