(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key (kbd "C-c C-n") 'dired-sidebar-toggle-sidebar)
(global-set-key (kbd "C-x RET") 'execute-extended-command)
(defun move-line-up ()
  "Move the current line up."
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun move-line-down ()
  "Move the current line down."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

(global-set-key (kbd "<M-up>") 'move-line-up)
(global-set-key (kbd "<M-down>") 'move-line-down)

(use-package general
  :straight t
  ;; PERF: Loading `general' early make Emacs very slow on startup.
  :after evil
  :demand t
  :config
  ;; Advise `define-key' to automatically unbind keys when necessary.
  (general-auto-unbind-keys)
  ;; Set up some basic equivalents (like `general-nmap') with short named
  ;; aliases (like `nmap') for VIM mapping functions.
  (general-evil-setup t))

;; Global leader
(general-create-definer my-leader-keys
  :states '(normal visual motion)
  :keymaps 'override
  :prefix "SPC")

;; Alternative leader for specific modes where SPC should act as space
(general-create-definer my-alt-leader-keys
  :states 'insert
  :keymaps '(eshell-mode-map deft-mode-map vterm-mode-map)
  :prefix "C-SPC")

(my-leader-keys
   "SPC"  '(execute-extended-command :which-key "M-x")
    ">"    '(next-buffer :which-key "Next buffer")
    "<"    '(previous-buffer :which-key "Previous buffer")
    ";"    '(eval-expression :which-key "Eval expression")
    ":"    '(project-find-file :which-key "Find file in project")
    "X"    '(org-capture :which-key "Org Capture")
    "u"    '(universal-argument :which-key "Universal Argument")
    "C"    '(universal-coding-system-argument :which-key "Set Coding System")
    "O"    '(other-window :which-key "Other window")

    ;; Applications (Open)
    "o"    '(nil :which-key "open")
    "o-"   '(dired :which-key "Open Dired")
    "oa"   '(org-agenda :which-key "Org Agenda")
    "oe"   '(eshell :which-key "Open Eshell")
    "o="   '(calc :which-key "Open Calculator")

    ;; Search (similar to Cmd + Shift + F in VS Code)
    "s"    '(nil :which-key "search")
    "ss"   '(counsel-rg :which-key "Ripgrep Search")  ;; or use `projectile-ripgrep` if using Projectile

    ;; Notes
    "n" '(:ignore t :which-key "notes")
    "nn" '(org-capture :which-key "new note")
    "nd" '(deft :which-key "deft")

    ;; File
    "f"    '(:ignore t :which-key "file")
    "fS"   '(write-file :which-key "Save as ...")
    "fd"   #'+delete-this-file
    "fD"   #'+delete-this-file-and-buffer
    "ff"   #'counsel-projectile-find-file
    "fs"   #'save-buffer
    "ft"   #'recover-this-file
    "fT"   #'recover-file
    "fy"   #'+yank-this-file-name

    ;; ====== Project ======
    "p"   '(:ignore t :which-key "project")
    "pp"  #'project-switch-project
    "pc"  #'project-compile
    "pd"  #'project-find-dir
    "pf"  #'project-find-file
    "pk"  #'project-kill-buffers
    "pb"  #'project-switch-to-buffer
    "pa"  #'+project-add-project
    "pD"  #'+dir-locals-open-or-create
    "p-"  #'project-dired
    "px"  #'project-execute-extended-command
    ;; compile/test
    "pc" #'project-compile
    ;; run
    "pr"  '(nil :wk "run")
    "pre" #'project-eshell
    "prg" #'+project-gdb
    "prs" #'project-shell
    "prc" #'project-shell-command
    "prC" #'project-async-shell-command
    ;; forget
    "pF"  '(nil :wk "forget/cleanup")
    "pFz" #'+project-forget-zombie-projects
    "pFp" #'project-forget-project
    "pFu" #'project-forget-projects-under
    "pFc" #'+project-list-cleanup
    ;; search/replace
    "ps"  '(nil :wk "search/replace")
    "pss" #'project-search
    "psn" '(fileloop-continue :wk "Next match")
    "psr" #'project-query-replace-regexp
    "psf" #'project-find-regexp

    ;; Toggle
    "t" '(:ignore t :which-key "toggle")
    "tz" '(dired-sidebar-toggle-sidebar :which-key "Toggle Dired Sidebar")

    ;; journal
    "j" '(:ignore t :which-key "journal")
    "jc" '(calendar :which-key "Open calendar")
    "jj" '(org-journal-new-entry :which-key "New journal entry")
    "js" '(org-journal-search :which-key "Next entry")
    "jp" '(org-journal-previous-entry :which-key "Next entry")
    "jo" '(org-journal-open-current-journal-file :which-key "Open current entry")
)

;; Define alternative leader keys in eshell and deft
(my-alt-leader-keys
  "SPC" '(execute-extended-command :which-key "M-x")
  "f"   '(find-file :which-key "Find file")
  "b"   '(switch-to-buffer :which-key "Switch buffer")
  "t"   '(eshell :which-key "Open Eshell")  ; Useful in deft
  "n"   '(deft :which-key "Open Deft"))    ; Useful in eshell
