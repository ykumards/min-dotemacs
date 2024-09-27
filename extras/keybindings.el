;; Global keybinding for Neotree toggle
(global-set-key [f8] 'treemacs)

(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key (kbd "C-c C-n") 'dired-sidebar-toggle-sidebar)
(global-set-key (kbd "C-x RET") 'execute-extended-command)


;; MacOS editing behavior
(evil-define-key 'insert 'global
  (kbd "C-<backspace>") 'evil-delete-backward-line      ;; Cmd + Delete: Delete to start of line
  (kbd "C-S-<backspace>") 'kill-line                    ;; Cmd + Shift + Delete: Delete to end of line
  (kbd "M-<backspace>") 'backward-kill-word             ;; Option + Delete: Kill word backward
  (kbd "C-<backspace>") 'kill-whole-line                ;; Cmd + Backspace: Kill entire line
  (kbd "C-<left>") 'move-beginning-of-line              ;; Cmd + Left: Move to start of line
  (kbd "C-<right>") 'move-end-of-line)                 ;; Cmd + Right: Move to end of line

(global-set-key (kbd "C-a") 'mark-whole-buffer)         ;; Cmd + A: Select all

;; Move line up/down
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
    "pt"  '(treemacs :which-key "Toggle Treemacs")
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

    ;; Avy jumps
    "j"  '(:ignore t :which-key "jump")  ;; Group for Avy-related commands
   "jc" '(avy-goto-char :which-key "Go to char")
   "jw" '(avy-goto-word-1 :which-key "Go to word")
   "jl" '(avy-goto-line :which-key "Go to line")
   "js" '(avy-goto-char-timer :which-key "Go to char sequence")

    ;; journal
    "l" '(:ignore t :which-key "journal")
    "lc" '(calendar :which-key "Open calendar")
    "lj" '(org-journal-new-entry :which-key "New journal entry")
    "ls" '(org-journal-search :which-key "Next entry")
    "lp" '(org-journal-previous-entry :which-key "Next entry")
    "lo" '(org-journal-open-current-journal-file :which-key "Open current entry")

    "w"  '(:ignore t :which-key "windows")
    "wl" '(windmove-right :which-key "Move right")
    "wh" '(windmove-left :which-key "Move left")
    "wk" '(windmove-up :which-key "Move up")
    "wj" '(windmove-down :which-key "Move down")
    "wo" '(ace-window :which-key "Ace window")
)

;; Define alternative leader keys in eshell and deft
(my-alt-leader-keys
  "SPC" '(execute-extended-command :which-key "M-x")
  "f"   '(find-file :which-key "Find file")
  "b"   '(switch-to-buffer :which-key "Switch buffer")
  "t"   '(eshell :which-key "Open Eshell")  ; Useful in deft
  "n"   '(deft :which-key "Open Deft"))    ; Useful in eshell
