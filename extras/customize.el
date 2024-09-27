;; Load nano things
(require 'nano-splash)

;; Set default font family and size
(set-face-attribute 'default nil
                    :family "JetBrainsMono Nerd Font"  ;; Replace with your preferred font
                    :height 140          ;; Font size in 1/10 pt, 120 means 12pt
                    :weight 'light      ;; You can also use 'bold' if you want bold
                    :width 'normal)      ;; Normal width, can be 'condensed' or 'expanded'

;; Install Doom Themes
(use-package doom-themes
  :straight t  ;; Or :ensure t if using package.el
  :config
  ;; Load the doom-Iosvkem theme
  (load-theme 'doom-dracula t)

  ;; Optional: Enable bold and italic in the theme
  (setq doom-themes-enable-bold t    ;; if nil, bold is disabled across all themes
        doom-themes-enable-italic t) ;; if nil, italics is disabled across all themes

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Corrects (and improves) org-mode's native fontification
  (doom-themes-org-config))

;;;;; SMOOTH SCROLLING ;;;;;;;;;
;; Enable pixel-by-pixel smooth scrolling (Emacs 29+)
(pixel-scroll-precision-mode)

;; Smoother scrolling behavior
(setq scroll-margin 2
      scroll-conservatively 10000
      scroll-preserve-screen-position t
      auto-window-vscroll nil)

;; Smooth and responsive mouse wheel scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      mouse-wheel-follow-mouse 't
      scroll-step 1)

;; Use smooth-scrolling package for enhanced smoothness (optional)
(use-package smooth-scrolling
  :ensure t
  :config
  (smooth-scrolling-mode 1)
  (setq smooth-scroll-margin 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Markdown Mode Configuration
(use-package markdown-mode
  :straight t
  :mode ("\\.md\\'" . markdown-mode)
  :init
  (setq markdown-command "pandoc")  ;; or multimarkdown if installed
  :config
  (setq markdown-enable-math t))  ;; Enable math support

;; Org Calendar settings
(evil-set-initial-state 'calendar-mode 'normal)

(with-eval-after-load 'calendar
  (evil-define-key 'normal calendar-mode-map
    "j" 'calendar-forward-week              ;; Move forward by one week
    "k" 'calendar-backward-week             ;; Move backward by one week
    "h" 'calendar-backward-day              ;; Move back one day
    "l" 'calendar-forward-day               ;; Move forward one day
    "H" 'calendar-beginning-of-week         ;; Move to the beginning of the week
    "L" 'calendar-end-of-week               ;; Move to the end of the week
    "M" 'calendar-beginning-of-month        ;; Move to the beginning of the month
    "y" 'calendar-cursor-to-nearest-date    ;; Jump to the nearest date
    "n" 'calendar-forward-month             ;; Move forward by one month
    "p" 'calendar-backward-month            ;; Move backward by one month
    "f" 'calendar-forward-year              ;; Move forward by one year
    "b" 'calendar-backward-year             ;; Move backward by one year
    "q" 'calendar-exit                      ;; Exit the calendar
    ))


