;; Load nano things
(require 'nano-splash)

;; Set default font family and size
(set-face-attribute 'default nil
                    :family "JetBrainsMono Nerd Font"  ;; Replace with your preferred font
                    :height 130          ;; Font size in 1/10 pt, 120 means 12pt
                    :weight 'light      ;; You can also use 'bold' if you want bold
                    :width 'normal)      ;; Normal width, can be 'condensed' or 'expanded'

;; Install Doom Themes
(use-package doom-themes
  :straight t  ;; Or :ensure t if using package.el
  :config
  ;; Load the doom-Iosvkem theme
  (load-theme 'doom-Iosvkem t)

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
  :init (setq markdown-command "multimarkdown")
  :config
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-enable-wiki-links t))

;; Optional: Add table of contents functionality
(use-package markdown-toc
  :straight t
  :hook (markdown-mode . markdown-toc-mode))

;; Syntax checking for markdown
(use-package flycheck
  :straight t
  :hook (markdown-mode . flycheck-mode))

;; Enable orgtbl-mode for better tables
(add-hook 'markdown-mode-hook 'orgtbl-mode)

;; Enable visual-line-mode for better line wrapping
(add-hook 'markdown-mode-hook 'visual-line-mode)
