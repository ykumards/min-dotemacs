;; Load nano things
(require 'nano-splash)

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
