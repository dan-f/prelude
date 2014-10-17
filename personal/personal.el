;; mac things
(setq mac-option-modifier 'super)


;; packages
(prelude-require-package 'highlight-symbol)
(dolist (hook '(prog-mode-hook html-mode-hook css-mode-hook))
  (add-hook hook 'highlight-symbol-mode)
  (add-hook hook 'highlight-symbol-nav-mode))
(eval-after-load 'highlight-symbol
  '(diminish 'highlight-symbol-mode))

(prelude-require-package 'ido-vertical-mode)
(ido-vertical-mode)

(prelude-require-package 'js2-mode)

(prelude-require-package 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(prelude-require-package 'ggtags)

(prelude-require-packages '(perspective persp-projectile))
(persp-mode)
(require 'persp-projectile)

;; appearance
(prelude-require-packages '(flatland-theme
                            flatui-theme
                            grandshell-theme
                            gruvbox-theme
                            ))

(scroll-bar-mode -1)
(set-frame-font "Source Code Pro 13")

(setq custom-safe-themes 't)
(disable-theme 'zenburn)

;; (invert-face 'default)
;; (set-cursor-color "yellow")


;; annoyances
(when (display-graphic-p) ; much better GUI scrolling
  (progn
    ;; (setq mouse-wheel-progressive-speed t)
    (setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))))

(delete* 'lines-tail whitespace-style) ; don't highlight lines longer than whitespace-line-column


;; keybindings
(global-set-key (kbd "C-c ;") 'dan-f/comment-or-uncomment-line-or-region)
