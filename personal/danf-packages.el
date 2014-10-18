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

(provide 'danf-packages)
