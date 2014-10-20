;; annoyances
(when (display-graphic-p) ; much better GUI scrolling
  (progn
    ;; (setq mouse-wheel-progressive-speed t)
    (setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))))


;; keybindings
(global-set-key (kbd "C-c ;") 'dan-f/comment-or-uncomment-line-or-region)

(provide 'danf-global)
