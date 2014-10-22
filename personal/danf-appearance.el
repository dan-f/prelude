(scroll-bar-mode -1)
(set-frame-font "Source Code Pro 13")

(prelude-require-packages '(cyberpunk-theme
                            flatland-theme
                            flatui-theme
                            grandshell-theme
                            gruvbox-theme))

(setq custom-safe-themes 't)
(disable-theme 'zenburn)
(load-theme 'cyberpunk)

(provide 'danf-appearance)
