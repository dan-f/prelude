(scroll-bar-mode -1)
(set-frame-font "Source Code Pro 13")

(prelude-require-packages '(flatland-theme
                            flatui-theme
                            grandshell-theme
                            gruvbox-theme))

(setq custom-safe-themes 't)
(disable-theme 'zenburn)

(provide 'danf-appearance)
