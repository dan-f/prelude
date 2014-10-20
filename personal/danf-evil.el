(require 'evil)

;;
;; Basic Settings
;;

(if (display-graphic-p)
    (evil-set-toggle-key "s-SPC")
  (evil-set-toggle-key "M-SPC"))

(setq evil-default-cursor t)
(setq evil-emacs-state-cursor  '(nil box))
(setq evil-normal-state-cursor '(nil box))
(setq evil-visual-state-cursor '(nil box))
(setq evil-insert-state-cursor '(nil bar))
(setq evil-motion-state-cursor '(nil box))

;; makes evil-emacs-state modes open up in motion state
;; (setq evil-motion-state-modes (append evil-emacs-state-modes evil-motion-state-modes))
;; (setq evil-emacs-state-modes nil)

;;
;; Global Remaps
;;

(define-key evil-normal-state-map (kbd "M-H") #'help)
(define-key evil-normal-state-map (kbd "Y") (kbd "y$"))

;;; Remove useless vim bindings
(define-key evil-insert-state-map (kbd "C-k") nil)
(define-key evil-motion-state-map (kbd "RET") nil) ;; This somehow messes up RET in normal state, so...:
(define-key evil-normal-state-map (kbd "RET") 'evil-ret)
(define-key evil-motion-state-map (kbd "TAB") nil)
(define-key evil-motion-state-map (kbd "SPC") nil)

;; Maps "kj" to escape
;; from http://zuttobenkyou.wordpress.com/2011/02/15/some-thoughts-on-emacs-and-vim/
(evil-define-key 'insert global-map "k" #'cofi/maybe-exit)


;; window mappings
(define-key evil-normal-state-map (kbd "C-h") #'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") #'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") #'evil-window-up)
(define-key evil-normal-state-map (kbd "C-l") #'evil-window-right)
(define-key evil-motion-state-map (kbd "C-h") #'evil-window-left)
(define-key evil-motion-state-map (kbd "C-j") #'evil-window-down)
(define-key evil-motion-state-map (kbd "C-k") #'evil-window-up)
(define-key evil-motion-state-map (kbd "C-l") #'evil-window-right)

;; Navigation and Editing
(define-key evil-motion-state-map (kbd "C-u") 'evil-scroll-up)
;; (define-key evil-motion-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-insert-state-map (kbd "C-e") 'move-end-of-line)
(define-key evil-visual-state-map (kbd "TAB") #'evil-indent)

(define-key evil-normal-state-map (kbd "H") 'evil-first-non-blank)
(define-key evil-normal-state-map (kbd "L") 'evil-end-of-line)
(define-key evil-visual-state-map (kbd "H") 'evil-first-non-blank)
(define-key evil-visual-state-map (kbd "L") 'evil-end-of-line)
(define-key evil-motion-state-map (kbd "H") 'evil-first-non-blank)
(define-key evil-motion-state-map (kbd "L") 'evil-end-of-line)

(define-key evil-normal-state-map (kbd "TAB") 'evil-jump-forward)
(define-key evil-visual-state-map (kbd "TAB") 'evil-jump-forward)

;;; ace-jump
(define-key evil-normal-state-map (kbd "SPC")   'evil-ace-jump-char-mode)
(define-key evil-normal-state-map (kbd "S-SPC") 'evil-ace-jump-word-mode)
(define-key evil-normal-state-map (kbd "C-SPC") 'evil-ace-jump-line-mode)
(define-key evil-operator-state-map (kbd "SPC")   'evil-ace-jump-char-mode)
(define-key evil-operator-state-map (kbd "S-SPC") 'evil-ace-jump-word-mode)
(define-key evil-operator-state-map (kbd "C-SPC") 'evil-ace-jump-line-mode)
(define-key evil-visual-state-map (kbd "SPC")   'evil-ace-jump-char-mode)
(define-key evil-visual-state-map (kbd "S-SPC") 'evil-ace-jump-word-mode)
(define-key evil-visual-state-map (kbd "C-SPC") 'evil-ace-jump-line-mode)

;;; Operators, commands

(evil-define-command cofi/maybe-exit ()
  :repeat change
  (interactive)
  (let ((modified (buffer-modified-p)))
    (insert "k")
    (let ((evt (read-event (format "Insert %c to exit insert state" ?j)
                           nil 0.5)))
      (cond
       ((null evt) (message ""))
       ((and (integerp evt) (char-equal evt ?j))
        (delete-char -1)
        (set-buffer-modified-p modified)
        (push 'escape unread-command-events))
       (t (setq unread-command-events (append unread-command-events
                                              (list evt))))))))

(evil-define-operator evil-comment (beg end type register yank-handler)
  "Toggle comment on text from BEG to END with TYPE.
Save pre-commented text in REGISTER or in the kill-ring with YANK-HANDLER.
Copied from evil-delete implementation."
  (interactive "<R><x><y>")
  (unless register
    (let ((text (filter-buffer-substring beg end)))
      (unless (string-match-p "\n" text)
        ;; set the small delete register
        (evil-set-register ?- text))))
  (evil-yank beg end type register yank-handler)
  (cond
   ((eq type 'block)
    (evil-apply-on-block #'comment-or-uncomment-region beg end nil))
   ((and (eq type 'line)
         (= end (point-max))
         (or (= beg end)
             (/= (char-before end) ?\n))
         (/= beg (point-min))
         (=  (char-before beg) ?\n))
    (comment-or-uncomment-region (1- beg) end))
   (t
    (comment-or-uncomment-region beg end)))
  (when (and (evil-called-interactively-p)
             (eq type 'line))
    (evil-first-non-blank)))


;;
;; Advice
;;

(defadvice comment-dwim (after evil-comment-dwim activate)
  (when (and (not mark-active)
             evil-mode
             (not (eq evil-state 'emacs)))
    (evil-insert 1)))

;;
;; Mode-specific Settings
;;

;; Some modes clash too much with Evil
;;
;; Taken (and modified) shamelessly from cofi's evil config
;; ( https://github.com/cofi/dotfiles/tree/master/emacs.d/config )
(cl-loop for (mode . state) in '((inferior-emacs-lisp-mode  . emacs)
                                 (term-mode                 . emacs)
                                 (bc-menu-mode              . emacs)
                                 (gud-mode                  . emacs))
         do (evil-set-initial-state mode state))

;;; emacs lisp
(evil-define-key 'normal emacs-lisp-mode-map
  "K" '(lambda ()
         (interactive)
         (describe-function (symbol-at-point))))

;; make magit evil
(evil-define-key 'emacs magit-mode-map
  "h" 'magit-toggle-diff-refine-hunk
  "j" 'magit-goto-next-section
  "k" 'magit-goto-previous-section
  "l" 'magit-key-mode-popup-logging
  "K" 'magit-discard-item
  "\C-h" 'evil-window-left
  "\C-j" 'evil-window-down
  "\C-k" 'evil-window-up
  "\C-l" 'evil-window-right
  )


(evil-define-key 'normal comint-mode-map "C-d" 'evil-scroll-down)
(evil-define-key 'insert comint-mode-map "C-j" 'comint-send-input)

;; use j/k as n/p in grep/ag mode
(evil-define-key 'normal grep-mode-map
  "j" 'next-error-no-select
  "k" 'previous-error-no-select
  "q" 'quit-window)

(evil-define-key 'normal ag-mode-map
  "j" 'next-error-no-select
  "k" 'previous-error-no-select
  "q" 'quit-window)


;;
;; Plugin Settings
;;

;; evil-leader
(prelude-require-package 'evil-leader)
(global-evil-leader-mode 1)

(evil-leader/set-leader ",")
(setq evil-leader/in-all-states nil)      ; in emacs state, use C-, as <leader>

(evil-leader/set-key
  "i" (lambda () (interactive) (find-file "~/.emacs.d/init.el"))
  "a" 'ag-project-regexp
  "x" ctl-x-map
  "f" 'helm-find-files
  "d" 'ido-dired
  "b" 'helm-mini
  "l" 'load-file
  "c" 'evil-comment
  "w" 'save-buffer

  "t" 'split-ansi-term
  "s" 'split-eshell

  "pa" 'projectile-ag
  "ph" 'helm-projectile
  "pb" 'helm-projectile-switch-to-buffer
  "pf" 'helm-projectile-find-file
  "pp" 'helm-projectile-switch-project

  "g" 'magit-status
  "q" 'evil-quit
  "o" 'helm-occur
  "m" 'compile
  "k" 'kill-this-buffer
  "K" 'kill-buffer-and-window
  "," 'evil-repeat-find-char-reverse)

(dolist (mode '(emacs-lisp-mode
                lisp-interaction-mode
                scheme-mode))
  (evil-leader/set-key-for-mode mode
    "er" 'eval-region
    "eb" 'eval-buffer
    "ed" 'eval-defun
    "es" 'eval-last-sexp))

(evil-leader/set-key-for-mode 'python-mode
  "eb" 'python-shell-send-buffer
  "ed" 'python-shell-send-defun
  "er" 'python-shell-send-region)

;;; Last, but not least...
(evil-mode 1)

(provide 'danf-evil)
