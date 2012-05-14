;; Turn off scrollbars early in startup to avoid window width weirdness.
(scroll-bar-mode -1)
(tool-bar-mode -1)


;;; Package manager
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(nconc package-archives
       '(("melpa" . "http://melpa.milkbox.net/packages/")
         ("marmalade" . "http://marmalade-repo.org/packages/")))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages
  '(fill-column-indicator
    goto-last-change
    haml-mode
    ido-ubiquitous
    magit
    paredit
    pretty-symbols-mode))
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


;;; Keys
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))
(global-set-key (kbd "C-w") 'my-kill-region-or-backward-kill-word)
(global-set-key (kbd "C-x k") 'my-kill-buffer)

(global-set-key (kbd "<f5>") 'switch-to-prev-buffer)
(global-set-key (kbd "<f6>") 'switch-to-next-buffer)

(global-set-key (kbd "C-c C-m") 'execute-extended-command)
(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "C-c l") 'toggle-truncate-lines)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "C-c t") 'my-cycle-tab-width)
(global-set-key (kbd "C-c u") 'goto-last-change-with-auto-marks)
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-c .") 'imenu)
(global-set-key (kbd "C-c SPC") 'my-frame-move)

;; Use regex searches by default
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; TODO: goto-last-change-with-automarks (my "C-c u") isn't autoloaded.
(require 'goto-last-change)

(add-hook 'diff-mode-hook
  (lambda () (local-unset-key (kbd "M-q")))) ; don't override fill-paragraph.

(define-key emacs-lisp-mode-map (kbd "M-.") 'find-function-at-point)

(when (display-graphic-p)
  (global-set-key (kbd "C-x C-c") 'my-ask-before-quitting))

(when (eq system-type 'darwin)  ; OS X
  ;;(global-set-key (kbd "s-q") 'my-ask-before-quitting)
  (setq mac-command-modifier 'meta)
  (global-set-key (kbd "M-`") 'other-frame)
  (eval-after-load 'term '(define-key term-raw-map (kbd "M-`") nil)))


;;; Misc. functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-kill-region-or-backward-kill-word (&optional arg region)
  "`kill-region' if the region is active, otherwise `backward-kill-word'."
  (interactive
   (list (prefix-numeric-value current-prefix-arg) (use-region-p)))
  (if region
      (kill-region (region-beginning) (region-end))
    (backward-kill-word arg)))

(defun my-cycle-tab-width ()
  "Cycle tab-width between values 2, 4, and 8."
  (interactive)
  (cond
   ((= tab-width 8) (setq tab-width 2) (message "Set tab width to 2"))
   ((= tab-width 2) (setq tab-width 4) (message "Set tab width to 4"))
   (t               (setq tab-width 8) (message "Set tab width to 8")))
  (redraw-display))

;; Switch to last buffer without having to press return.
(defun my-prev-buffer ()
  "Switch to previous buffer."
  (interactive)
  (switch-to-buffer (other-buffer)))

(defun my-kill-buffer ()
  "Kill current buffer without confirmation (unless modified)."
  (interactive)
  (kill-buffer (current-buffer)))

(defun my-ask-before-quitting ()
  "Prompt before quitting Emacs."
  (interactive)
  (if (y-or-n-p "Are you sure you want to exit Emacs? ")
      (save-buffers-kill-emacs)
    (message "Canceled exit")))


;;; Typography & colours
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cond
 ((eq system-type 'darwin)
  (set-face-attribute 'default nil :family "Menlo" :height 120)
  (set-face-attribute 'variable-pitch nil :family "Verdana" :height 140))
 (t
  (set-face-attribute 'default nil :family "DejaVu Sans Mono" :height 100)))

(set-face-attribute 'font-lock-comment-face nil :foreground "#3f7f5f")
(set-face-attribute 'font-lock-string-face nil :foreground "#3f003f")
(set-face-attribute 'font-lock-constant-face nil :foreground "#5f005f")
(set-face-attribute 'font-lock-keyword-face nil :foreground "#00003f")
(set-face-attribute 'font-lock-builtin-face nil :foreground "#00003f")
(set-face-attribute 'font-lock-type-face nil :foreground "#000000")
(set-face-attribute 'font-lock-function-name-face nil
                    :foreground "#000000" :weight 'bold)
(set-face-attribute 'font-lock-variable-name-face nil
                    :foreground "#000000" :weight 'bold)

(set-face-attribute 'region nil :background "#add4ff")

(eval-after-load 'cperl-mode
  '(progn
     (set-face-attribute 'cperl-nonoverridable-face nil :foreground "#00003f")))

(eval-after-load 'diff-mode
  '(progn
     (set-face-attribute 'diff-added nil :foreground "#005f00")
     (set-face-attribute 'diff-removed nil :foreground "#af0000")))

(eval-after-load 'hi-lock
  '(progn
     (set-face-attribute 'hi-yellow nil :background "#fff58f")
     (set-face-attribute 'hi-green nil :background "#c0efc1")))

(eval-after-load 'magit
  '(progn
     (set-face-attribute 'magit-diff-add nil :foreground "#005f00")
     (set-face-attribute 'magit-diff-del nil :foreground "#af0000")))

(eval-after-load 'sh-script
  '(progn
     (set-face-attribute 'sh-heredoc nil :foreground "#00007f")
     (set-face-attribute 'sh-quoted-exec nil :foreground "#5f5fff")))

(eval-after-load 'whitespace
  '(progn
     (setq whitespace-style
           '(face
             tabs trailing space-before-tab newline
             indentation empty space-after-tab
             tab-mark))
     (set-face-attribute 'whitespace-empty nil :background "#fff0f0")
     (set-face-attribute 'whitespace-indentation nil
                         :background "#fff0f0" :foreground "#cccccc")
     (set-face-attribute 'whitespace-line nil
                         :background "#fff0f0" :foreground nil)
     (set-face-attribute 'whitespace-space-after-tab nil
                         :background "#fff0f0" :foreground "#cccccc")
     (set-face-attribute 'whitespace-space-before-tab nil :background "#fff0f0")
     (set-face-attribute 'whitespace-tab nil :background "#fff0f0")
     (set-face-attribute 'whitespace-trailing nil
                         :background "#fff0f0" :foreground "#cccccc")))

(global-hl-line-mode 1)


;;; Navigation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(setq ido-save-directory-list-file "~/.emacs.d/.ido.last")
(ido-mode 1)
(ido-ubiquitous-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-default-buffer-method 'selected-window)

;; Allows C-u C-SPC C-SPC instead of C-u C-SPC C-u C-SPC
(setq set-mark-command-repeat-pop t)


;;; Programming
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(show-paren-mode 1)
(add-hook 'prog-mode-hook 'turn-on-eldoc-mode)

(setq-default indent-tabs-mode nil)
(add-hook 'prog-mode-hook 'whitespace-mode)

(add-hook 'prog-mode-hook 'turn-on-fci-mode)

;; Create our own style for c-mode and friends
(c-add-style
 "my-c-style"                  ; define new style with this name
 '("gnu"                       ; ...based on this style
   (tab-width . 8)
   (c-basic-offset . 4)
   (indent-tabs-mode . nil)    ; don't use tab characters for indentation
   (c-offsets-alist . (
     (substatement-open . 0)   ; don't indent opening brackets
     (case-label . 4)          ; indent case labels:
     (arglist-intro . +)       ; indent first argument +
     (arglist-cont . 0)        ; line up following args with first one
     (inextern-lang . 0)       ; don't indent inside extern blocks
     (innamespace . 0)))))     ; don't indent inside namespaces
(setq c-default-style "my-c-style")

(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))

(setq pretty-symbol-categories '(lambda relational))
(add-hook 'prog-mode-hook 'pretty-symbols-mode)

(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
;; Don't enable paredit by default in other languages, but when I do want it:
(defun my-space-for-paredit-delimiter-p (endp delimiter)
  (not (member major-mode '(c-mode c++-mode python-mode js-mode))))
(eval-after-load 'paredit
  '(add-to-list 'paredit-space-for-delimiter-predicates
                'my-space-for-paredit-delimiter-p))

;; A monkeypatch to cause annotate to ignore whitespace
(defun vc-git-annotate-command (file buf &optional rev)
  (let ((name (file-relative-name file)))
    (vc-git-command buf 'async nil "blame" "--date=iso" "-C" "-C" "-w" rev "--" name)))


;;; Shell mode and shell commands (M-x grep etc.)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setenv "PAGER" "/bin/cat")


;;; Info
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-after-load 'info
  '(progn
     (push "/opt/local/share/info" Info-default-directory-list)
     (push "~/.emacs.d/info" Info-default-directory-list)
     (setq Info-additional-directory-list
           '("~/work/info" "/opt/local/share/info"))))


;;; GUI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (display-graphic-p)
  (setq frame-title-format '(buffer-file-name "%f" "%b"))
  ;; Server for `emacsclient' from the shell.
  (server-start))

(when (or (not (display-graphic-p))
          (not (eq system-type 'darwin)))
  (menu-bar-mode -1))

;; Unfortunately my OS X window manager (Divvy) doesn't work with X11 windows.
(defun my-frame-move (position)
  (interactive "cPosition (D/F left/right; J/K/L left/middle/right; SPC fullscreen): ")
  ((lambda (l+w)
     (let ((l (car l+w)) (w (cdr l+w)))
       (modify-frame-parameters nil `((left . ,l) (width . ,w)
                                      (top . 0) (height . 68)))))
   (cond
    ((eq position ?h) '(0 . 80))        ; left 1/3 (qwerty J)
    ((eq position ?t) '(610 . 80))      ; middle 1/3 (qwerty K)
    ((eq position ?n) '(1265 . 80))     ; right 1/3 (qwerty L)
    ((eq position ?e) '(0 . 118))       ; left 1/2 (qwerty D)
    ((eq position ?u) '(960 . 118))     ; right 1/2 (qwerty F)
    ((eq position #x20) '(0 . 238))     ; full screen (spacebar)
    (t (error "my-frame-move: Invalid position code")))))


;;; OS X
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (eq system-type 'darwin)
  (setenv "PATH" (concat
                  (getenv "HOME") "/bin:"
                  exec-directory ":"
                  "/opt/local/bin:"
                  (getenv "PATH")))
  (setq exec-path (split-string (getenv "PATH") ":"))
  (setq ispell-program-name "/opt/local/bin/aspell")
  (setenv "ASPELL_CONF" nil)
  ;; OS X 'ls' doesn't support '--dired' flag.
  (setq dired-use-ls-dired nil))


;;; Misc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tooltip-mode -1)
(setq visible-bell t)
(setq inhibit-startup-screen t)
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'qrr 'query-replace-regexp)
(setq make-backup-files nil)
(column-number-mode 1)
(setq-default truncate-lines t)
(setq sentence-end-double-space nil)
(setq-default fill-column 79)   ; use C-x f to set buffer-local value.

(setq custom-file "~/.emacs.d/emacs-custom.el")
(load-file custom-file)

(eval-after-load 'bookmark
  (setq bookmark-default-file "~/.emacs.d/bookmarks"))
