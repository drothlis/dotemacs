;;; -*- lexical-binding: t; -*-

;; Turn off scrollbars early in startup to avoid window width weirdness.
(scroll-bar-mode -1)
(tool-bar-mode -1)


;;; Package manager

(require 'package)
(nconc package-archives
       '(;; ("marmalade" . "http://marmalade-repo.org/packages/")
         ("melpa-stable" . "https://stable.melpa.org/packages/")))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages
  '(diff-hl
    dired-details
    dired-details+
    dockerfile-mode
    expand-region
    fill-column-indicator
    flycheck
    goto-last-change
    haml-mode
    highlight-symbol
    htmlize
    ido-ubiquitous
    imenu-anywhere
    jinja2-mode
    lsp-mode
    lsp-ui
    lua-mode
    magit
    markdown-mode
    markdown-preview-mode
    paredit
    rust-mode
    typescript-mode
    wsd-mode  ; websequencediagrams.com
    yaml-mode
    ))
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Not available as a package
(add-to-list 'load-path (concat user-emacs-directory "non-elpa"))

(setenv "PATH" (concat
                (getenv "HOME") "/local/bin:"
                (getenv "HOME") "/.local/bin:"
                (getenv "PATH")))


;;; Keys

;; This minimizes the window and sometimes continues frozen even after I
;; alt-tab back to it.
(global-unset-key (kbd "C-z"))

(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))
(global-set-key (kbd "C-x k") 'my-kill-buffer)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-=") 'er/expand-region)

(global-set-key (kbd "<f5>") 'switch-to-prev-buffer)
(global-set-key (kbd "<f6>") 'switch-to-next-buffer)
(global-set-key (kbd "<f7>") 'recompile)
(global-set-key (kbd "<f8>") 'iimage-mode)

(global-set-key (kbd "<next>") 'forward-page) ; page down
(global-set-key (kbd "<prior>") 'backward-page) ; page up

(global-set-key (kbd "C-c C-m") 'execute-extended-command)
(global-set-key (kbd "C-c c") 'git-grep)
(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "C-c t") 'my-cycle-tab-width)
(global-set-key (kbd "C-c u") 'goto-last-change-with-auto-marks)
(global-set-key (kbd "C-c w") 'my-toggle-truncate-lines)
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-c .") 'imenu)
(global-set-key (kbd "C-c ,") 'imenu-anywhere)
(global-set-key (kbd "C-c \\") 'align-regexp)

;; Suggested by "The compact org-mode guide"
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-switchb)
(setq org-completion-use-ido t)
(setq org-clock-idle-time 30)
(setq org-clock-display-default-range 'untilnow)

;; Use regex searches by default
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

(windmove-default-keybindings 'meta) ; M-<up> for windmove-up, etc.

;; TODO: Get these in dired by default.
(autoload 'dired-jump "dired-x"
  "Jump to Dired buffer corresponding to current buffer." t)
(autoload 'dired-jump-other-window "dired-x"
  "Like \\[dired-jump] (dired-jump) but in other window." t)
(global-set-key (kbd "C-x C-j") 'dired-jump)
(global-set-key (kbd "C-x 4 C-j") 'dired-jump-other-window)

;; TODO: goto-last-change-with-automarks (my "C-c u") isn't autoloaded.
(require 'goto-last-change)

(add-hook 'diff-mode-hook
  (lambda () (local-unset-key (kbd "M-q")))) ; don't override fill-paragraph.

(define-key emacs-lisp-mode-map (kbd "M-.") 'find-function-at-point)

(eval-after-load 'cc-mode
  '(define-key c-mode-base-map (kbd "C-c .") nil))

(eval-after-load 'diff-hl
  '(progn
     (global-set-key (kbd "M-g [") 'diff-hl-previous-hunk)
     (global-set-key (kbd "M-g ]") 'diff-hl-next-hunk)))

;; For consistency with help-mode history navigation keybindings.
;; TODO: Submit to Emacs?
(eval-after-load 'info
  '(progn
     (define-key Info-mode-map (kbd "C-c C-b") 'Info-history-back)
     (define-key Info-mode-map (kbd "C-c C-f") 'Info-history-forward)))

(when (eq system-type 'darwin)  ; OS X
  (setq mac-command-modifier 'meta)
  (global-set-key (kbd "M-`") 'other-frame)
  (eval-after-load 'term '(define-key term-raw-map (kbd "M-`") nil)))


;;; Misc. commands

(defun my-cycle-tab-width ()
  "Cycle tab-width between values 2, 4, and 8."
  (interactive)
  (cond
   ((= tab-width 8) (setq tab-width 2) (message "Set tab width to 2"))
   ((= tab-width 2) (setq tab-width 4) (message "Set tab width to 4"))
   (t               (setq tab-width 8) (message "Set tab width to 8")))
  (redraw-display))

(defun my-toggle-truncate-lines ()
  "In text modes disable visual-line-mode; otherwise toggle truncate-lines.

Note that I enable visual-line-mode (virtual line wrapping at
word boundaries) in text-mode-hook."
  (interactive)
  (if visual-line-mode
      (visual-line-mode -1)
    (toggle-truncate-lines)))

;; Switch to last buffer without having to press return.
(defun my-prev-buffer ()
  "Switch to previous buffer."
  (interactive)
  (switch-to-buffer (other-buffer)))

(defun my-kill-buffer ()
  "Kill current buffer without confirmation (unless modified)."
  (interactive)
  (kill-buffer (current-buffer)))

(defun do-after-save (after-save-command)
  "Run the specified shell command after every time the current buffer is saved"
  (interactive "MShell command to run after saving this buffer: ")
  (add-hook 'after-save-hook (lambda () (shell-command after-save-command)) t t))


;;; Typography & colours

(cond
 ((eq system-type 'darwin)
  (set-face-attribute 'default nil :family "Menlo" :height 120)
  (set-fontset-font "fontset-default" 'unicode "Menlo")
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

(eval-after-load 'ido
  ;; Face for "virtual" (recently-closed) buffers. TODO: Improve ido-virtual docstring.
  '(set-face-attribute 'ido-virtual nil :foreground "#7f7f7f"))

(eval-after-load 'web-mode
  '(progn
     (set-face-attribute 'web-mode-html-attr-value-face nil :foreground "#33334f")
     (set-face-attribute 'web-mode-html-tag-bracket-face nil :foreground "Snow4")))

(global-hl-line-mode)


;;; Navigation

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(setq ido-save-directory-list-file "~/.emacs.d/.ido.last")
(ido-mode 1)
(ido-ubiquitous-mode)
(setq ido-enable-flex-matching t
      ido-default-buffer-method 'selected-window
      ido-use-filename-at-point 'guess
      ido-use-url-at-point nil
      ido-auto-merge-work-directories-length -1 ; disable auto-search; use M-s
      ido-create-new-buffer 'always
      ido-use-virtual-buffers t         ; keep recently-closed buffers in list
      ;;ido-show-dot-for-dired t
      )
(setq ffap-url-regexp nil) ; disable URL lookup in ido-use-filename-at-point
(setq ffap-machine-p-known 'reject)
(setq ffap-machine-p-local 'reject)
(setq ffap-machine-p-unknown 'reject)

;; Allows C-u C-SPC C-SPC instead of C-u C-SPC C-u C-SPC
(setq set-mark-command-repeat-pop t)

;; Enable desktop-save-mode whenever I use desktop-change-dir
(eval-after-load 'desktop
  '(progn
     (add-hook 'desktop-after-read-hook 'desktop-save-mode)
     (setq desktop-restore-eager 10)))

;; Remember cursor position in each file
(require 'saveplace)
(setq save-place-file "~/.emacs.d/saved-places")
(setq-default save-place t)

;; Remember minibuffer history
(require 'savehist)
(setq savehist-file "~/.emacs.d/saved-history")
(savehist-mode 1)

(add-hook 'occur-mode-hook 'next-error-follow-minor-mode)

(eval-after-load 'dired '(require 'dired-x))
(add-hook 'dired-mode-hook 'dired-omit-mode)
(eval-after-load 'dired '(require 'dired-details+))

(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)


;;; Text

(setq-default major-mode 'text-mode)

(add-hook 'text-mode-hook 'visual-line-mode) ; virtual line wrapping
(add-hook 'sgml-mode-hook (lambda () (visual-line-mode -1))) ; ...except in html-mode
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

(add-hook 'text-mode-hook 'outline-minor-mode)

(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("README\\.md$" . gfm-mode))
(setq-default markdown-indent-on-enter nil)

(put 'upcase-region 'disabled nil)

(setq sort-fold-case t)

(eval-after-load 'markdown-mode
  '(setq markdown-command "pandoc -f gfm"))


;;; Programming

(show-paren-mode)
(setq xref-prompt-for-identifier t)
(add-hook 'prog-mode-hook 'eldoc-mode)
(setq-default indent-tabs-mode nil)
;; (add-hook 'prog-mode-hook 'fci-mode)

;;; LSP (Language Server Protocol) for Rust etc.
(require 'lsp-mode)
(add-hook 'rust-mode-hook #'lsp)
;;; Settings from https://emacs-lsp.github.io/lsp-mode/page/performance/
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))
(setq lsp-completion-provider :capf)
(setq lsp-rust-server 'rust-analyzer)
(setq lsp-enable-snippet nil)
(setq lsp-modeline-code-actions-enable nil)

(add-hook 'prog-mode-hook 'outline-minor-mode)

(add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)

(which-function-mode)
;; Bring mode-line-misc-info (for which-function) before mode-line-modes
(setq-default mode-line-format
  '("%e" mode-line-front-space mode-line-mule-info
    mode-line-client mode-line-modified mode-line-remote
    mode-line-frame-identification mode-line-buffer-identification " "
    mode-line-position
    mode-line-misc-info
    flycheck-mode-line
    (vc-mode vc-mode) "  "
    mode-line-modes mode-line-end-spaces))

;; TAB completes using completion-at-point, which uses language-appropriate
;; completion (the Emacs interpreter's internal knowledge in elisp modes, tags
;; tables in C and other modes, semantic-mode information when enabled).
(setq tab-always-indent 'complete)
;; M-/ completes using hippie-expand, which mostly uses dabbrev, i.e. just
;; finds any matching text in open buffers.
(setq hippie-expand-try-functions-list
  '(try-complete-file-name-partially
    try-complete-file-name
    try-expand-all-abbrevs
    try-expand-dabbrev
    try-expand-dabbrev-visible
    try-expand-dabbrev-all-buffers
    try-expand-dabbrev-from-kill
    try-complete-lisp-symbol-partially
    try-complete-lisp-symbol))

(setq highlight-symbol-idle-delay 0.1)
(add-hook 'prog-mode-hook 'highlight-symbol-mode)

;; Disable distracting whitespace-mode highlighting when in view-mode.
(add-hook 'prog-mode-hook 'whitespace-mode)
(setq view-read-only t) ; enable view-mode with C-x C-q
(defun my-toggle-read-only ()
  (interactive)
  (toggle-read-only)
  (whitespace-mode (if view-mode -1 1)))
(global-set-key (kbd "C-x C-q") 'my-toggle-read-only)
(eval-after-load 'view
  '(define-key view-mode-map "e" 'my-toggle-read-only))

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

;; TODO: Disable paired apostrophes inside python "" strings.
;;(electric-pair-mode)

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
    (vc-git-command buf 'async nil
                    "blame" "--date=iso" "-M" "-C" "-C" "-w" rev "--" name)))

(eval-after-load 'magit
  '(progn
     (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
     (setq magit-push-always-verify nil)
     (setq git-commit-fill-column 72)
     (add-hook 'magit-log-edit-mode-hook (lambda () (set-fill-column 72)))
     (add-to-list 'same-window-regexps "\*magit: .*\*")
     (add-to-list 'same-window-regexps "\*magit-log: .*\*")))

(defun git-grep (search-term)           ; https://www.ogre.com/node/447
  "git-grep the entire current repo"
  (interactive (list (completing-read "Search for: " nil nil nil (current-word))))
  (grep-find (concat "git grep -P -n -e " search-term " `git rev-parse --show-toplevel`")))

(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(eval-after-load 'ruby-mode
  '(progn
     (define-key ruby-mode-map "{" nil)  ;
     (define-key ruby-mode-map "}" nil)  ; disable electric indentation.
     (setq ruby-deep-indent-paren nil)
     (setq ruby-insert-encoding-magic-comment nil))) ; ANNOYING!

;; Copied from instructions at the top of python.el
;; (setq
;;  python-shell-interpreter "ipython"
;;  python-shell-interpreter-args "-i"
;;  ;; python-shell-prompt-regexp "In \\[[0-9]+\\]: "
;;  ;; python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
;;  ;; python-shell-completion-setup-code
;;  ;;   "from IPython.core.completerlib import module_completion"
;;  ;; python-shell-completion-module-string-code
;;  ;;   "';'.join(module_completion('''%s'''))\n"
;;  ;; python-shell-completion-string-code
;;  ;;   "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
;;  python-fill-docstring-style 'pep-257-nn
;;  )
(add-hook 'python-mode-hook
          (lambda () (setq imenu-create-index-function
                      #'python-imenu-create-flat-index)))
;; (add-hook 'python-mode-hook (lambda () (eldoc-mode -1)))

(eval-after-load 'haskell-mode
  '(progn
     (turn-on-haskell-doc)
     ;(turn-on-haskell-indentation)
     (turn-on-haskell-simple-indent)))

(add-hook 'html-mode-hook (lambda () (run-hooks 'prog-mode-hook)))

;; Teach js-mode/imenu about Angular-style functions/classes. Taken from
;; https://github.com/redguardtoo/emacs.d/blob/master/lisp/init-javascript.el
(setq javascript-common-imenu-regex-list
      '((nil "[. \t]controller([ \t]*['\"]\\([^'\"]+\\)" 1)
        (nil "[. \t]filter([ \t]*['\"]\\([^'\"]+\\)" 1)
        (nil "[. \t]factory([ \t]*['\"]\\([^'\"]+\\)" 1)
        (nil "[. \t]service([ \t]*['\"]\\([^'\"]+\\)" 1)
        (nil "[. \t]module([ \t]*['\"]\\([a-zA-Z0-9_\.]+\\)" 1)
        (nil "[. \t]directive([ \t]*['\"]\\([^'\"]+\\)" 1)
        (nil "[. \t]component([ \t]*['\"]\\([^'\"]+\\)" 1)
        ("Event" "[. \t]\$on([ \t]*['\"]\\([^'\"]+\\)" 1)
        ("Config" "[. \t]config([ \t]*function *( *\\([^\)]+\\)" 1)
        ("Config" "[. \t]config([ \t]*\\[ *['\"]\\([^'\"]+\\)" 1)
        ("OnChange" "[ \t]*\$(['\"]\\([^'\"]*\\)['\"]).*\.change *( *function" 1)
        ("OnClick" "[ \t]*\$([ \t]*['\"]\\([^'\"]*\\)['\"]).*\.click *( *function" 1)
        (nil "function[ \t]+\\([a-zA-Z0-9_$.]+\\)[ \t]*(" 1)
        (nil "^[ \t]*\\([a-zA-Z0-9_$.]+\\)[ \t]*=[ \t]*function[ \t]*(" 1)
        ("Task" "[. \t]task([ \t]*['\"]\\([^'\"]+\\)" 1)
        ))
(defun js-imenu-make-index ()
  (save-excursion
    (imenu--generic-function javascript-common-imenu-regex-list)))
(add-hook 'js-mode-hook
          (lambda () (setq imenu-create-index-function 'js-imenu-make-index)))
(add-hook 'js2-mode-hook
          (lambda () (setq imenu-create-index-function 'js-imenu-make-index)))
(setq js2-basic-offset 2)
(setq js-switch-indent-offset 2)

(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))


;;; Shell mode and shell commands (M-x grep etc.)

(setenv "PAGER" "/bin/cat")


;;; Info
(require 'info-look)
(info-lookup-add-help
 :mode 'python-mode
 :regexp "[[:alnum:]_]+"
 :doc-spec
 '(("(python3.10)" nil "")))


;;; GUI, frames & windows

(when (display-graphic-p)
  (setq confirm-kill-emacs 'y-or-n-p)
  (setq frame-title-format '(buffer-file-name "%f" "%b"))
  (setq initial-frame-alist
        '((left . 1720) (width . 81) (top . 0) (height . 100)))
  ;; Server for `emacsclient' from the shell.
  (require 'server)
  (unless (eq t (server-running-p)) (server-start)))

(when (or (not (display-graphic-p))
          (not (eq system-type 'darwin)))
  (menu-bar-mode -1))

;; Stop "q" within a full-frame help or magit buffer from minimising the frame.
(setq frame-auto-hide-function (lambda (frame) (bury-buffer)))

;; Always split windows horizontally (side-by-side) if window is wide enough;
;; otherwise Emacs will fall back to splitting vertically, regardless of
;; the window's height. See the help for `split-window-sensibly'.
(setq split-height-threshold nil)

;; Home-made 3-column window positioning in X11.
;; Coordinates are hardcoded to my 2560x1600 laptop screen.
(defun my-frame-move (position)
  (interactive "cPosition (J/K/L left/middle/right): ")
  ((lambda (l)
     (modify-frame-parameters nil `((left . ,l) (width . 81)
                                      (top . 0) (height . 100))))
   (cond
    ((eq position ?h) 82)    ; qwerty J
    ((eq position ?t) 908)   ; qwerty K
    ((eq position ?n) 1735)  ; qwerty L
    (t (error "my-frame-move: Invalid position code")))))
(global-set-key (kbd "C-c SPC") 'my-frame-move)


;;; Email

(require 'gnus)
(setq nnml-directory "~/Documents/gmail")
(setq message-directory "~/Documents/gmail")
(setq gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\”]\”[#’()]")

(setq gnus-select-method
      '(nnimap "gmail"
               (nnimap-address "imap.gmail.com")
               (nnimap-server-port 993)
               (nnimap-stream ssl)))
(setq starttls-use-gnutls t)
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials '(("smtp.gmail.com" 587 "drothlis@gmail.com" nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)

(setq gnus-large-newsgroup nil)


;;; OS X

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
  (setq dired-use-ls-dired nil)
  ;; Use Spotlight for M-x locate
  (setq locate-command "mdfind")
  ;; Sane trackpad scrolling
  (setq mouse-wheel-scroll-amount '(1 ((control))))
  )


;;; Misc

(tooltip-mode -1)
(setq visible-bell t)
(setq inhibit-startup-screen t)
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'qrr 'query-replace-regexp)
(setq make-backup-files nil)
(column-number-mode)
(setq-default truncate-lines t)
(setq scroll-error-top-bottom t)
(setq sentence-end-double-space nil)
(setq-default fill-column 79)   ; use C-x f to set buffer-local value.
(setq dired-auto-revert-buffer t)

(setq custom-file "~/.emacs.d/emacs-custom.el")
(load-file custom-file)

(eval-after-load 'bookmark
  (setq bookmark-default-file "~/.emacs.d/bookmarks"))


;;; Stb-tester

(require 'flycheck)
(flycheck-define-checker stb-tester-checker
  "Run custom pylint & pep8 checks for stb-tester repository"
  :command ("env" "PYTHONPATH=." "PYLINT=pylint"
            "extra/pylint.sh" source-inplace)
  :working-directory (lambda (checker)
                       (locate-dominating-file (buffer-file-name) ".git"))
  :error-patterns ((error line-start (file-name) ":" line
                          (zero-or-one ":" column) ": "
                          (message) line-end))
  :error-filter (lambda (errors) (flycheck-increment-error-columns errors))
  :modes python-mode
  :predicate (lambda () (string-match "/stb-tester/" buffer-file-truename)))
(flycheck-define-checker stb-tester-one-python3-checker
  "Run custom pylint & pep8 checks for stb-tester-one repository"
  :command ("env" "PYTHONPATH=./pythonpath"
            "sh" "-c"
            "pylint --output-format=text $1"
            "--" source-inplace)
  :working-directory (lambda (checker)
                       (locate-dominating-file (buffer-file-name) ".git"))
  :error-patterns ((error line-start (file-name) ":" line
                          (zero-or-one ":" column) ": "
                          (message) line-end))
  :error-filter (lambda (errors) (flycheck-increment-error-columns errors))
  :modes python-mode
  :predicate (lambda ()
               (and (or (string-match "/central-server/" buffer-file-truename)
                        (string-match "/stbt_nursery/" buffer-file-truename)
                        (string-match "/test_pack_client/" buffer-file-truename)
                        (string-match "/test-pack-base-33/" buffer-file-truename)
                        (string-match "/test-packs/v33/" buffer-file-truename)
                        (string-match "/configure" buffer-file-truename))
                    (not (string-match "/stb-tester/" buffer-file-truename))
                    (not (string-match "test-packs/" buffer-file-truename)))))
(flycheck-define-checker stb-tester-one-checker
  "Run custom pylint & pep8 checks for stb-tester-one repository"
  :command ("~/.local/bin/pipenv" "run" "env" "PYTHONPATH=./pythonpath"
            "sh" "-c"
            "pylint --output-format=text $1"
            "--" source-inplace)
  :working-directory (lambda (checker)
                       (locate-dominating-file (buffer-file-name) ".git"))
  :error-patterns ((error line-start (file-name) ":" line
                          (zero-or-one ":" column) ": "
                          (message) line-end))
  :error-filter (lambda (errors) (flycheck-increment-error-columns errors))
  :modes python-mode
  :predicate (lambda ()
               (and (string-match "stb-tester-one\\|stb-tester-node"
                                  buffer-file-truename)
                    (not (string-match "/stb-tester/" buffer-file-truename))
                    (not (string-match "/central-server/" buffer-file-truename))
                    (not (string-match "/stbt_nursery/" buffer-file-truename))
                    (not (string-match "/test_pack_client/" buffer-file-truename))
                    (not (string-match "/test-pack-base-33/" buffer-file-truename))
                    (not (string-match "/test-packs/v33/" buffer-file-truename))
                    (not (string-match "/configure" buffer-file-truename)))))
(flycheck-define-checker test-pack-checker
  "Run stbt lint for stb-tester-test-pack repositories"
  :command (".venv/bin/pylint" "--load-plugins=stbt.pylint_plugin"
            "--output-format=parseable"
            source-inplace)
  :working-directory (lambda (checker)
                       (locate-dominating-file (buffer-file-name) ".pylintrc"))
  :error-patterns ((error line-start (file-name) ":" line
                          (zero-or-one ":" column) ": "
                          (message) line-end))
  :error-filter (lambda (errors) (flycheck-increment-error-columns errors))
  :modes python-mode
  :predicate (lambda ()
               (or (string-match "test-packs/" buffer-file-truename)
                   (string-match "/stb-tester-test-pack/" buffer-file-truename))))
(add-to-list 'flycheck-checkers 'stb-tester-checker)
(add-to-list 'flycheck-checkers 'stb-tester-one-python3-checker)
(add-to-list 'flycheck-checkers 'stb-tester-one-checker)
(add-to-list 'flycheck-checkers 'test-pack-checker)
(setq flycheck-check-syntax-automatically '(save idle-change mode-enabled))
(setq flycheck-highlighting-mode 'symbols)
(setq flycheck-checker-error-threshold 10000)
(add-hook
 'python-mode-hook
 (lambda ()
   (when (string-match "/\\(test-packs\\|stb-tester\\|stb-tester-one\\|stb-tester-node\\|stb-tester-test-pack\\)/"
                       buffer-file-truename)
     (flycheck-mode 1))))
