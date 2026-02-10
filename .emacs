(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(agent-shell consult exec-path-from-shell magit marginalia markdown-mode orderless projectile s treesit-auto vertico xterm-color)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; TODO: revisit corfu, embark, flycheck, eglot when frames are in terminal emacs (31+)

;; initialization
(tool-bar-mode -1)
(menu-bar-mode -1)
(toggle-frame-maximized)
(setq inhibit-startup-message t) 
(setq initial-scratch-message nil)

;; install if missing
(unless package-archive-contents
  (package-refresh-contents))
(package-install-selected-packages)

(eval-when-compile
  (require 'use-package))

;; theme
(load-theme 'modus-operandi t)

;; shell
(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns x))
  :config
  (setenv "SHELL" "/bin/zsh")
  (exec-path-from-shell-initialize))

;; agent-shell
(use-package agent-shell
    :ensure t
    :ensure-system-package
    ((claude . "brew install claude-code")
     (claude-code-acp . "npm install -g @zed-industries/claude-code-acp")))
(setq agent-shell-anthropic-authentication
      (agent-shell-anthropic-make-authentication :login t))
(setq agent-shell-preferred-agent-config (agent-shell-anthropic-make-claude-code-config))
(setq agent-shell-transcript-file-path-function nil)

;; magit
(use-package magit
  :ensure t)

;; treesit
(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; which key
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; pixel scroll
(pixel-scroll-precision-mode)

;; useful built-in modes
(show-paren-mode 1)
(electric-pair-mode 1)
(delete-selection-mode 1)
(global-auto-revert-mode 1)
(winner-mode 1)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq js-indent-level 2)
(setq ruby-indent-level 2)
(setq-default fill-column 150)
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)

;; completion settings
(setq tab-always-indent 'complete)
(setq completion-cycle-threshold 3)

;; clipboard integration
(setq select-enable-clipboard t)
(setq select-enable-primary t)  ; Also sync with X primary selection (useful on Linux)

;; vertico
(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package savehist
  :init
  (savehist-mode))

(use-package emacs
  :custom
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)))

;; orderless
(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless partial-completion basic)
        completion-category-defaults nil
        completion-category-overrides nil))

;; marginalia
(use-package marginalia
  :ensure t
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;; consult
(use-package consult
  :ensure t
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

;; projectile
(use-package projectile
  :ensure t
  :bind (("C-c p" . projectile-command-map))
  :custom
  (projectile-project-search-path '(("~/dev/" . 1) ("~/invoca/" . 1)))
  (projectile-ignored-projects '("~/"))
  :config
  (projectile-mode +1))

;; pinentry/gpg
(require 'epg)
(setq epg-pinentry-mode 'loopback)
;; https://gist.github.com/bmhatfield/cc21ec0a3a2df963bffa3c1f884b676b

;; dired enhancements
(setq dired-dwim-target t)              ; Guess target directory for copy/move
(setq dired-listing-switches "-alh")    ; Human-readable file sizes

;; git
;; https://simpleit.rocks/git/make-git-ignore-temporary-files-produced-by-emacs-and-vim-in-all-directories-globally/
;; install ag/rg

;; shell
;; iterm2 disable command w, enable alt/option
(require 'xterm-color)
(setq compilation-environment '("TERM=xterm-256color"))

(defun my/advice-compilation-filter (f proc string)
  (funcall f proc (xterm-color-filter string)))

(advice-add 'compilation-filter :around #'my/advice-compilation-filter)

;; zsh
;; export GPG_TTY=$(tty)
;; export TERM=xterm-256color

;; cleanup backup files
;; find . -name '*~' -delete
;; find . -name '*#' -delete
(setq backup-directory-alist '(("." . "~/.emacs.d/backup")))
(setq backup-by-copying t)           ; Don't delink hardlinks
(setq version-control t)             ; Use version numbers on backups
(setq delete-old-versions t)         ; Automatically delete excess backups
(setq kept-new-versions 20)          ; How many of the newest versions to keep
(setq kept-old-versions 5)           ; How many of the old versions to keep

;; get the relative path of the current buffer
(defun copy-relative-path-with-prefix (&optional prefix)
  "Copy the current buffer file name to the clipboard with optional PREFIX."
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (let ((fullname (if prefix (concat prefix filename) filename)))
        (kill-new fullname)
        (shell-command (concat "echo -n " (shell-quote-argument fullname) " | pbcopy"))
        (message "Copied: %s" fullname)))))

(defun copy-relative-path ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (copy-relative-path-with-prefix))

(defun copy-relative-path-rspec ()
  "Copy the current buffer file name with rspec prefix to the clipboard."
  (interactive)
  (copy-relative-path-with-prefix "bundle exec rspec "))

(defun copy-relative-path-ruby ()
  "Copy the current buffer file name with ruby prefix to the clipboard."
  (interactive)
  (copy-relative-path-with-prefix "bundle exec ruby "))

(defun run-test-file ()
  "Run the test for the current buffer if it's a test file.
For RSpec files (*_spec.rb), runs 'bundle exec rspec <file>'.
For Minitest files (*_test.rb), runs 'bundle exec ruby <file>'.
Command runs from the project root directory using Projectile."
  (interactive)
  (let ((filename (buffer-file-name))
        (project-root (projectile-project-root)))
    (if (not filename)
        (message "Buffer is not visiting a file")
      (if (not project-root)
          (message "Not in a Projectile project")
        (cond
         ((string-match "_spec\\.rb$" filename)
          (let ((default-directory project-root))
            (compile (concat "bundle exec rspec " (shell-quote-argument filename)))))
         ((string-match "_test\\.rb$" filename)
          (let ((default-directory project-root))
            (compile (concat "bundle exec ruby " (shell-quote-argument filename)))))
         (t
          (message "Not a test file (must end with _spec.rb or _test.rb)")))))))
