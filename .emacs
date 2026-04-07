(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(require 'use-package)
(setq use-package-always-ensure t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(agent-shell cape consult corfu corfu-terminal embark embark-consult exec-path-from-shell magit marginalia markdown-mode orderless projectile s shell-maker transient treesit-auto vertico
		 wgrep xterm-color)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; corfu - in-buffer completion popup
;; corfu-terminal replaces child frames with overlays for TUI support (Emacs 30)
(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2)
  (corfu-quit-no-match 'separator)
  :init
  (global-corfu-mode)
  :config
  (corfu-popupinfo-mode 1))

;; cape - additional completion-at-point sources for corfu (files, dabbrev, etc.)
(use-package cape
  :after corfu
  :init
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-dabbrev))

(use-package corfu-terminal
  :after corfu
  :config
  (unless (display-graphic-p)
    (corfu-terminal-mode +1))
  ;; Handle daemon spawning both GUI and TUI frames
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (if (display-graphic-p frame)
                  (corfu-terminal-mode -1)
                (corfu-terminal-mode +1)))))

;; embark - contextual actions on completions/symbols/regions
;; C-c C-e in minibuffer exports consult results to a grep buffer (then C-c C-p for wgrep)
(use-package embark
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings)
   :map minibuffer-local-map
   ("C-c C-e" . embark-export))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; eglot - built-in LSP client (pairs with flymake, which consult-flymake already targets)
;; Requires LSP servers: gem install ruby-lsp  |  npm i -g typescript-language-server
(use-package eglot
  :ensure nil
  :hook ((ruby-mode ruby-ts-mode js-mode js-ts-mode typescript-ts-mode tsx-ts-mode python-mode python-ts-mode) . eglot-ensure)
  :custom
  (eglot-autoshutdown t)
  ;; Disable ClassLength: it spans the entire class body when violated, highlighting the whole file
  (eglot-workspace-configuration '(:rubyLsp (:linters (:rubocop (:disabledCops ["Metrics/ClassLength"])))))
  :config
  ;; Use global ruby-lsp shim directly — it's not in the web Gemfile so bundle exec fails
  (add-to-list 'eglot-server-programs
               '((ruby-mode ruby-ts-mode) . ("ruby-lsp")))
  ;; Bust LSP completion cache on each keystroke to prevent stale candidates
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  ;; Compose LSP + file completions; dabbrev as fallback
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (setq-local completion-at-point-functions
                          (list (cape-capf-super #'eglot-completion-at-point #'cape-file)
                                #'cape-dabbrev)))))

;; initialization
(tool-bar-mode -1)
(menu-bar-mode -1)
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (when (display-graphic-p frame)
              (toggle-frame-maximized frame))))
(setq inhibit-startup-message t) 
(setq initial-scratch-message nil)

;; install if missing
(unless package-archive-contents
  (package-refresh-contents))
(package-install-selected-packages t)

;; theme
(load-theme 'modus-operandi t)

;; shell
(use-package exec-path-from-shell
  :config
  (setenv "SHELL" "/bin/zsh")
  (exec-path-from-shell-initialize))

;; agent-shell
(use-package agent-shell
    :ensure-system-package
    ((claude . "brew install claude-code")
     (claude-agent-acp . "npm install -g @agentclientprotocol/claude-agent-acp"))
    :config
    (setq agent-shell-anthropic-authentication
          (agent-shell-anthropic-make-authentication :login t))
    (setq agent-shell-preferred-agent-config (agent-shell-anthropic-make-claude-code-config))
    (setq agent-shell-transcript-file-path-function nil))

;; wgrep - edit consult-ripgrep/grep results in place (C-c C-p to enable, C-c C-c to apply)
(use-package wgrep
  :hook (grep-mode . wgrep-setup))

;; magit
(use-package magit)

;; treesit
(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; which key (built-in since Emacs 30)
(which-key-mode)

;; pixel scroll
(pixel-scroll-precision-mode)

;; useful built-in modes
(show-paren-mode 1)
(electric-pair-mode 1)
(delete-selection-mode 1)
(global-auto-revert-mode 1)
(winner-mode 1)
(recentf-mode 1)
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
  :init
  (setq completion-styles '(orderless partial-completion basic)
        completion-category-defaults nil
        completion-category-overrides '((eglot (styles orderless))
                                        (eglot-capf (styles orderless)))))

;; marginalia
(use-package marginalia
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;; consult
(use-package consult
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
         ("C-x r f" . consult-recent-file)         ;; recent files
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
  :bind (("C-c p" . projectile-command-map))
  :custom
  (projectile-project-search-path '(("~/dev/" . 1) ("~/invoca/" . 1)))
  (projectile-ignored-projects '("~/"))
  :config
  (projectile-mode +1)
  ;; Override the default projectile toggle keybinding
  (define-key projectile-command-map (kbd "t") 'project-toggle-between-implementation-and-test))

;; keybindings for custom functions
(global-set-key (kbd "C-c t t") 'run-test-file)
(global-set-key (kbd "C-c t w") 'copy-region-to-clipboard)

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
(make-directory "~/.emacs.d/backup" t)
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
                    (file-relative-name (buffer-file-name) (projectile-project-root)))))
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

(defun copy-region-to-clipboard ()
  "Copy the selected region to the system clipboard using pbcopy."
  (interactive)
  (if (use-region-p)
      (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
        (shell-command-on-region (region-beginning) (region-end) "pbcopy")
        (message "Copied %d characters to clipboard" (length text)))
    (message "No region selected")))

(defun project-toggle-between-implementation-and-test ()
  "Toggle between implementation and test files in a cycle.
Cycles through: implementation → *_spec.rb → *_test.rb → implementation.
Handles Rails conventions: app/ ↔ spec/ ↔ test/"
  (interactive)
  (let* ((filename (buffer-file-name))
         (impl-file nil)
         (spec-file nil)
         (test-file nil))
    (cond
     ;; Currently in spec file (spec/...)
     ((string-match "\\(.+\\)/spec/\\(.+\\)_spec\\.rb$" filename)
      (let ((root (match-string 1 filename))
            (relative-path (match-string 2 filename)))
        (setq impl-file (concat root "/app/" relative-path ".rb"))
        (setq test-file (concat root "/test/" relative-path "_test.rb"))
        (cond
         ;; If test file exists, go to it (continue cycle)
         ((file-exists-p test-file)
          (find-file test-file))
         ;; Otherwise go back to implementation
         ((file-exists-p impl-file)
          (find-file impl-file))
         (t (message "Implementation file not found: %s" impl-file)))))
     
     ;; Currently in test file (test/...)
     ((string-match "\\(.+\\)/test/\\(.+\\)_test\\.rb$" filename)
      (let ((root (match-string 1 filename))
            (relative-path (match-string 2 filename)))
        (setq impl-file (concat root "/app/" relative-path ".rb"))
        (cond
         ;; Always go back to implementation to complete the cycle
         ((file-exists-p impl-file)
          (find-file impl-file))
         (t (message "Implementation file not found: %s" impl-file)))))
     
     ;; Currently in implementation file (app/...)
     ((string-match "\\(.+\\)/app/\\(.+\\)\\.rb$" filename)
      (let ((root (match-string 1 filename))
            (relative-path (match-string 2 filename)))
        (setq spec-file (concat root "/spec/" relative-path "_spec.rb"))
        (setq test-file (concat root "/test/" relative-path "_test.rb"))
        (cond
         ;; Prefer spec file if it exists
         ((file-exists-p spec-file)
          (find-file spec-file))
         ;; Fall back to test file
         ((file-exists-p test-file)
          (find-file test-file))
         (t (message "No test file found (tried %s and %s)" spec-file test-file)))))
     
     (t (message "Not in app/, spec/, or test/ directory")))))
