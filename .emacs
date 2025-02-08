(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(use-package xterm-color projectile magit which-key exec-path-from-shell vertico consult marginalia orderless flycheck treesit-auto)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; TODO: revisit corfu, lsp-mode when frames are in terminal emacs (31+)

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
(when (memq window-system '(mac ns x))
  (setenv "SHELL" "/bin/zsh")
  (exec-path-from-shell-initialize))

;; treesit
(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; flycheck
(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

;; which key
(which-key-mode)

;; pixel scroll
(pixel-scroll-precision-mode)

;; line numbers etc
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq js-indent-level 2)
(setq-default fill-column 150)
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)

;; vertico
(use-package vertico
  :init
  (vertico-mode))

;; orderless
(use-package orderless
  :init
  (setq completion-styles '(orderless partial-completion basic)
        completion-category-defaults nil
        completion-category-overrides nil))

;; marginalia
(use-package marginalia
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;; consult
(use-package consult
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

;; projectile
(require 'projectile)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(setq projectile-project-search-path '(("~/dev/" . 1) ("~/invoca/" . 1)))
(setq projectile-ignored-projects '("~/"))
(projectile-mode +1)
(global-set-key "\347f" 'consult-ripgrep)

;; pinentry/gpg
(require 'epg)
(setq epg-pinentry-mode 'loopback)
;; https://gist.github.com/bmhatfield/cc21ec0a3a2df963bffa3c1f884b676b

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
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
  backup-by-copying t    ; Don't delink hardlinks
  version-control t      ; Use version numbers on backups
  delete-old-versions t  ; Automatically delete excess backups
  kept-new-versions 20   ; how many of the newest versions to keep
  kept-old-versions 5    ; and how many of the old
  )

;; get the relative path of the current buffer
(defun copy-relative-path ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (shell-command (concat "echo -n " filename " | pbcopy"))
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))
