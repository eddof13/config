(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(xterm-color undo-tree ivy projectile magit guru-mode which-key zenburn-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; initialization
(menu-bar-mode -1)
(setq inhibit-startup-message t) 
(setq initial-scratch-message nil)

;; theme
(load-theme 'zenburn t)

;; which key
(which-key-mode)

;; guru mode
(add-hook 'prog-mode-hook 'guru-mode)

;; ivy
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")

;; projectile
(require 'projectile)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(setq projectile-project-search-path '(("~/dev/" . 1)))
(setq projectile-ignored-projects '("~/"))
(projectile-mode +1)

;; undo tree
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo-tree")))
(global-undo-tree-mode)

;; pinentry/gpg
(require 'epg)
(setq epg-pinentry-mode 'loopback)
;; https://gist.github.com/bmhatfield/cc21ec0a3a2df963bffa3c1f884b676b

;; git
;; https://simpleit.rocks/git/make-git-ignore-temporary-files-produced-by-emacs-and-vim-in-all-directories-globally/

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
