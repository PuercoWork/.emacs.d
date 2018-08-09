;; -*- lexical-binding: t -*-

(server-start)

(setq inhibit-startup-message t
      message-log-max t
      load-prefer-newer t)

(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(require 'cl-lib)
(require 'package)

(let ((melpa '("melpa" . "https://melpa.org/packages/"))
      (emacs-pe '(("emacs-pe" . "https://emacs-pe.github.io/packages"))))
  (add-to-list 'package-archives melpa t)
  ;; (add-to-list 'package-archives emacs-pe t)
  )

(package-initialize)

;; misc
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(require 'use-package)

(set-default 'indent-tabs-mode nil)
(set-default 'indicate-empty-lines t)
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'list-buffers 'ibuffer)
(delete-selection-mode 1)


(prefer-coding-system 'utf-8)

(setq backup-directory-alist `(("." . ,(expand-file-name
					(concat user-emacs-directory "backups"))))
      make-backup-files nil
      vc-make-backup-files nil
      auto-save-default nil
      uniquify-buffer-name-style 'forward)

;; OS X specific

(setq mac-command-modifier 'meta
      mac-option-modifier nil
      ns-function-modifier 'hyper
      mac-right-option-modifier 'ctrl
      locate-command "mdfind")

(set-face-attribute 'default nil :height 160)

(use-package diminish
  :ensure t)

(defun ivy-format-function-cool-arrow (cands)
  "Transform CANDS into a string for minibuffer."
  (ivy--format-function-generic
   (lambda (str)
     (concat "↣ " (ivy--add-face str 'ivy-current-match)))
   (lambda (str)
     (concat "  " str))
   cands
   "\n"))

(use-package ivy
  :ensure t
  :config
  (setq ivy-format-function 'ivy-format-function-cool-arrow
        ivy-use-virtual-buffers t
        ivy-dispay-format 'fancy)
  :bind (("C-c C-r" . ivy-resume)
         ("C-x b" . ivy-switch-buffer)
         ("C-c v" . ivy-push-view)
         ("C-c V" . ivy-pop-view)
         (:map ivy-minibuffer-map
               ("C-s" . ivy-next-line)
               ("C-r" . ivy-previous-line))))
;; ivy-pop-view ?
;; TODO: Research if I can bookmark window-configurations or views

(use-package swiper
  :ensure t
  :bind (("C-c o" . swiper)))

(use-package counsel
  :ensure t
  :config (setq counsel-find-file-occur-use-find t)
  :bind (("C-h f" . counsel-describe-function)
         ("C-h v" . counsel-describe-variable)
         ("C-x C-f" . counsel-find-file)
         ("C-c C-f" . counsel-git)
         ("C-c g" . counsel-git-grep)
         ("<f2> i" . counsel-info-lookup-symbol)
         ("<f2> u" . counsel-unicode-char)
         ("<f2> k" . counsel-find-library)))

(use-package find-file-in-project
  :ensure t
  :bind (("C-x p" . 'find-file-in-project)))

(use-package counsel-css
  :ensure t
  :bind ((:map css-mode-map
               ("C-s" . counsel-css)) ))

(use-package wgrep
  :ensure t)

(use-package deadgrep
  :load-path "site-lisp/deadgrep"
  :config (defalias 'ag 'deadgrep))

;; Configure occur-mode to save the files after exiting editing mode.
;; (use-package occur
;;   :config (progn
;;             (next-error-follow-minor-mode &optional ARG))
;;   :bind (("C-c o" . occur)))

(use-package ace-window
  :ensure t
  :bind (("M-o" . 'other-window)
         ("M-0" . 'delete-window)
         ("M-1" . 'delete-other-windows)
         ("M-2" . 'split-window-vertically)
         ("M-3" . 'split-window-right)
         ("C-x o" . 'ace-window)
         ("C-x t" . (lambda () (interactive) (ace-window 4)))))

(use-package expand-region
  :ensure t
  :bind (("C-=" . 'er/expand-region)))

;; Spellcheck
(use-package flyspell
  :diminish 'flyspell-mode
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)))

(use-package eyebrowse
  :ensure t)

(use-package paren
  :init (show-paren-mode 1))

(define-key isearch-mode-map [return]
  #'isearch-exit-other-end)
(defun isearch-exit-other-end ()
  (interactive)
  (isearch-exit)
  (goto-char isearch-other-end))

(use-package password-vault+
  :load-path "site-lisp/password-vault+"
  :init (password-vault+-register-secrets-file (substitute-in-file-name "$HOME/.emacs.d/passwords.el.gpg")))


(use-package docker-tramp
  :ensure t)

(use-package tablist
  :ensure t)

(use-package docker
  :load-path "site-lisp")

(setq smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-local-domain "gmail.com"
      smtpmail-sendto-domain "gmail.com"
      smtpmail-smtp-user "javier@able.co"
      smtpmail-smtp-service 465
      ;; smtpmail-smtp-service 587
      smtpmail-stream-type 'ssl
      smtpmail-debug-info t
      send-mail-function 'smtpmail-send-it)

(use-package lisp-mode
  :mode (".*?fino-editor.*?\\.fs" . lisp-mode))

(use-package paredit
  :ensure t
  :diminish paredit-mode
  :init
  (add-hook 'lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
  :bind ((:map paredit-mode-map
               ("C-j" . nil))))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config (global-undo-tree-mode 1)
  :bind (("C-x u" . 'undo)))

(use-package magit
  :ensure t
  :bind (("C-c s" . 'magit-status))
  :diminish auto-revert-mode
  :config
  (setq magit-display-buffer-function
        'magit-display-buffer-fullframe-status-topleft-v1))

(use-package magithub
  :ensure t
  :config (magithub-feature-autoinject t))

(add-hook 'lisp-interaction-mode-hook (lambda ()
                                        (setq tab-always-indent 'complete)))
(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (setq tab-always-indent 'complete)))

(use-package eldoc
  :diminish eldoc-mode
  :init (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode))

(use-package macrostep
  :ensure t
  :bind (:map emacs-lisp-mode-map
        ("C-c M-e" . macrostep-expand)
        :map lisp-interaction-mode-map
        ("C-c M-e" . macrostep-expand)))


(defun ppop-notmuch-unread-mail ()
  (with-temp-buffer
    (if (zerop (call-process "notmuch" nil (list t nil) nil "count" "tag:unread"))
        (unless (bobp)
          (goto-char (point-min))
          (buffer-substring-no-properties (point) (line-end-position)))
      (message "PPOP-UNREAD-MAIL: %S" (buffer-string)))))

;; (use-package doom-themes
;;   :ensure t
;;   :config
;;   (load-theme 'doom-nova))

(use-package color-theme-sanityinc-tomorrow
 :ensure t
 :config (load-theme 'sanityinc-tomorrow-eighties))

(use-package hardhat
  :ensure t
  :init (global-hardhat-mode 1)
  :diminish global-hardhat-mode)

(use-package moody
  :ensure t
  :config
  (setq x-underline-at-descent-line t)
  (setq moody-slant-function #'moody-slant-apple-rgb)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(use-package xterm-color
  :ensure t
  :init
  (progn
    (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter)
    (setq comint-output-filter-functions
          (remove 'ansi-color-process-output comint-output-filter-functions))))

;; (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(use-package org
  :bind
  (("C-c a" . 'org-agenda))
  :config (setq org-catch-invisible-edits 'show))

(use-package mu4e
  :load-path "/usr/local/Cellar/mu/1.0/share/emacs/site-lisp/mu/mu4e/"
  :config (setq mu4e-maildir "~/.mail/gmail"
                mu4e-sent-folder "/Sent Mail"
                mu4e-trash-folder "/Trash"
                mu4e-get-mail-command "mbsync -a"))

(use-package mu4e-alert
  :ensure t
  :config (progn
            (mu4e-alert-set-default-style 'osx-notifier)
            (mu4e-alert-enable-mode-line-display)))

(use-package dired
  :config (progn
            (put 'dired-find-alternate-file 'disabled nil)
            (add-hook 'dired-mode-hook 'dired-hide-details-mode)))

(use-package dired-x)

(use-package ibuffer-vc
  :ensure t
  :init
  (add-hook 'ibuffer-hook
    (lambda ()
      (ibuffer-vc-set-filter-groups-by-vc-root)
      (unless (eq ibuffer-sorting-mode 'alphabetic)
        (ibuffer-do-sort-by-alphabetic)))))

(use-package nix-mode
  :load-path "site-lisp/nix-mode")



(use-package markdown-mode
  :ensure t
  :hook (markdown-mode . (lambda ()
                           (setq fill-column 79))))

(use-package chruby
  :ensure t)

(defun my/read-env-file (env-file)
  (with-current-buffer (find-file-noselect env-file)
    (cl-loop for line in (split-string (buffer-string) "\n")
             when (string-match "^\\(.+[^[:space:]]\\)[[:space:]]*=[[:space:]]*\\(.+\\)" line)
             collect (format "%s=%s"
                             (match-string-no-properties 1 line)
                             (match-string-no-properties 2 line)))))

(defun my/maybe-inject-proccess-environment (orig-fun &rest args)
  (chruby-use-corresponding)
  (when-let ((default-directory (locate-dominating-file default-directory ".git/"))
             (process-environment (append (my/read-env-file ".env") process-environment)))
    (apply orig-fun args)))

(use-package robe
  :ensure t
  :diminish robe-mode
  :hook (ruby-mode . robe-mode)
  :init (advice-add 'inf-ruby-console-auto :around #'my/maybe-inject-proccess-environment))

(use-package rspec-mode
  :ensure t
  :hook ((ruby-mode . rspec-mode)
         (dired-mode . rspec-dired-mode))
  :diminish rspec-mode
  :init (progn (advice-add 'rspec-compile :around #'my/maybe-inject-proccess-environment)
               (advice-add 'recompile :around #'my/maybe-inject-proccess-environment)))

(use-package web-mode
  :ensure t
  :mode "\\.erb\\'")

(use-package yaml-mode
  :ensure t)

(use-package sly
  :ensure t
  :config (setq sly-lisp-implementations '((sbcl ("/usr/local/bin/sbcl")))))

(use-package js2-mode
  :ensure t
  :diminish subword-mode
  :config (setq js2-basic-offset 2)
  :mode ("\\.js\\'")
  :hook ((js2-mode . (lambda () (setq mode-name "JS2")))))

(use-package prettier-js
  :ensure t
  :diminish prettier-js-mode
  :config (setq prettier-js-command "npx"
                prettier-js-args '("prettier"))
  :hook ((js2-mode . prettier-js-mode)
         (rjsx-mode . prettier-js-mode)))

(use-package flymake
  :hook ((ruby-mode . flymake-mode))
  :bind ((:map flymake-mode-map
               ("M-n" . 'flymake-goto-next-error)
               ("M-p" . 'flymake-goto-prev-error))))

(use-package eglot
  :ensure t
  :hook ((js2-mode . eglot-ensure)))

;; Not sure if necessary. C-x r w seems to be enough for my needs.
(use-package eyebrowse
  :ensure t
  :bind (("C-c 1" . eyebrowse-switch-to-window-config-1)
         ("C-c 2" . eyebrowse-switch-to-window-config-2)
         ("C-c 3" . eyebrowse-switch-to-window-config-3)
         ("C-c 4" . eyebrowse-switch-to-window-config-4)
         ("C-c 5" . eyebrowse-switch-to-window-config-5)
         ("C-c 6" . eyebrowse-switch-to-window-config-6)
         ("C-c 7" . eyebrowse-switch-to-window-config-7)))

(use-package json-mode
  :ensure t
  :config
  (setq json-reformat:indent-width 2)
  :mode (".*?fino-editor.*?\\.ast" . json-mode))

(use-package rjsx-mode
  :ensure t
  :mode "\\.jsx\\'")

(use-package typescript-mode
  :ensure t)

(use-package cider
  :ensure t)

(use-package epresent
  :ensure t)

(use-package restclient
  :ensure t)

(use-package xterm-color
  :ensure t
  :init (add-hook 'compilation-start-hook
          (lambda (proc)
            ;; We need to differentiate between compilation-mode buffers
            ;; and running as part of comint (which at this point we assume
            ;; has been configured separately for xterm-color)
            (when (eq (process-filter proc) 'compilation-filter)
              ;; This is a process associated with a compilation-mode buffer.
              ;; We may call `xterm-color-filter' before its own filter function.
              (set-process-filter
               proc
               (lambda (proc string)
                 (funcall 'compilation-filter proc
                          (xterm-color-filter string))))))))

(use-package honcho
  :ensure t)

(honcho-define-service fino-editor
  :command ("npm" "run" "start")
  :cwd "/Users/puercopop/Projects/fino-editor/")

(use-package subword-mode
  :diminish subword-mode
  :hook ((js2-mode . subword-mode)))


(use-package compile-eslint
  :load-path "site-lisp/compile-eslint"
  :init (push 'eslint compilation-error-regexp-alist))
(use-package docean
  :ensure t)
(put 'narrow-to-page 'disabled nil)
