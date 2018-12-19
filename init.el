;; -*- lexical-binding: t -*-

(server-start)

(setq inhibit-startup-message t
      message-log-max t
      load-prefer-newer t
      column-number-mode t)

(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(require 'cl-lib)
(require 'package)

(let ((melpa '("melpa" . "https://melpa.org/packages/"))
      (emacs-pe '(("emacs-pe" . "https://emacs-pe.github.io/packages")))
      (org-mode '("org" . "https://orgmode.org/elpa/")))
  (add-to-list 'package-archives melpa t)
  ;; (add-to-list 'package-archives emacs-pe t)
  (add-to-list 'package-archives org-mode t))

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
      create-lockfiles nil
      uniquify-buffer-name-style 'forward)

(set-face-attribute 'default nil :height 150)

(use-package minions
  :ensure t
  :config (minions-mode 1))

(defun ivy-format-function-cool-arrow (cands)
  "Transform CANDS into a string for minibuffer."
  (ivy--format-function-generic
   (lambda (str)
     (concat "↣ " (ivy--add-face str 'ivy-current-match)))
   (lambda (str)
     (concat "  " str))
   cands
   "\n"))

(use-package ido
  :init (progn
          (ido-mode t)
          (ido-everywhere t)
          (add-to-list 'ido-ignore-files "\\.DS_Store"))
  :config (setq ido-enable-flex-matching t
                ido-create-new-buffer 'always
                ido-use-virtual-buffers nil)
  :bind (:map ido-file-completion-map
              (("C-w" . ido-delete-backward-updir))))

(use-package ido-vertical-mode
  :ensure t
  :after (ido)
  :init (ido-vertical-mode t))

;; (use-package ido-better-flex
;;   :load-path "site-lisp/ido-better-flex"
;;   :after (ido)
;;   :config (ido-better-flex/enable))

;; TODO: Research if I can bookmark window-configurations or views
(use-package swiper
  :ensure t
  :bind (("C-c o" . swiper)))

(use-package counsel
  :ensure t
  :after (helpful)
  :config (setq counsel-find-file-occur-use-find t
                counsel-describe-function-function #'helpful-callable
                counsel-describe-variable-function #'helpful-variable)
  :bind (("C-h f" . counsel-describe-function)
         ("C-h v" . counsel-describe-variable)
         ("C-c C-f" . counsel-git)
         ("C-c g" . counsel-git-grep)
         ("<f2> i" . counsel-info-lookup-symbol)
         ("<f2> u" . counsel-unicode-char)
         ("<f2> k" . counsel-find-library)
         ("C-x p" . counsel-git)))

(use-package helpful
  :ensure t)

(use-package find-file-in-project
  :ensure t)

(use-package counsel-css
  :ensure t
  :bind ((:map css-mode-map
               ("C-s" . counsel-css)) ))

(use-package wgrep
  :ensure t)

(use-package deadgrep
  :load-path "site-lisp/deadgrep"
  :config (defalias 'ag 'deadgrep))

(use-package info
  :bind ((:map Info-mode-map
               ("(" . Info-backward-node)
               (")" . Info-forward-node))))

(global-auto-revert-mode t)

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
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)))

(use-package eyebrowse
  :ensure t
  :init (eyebrowse-mode t))

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
  :config (password-vault+-register-secrets-file (substitute-in-file-name "$HOME/.emacs.d/passwords.el.gpg")))


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

(use-package lisp-mode)

(use-package paredit
  :ensure t
  :init
  (add-hook 'lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
  :bind ((:map paredit-mode-map
               ("C-j" . nil))))

(use-package undo-tree
  :ensure t
  :config (global-undo-tree-mode 1)
  :bind (("C-x u" . undo)))

(defun my/show-commit ()
  (interactive)
  (let ((revision (car (vc-annotate-extract-revision-at-line))))
    (magit-show-commit revision)))

(use-package vc-annotate)

(use-package magit
  :ensure t
  :after (vc-annotate)
  :bind (("C-c s" . 'magit-status)
         (:map vc-annotate-mode-map
               ("<return>" . my/show-commit)))
  :config (setq magit-display-buffer-function
                'magit-display-buffer-fullframe-status-topleft-v1))

(use-package magithub
  :ensure t
  :config (magithub-feature-autoinject t)
  (magithub-feature-autoinject '(commit-browse status-checks-header completion)))

(dolist (hook '(lisp-interaction-mode-hook emacs-lisp-mode-hook))
  (add-hook hook (lambda ()
                   (setq tab-always-indent 'complete))))

(use-package eldoc
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

(use-package exotica-theme
  :ensure t
  :config (load-theme 'exotica t))

;; (use-package cyberpunk-theme
;;   :ensure t
;;   :config (load-theme 'cyberpunk t))

;; (use-package darktooth-theme
;;   :ensure t
;;   :config (load-theme 'darktooth t))

;; (use-package lush-theme
;;   :ensure t
;;   :config (load-theme 'lush t))

;; Cool green, poor typography choices
;; (use-package ahungry-theme
;;   :ensure t
;;   :config (progn (setq ahungry-theme-font-settings nil)
;;                  (load-theme 'ahungry t)))

;; Cool Purpule
;; (use-package color-theme-sanityinc-tomorrow
;;  :ensure t
;;  :config (load-theme 'sanityinc-tomorrow-eighties))

(use-package hardhat
  :ensure t
  :init (global-hardhat-mode 1))

(use-package moody
  :ensure t
  :config
  (setq x-underline-at-descent-line t)
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

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package org
  :bind (("C-c a" . 'org-agenda))
  :config (setq org-catch-invisible-edits 'show))

(use-package mu4e
  :load-path "/usr/local/share/emacs/site-lisp/mu4e/"
  :config (setq mu4e-maildir "~/.mail/gmail"
                mu4e-sent-folder "/Sent Mail"
                mu4e-trash-folder "/Trash"
                mu4e-get-mail-command "mbsync -a"))

(use-package mu4e-alert
  :ensure t
  :config (progn
            (mu4e-alert-set-default-style 'libnotify)
            (mu4e-alert-enable-mode-line-display)))

(use-package org-mu4e
  :after (org mu4e)
  :bind ((:map mu4e-headers-mode-map
               ("C-c c" . org-mu4e-store-and-capture)
         (:map mu4e-view-mode-map
               ("C-c c" . org-mu4e-store-and-capture)))))

(use-package dired
  :config (progn
            (put 'dired-find-alternate-file 'disabled nil)
            (add-hook 'dired-mode-hook 'dired-hide-details-mode)
            (setq dired-dwim-target t)))

(use-package dired-x
  :after (dired))

(use-package ibuffer-vc
  :ensure t
  :init
  (add-hook 'ibuffer-hook
    (lambda ()
      (ibuffer-vc-set-filter-groups-by-vc-root)
      (unless (eq ibuffer-sorting-mode 'alphabetic)
        (ibuffer-do-sort-by-alphabetic)))))

(use-package subword-mode
  :hook ((js2-mode . subword-mode)))

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

(defun my/call-in-rspec-mode (fn)
  (lambda (orig-fn &rest args)
    (if (eq major-mode 'rspec-compilation-mode)
      	(apply fn orig-fn args)
      (apply orig-fn args))))

(defun my/maybe-inject-proccess-environment (orig-fun &rest args)
  (chruby-use-corresponding)
  (when-let ((default-directory (locate-dominating-file default-directory ".git/"))
             (process-environment (append (my/read-env-file ".env") process-environment)))
    (apply orig-fun args)))

(use-package robe
  :ensure t
  :hook (ruby-mode . robe-mode)
  :init (advice-add 'inf-ruby-console-auto :around #'my/maybe-inject-proccess-environment))

(use-package rspec-mode
  :ensure t
  :hook ((ruby-mode . rspec-mode)
         (dired-mode . rspec-dired-mode))
  :init (progn (advice-add 'rspec-compile :around #'my/maybe-inject-proccess-environment)
               (advice-add 'recompile :around (my/call-in-rspec-mode  #'my/maybe-inject-proccess-environment))))

(use-package web-mode
  :ensure t
  :mode "\\.erb\\'")

(use-package yaml-mode
  :ensure t)

(use-package sly
  :load-path "site-lisp/sly"
  :config (setq inferior-lisp-program "/usr/local/bin/sbcl"
                sly-lisp-implementations '((sbcl ("/usr/local/bin/sbcl")))))

(use-package sly-macrostep
  :ensure t)

(use-package js2-mode
  :ensure t
  :config (setq js2-basic-offset 2)
  :mode ("\\.js\\'")
  :hook ((js2-mode . (lambda () (setq mode-name "JS2")))))

(use-package prettier-js
  :ensure t
  :config (setq prettier-js-command "npx"
                prettier-js-args '("prettier"))
  :hook ((js2-mode . prettier-js-mode)
         (rjsx-mode . prettier-js-mode)))

(use-package flymake
  ;; :hook ((ruby-mode . flymake-mode)) ; Disable flymake unless the ruby is inside a project
  :bind ((:map flymake-mode-map
               ("M-n" . 'flymake-goto-next-error)
               ("M-p" . 'flymake-goto-prev-error))))

(use-package eglot
  :ensure t
  :config (add-to-list 'eglot-server-programs
                       '((js-mode js2-mode rjsx-mode typescript-mode) . ("javascript-typescript-stdio"))))

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
  (setq json-reformat:indent-width 2))

(use-package rjsx-mode
  :ensure t
  :mode "\\.jsx\\'"
  :mode "\\.js\\'")

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

(use-package compile
  :config (assq-delete-all 'compilation-in-progress minor-mode-alist))

(use-package compile-eslint
  :load-path "site-lisp/compile-eslint"
  :init (push 'eslint compilation-error-regexp-alist))

(use-package sql
  ;; :bind (("C-c a s" . sql-connect))
  :config
  (progn
    (plist-put (cdr (assq 'postgres sql-product-alist)) :prompt-regexp "^[[:alnum:]_-]*=[#>] ")
    (plist-put (cdr (assq 'postgres sql-product-alist)) :prompt-cont-regexp "^[[:alnum:]_-]*[-(][#>] ")
    (defun marsam-sql-set-variables ()
      "Set variables on switch to sqli buffer."
      (cl-case sql-product
        (mysql    (setq sql-input-ring-separator "\n"
                        sql-input-ring-file-name (concat (file-remote-p default-directory) "~/.mysql_history")))
        (sqlite   (setq sql-input-ring-separator "\n"
                        sql-input-ring-file-name (concat (file-remote-p default-directory) "~/.sqlite_history")))
        (postgres (setq sql-input-ring-separator "\n"
                        sql-input-ring-file-name (concat (file-remote-p default-directory) "~/.psql_history")))))

    (add-hook 'sql-interactive-mode-hook #'marsam-sql-set-variables)))

(use-package sql-indent
  :ensure t
  :init (add-hook 'sql-mode-hook 'sqlind-minor-mode))

(use-package docean
  :ensure t)

(use-package rainbow-mode
  :ensure t
  :config (setq rainbow-x-colors nil)
  :hook ((css-mode . rainbow-mode)))

(use-package rainbow-delimiters
  :ensure t
  :hook ((prog-mode . rainbow-delimiters-mode)))

(use-package multiple-cursors
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

(use-package ri
  :load-path "site-lisp/ri.el")

(use-package pivotal-tracker
  :load-path "site-lisp/pivotal-tracker")

(use-package sauron
  :ensure t
  :config (setq sauron-separate-frame nil
                sauron-sticky-frame nil
                sauron-dbus-cookie t
                sauron-modules '(sauron-org sauron-notifications sauron-mu4e sauron-dbus)))

;; TODO: Configure as dedicated windows: sauron, sly-mrepl and
;; compilation modes.
(use-package shackle
  :ensure t
  :config (setq shackle-rules '((compilation-mode :noselect t))
                ;; shackle-default-rule '(:select t)
                ))

(use-package window-purpose
  :ensure t)

(setq project-config-file "~/.emacs.d/project-config.el")
(load project-config-file 'noerror)

(put 'narrow-to-page 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
