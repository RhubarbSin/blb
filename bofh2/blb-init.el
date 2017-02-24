(setq inhibit-startup-screen t)
(setq make-backup-files nil)
(setq enable-local-variables :all)
(setq bookmark-default-file "~/.emacs.d/.emacs.bmk")
(setq column-number-mode t)
(setq size-indication-mode t)
(setq mouse-drag-copy-region t)
(setq tab-stop-list (quote (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60)))
(setq tab-width 4)
(setq blink-cursor-blinks 0)
(setq after-save-hook (quote (executable-make-buffer-file-executable-if-script-p)))
(setq Man-notify-method 'pushy)
(setq browse-url-browser-function (quote w3m-browse-url))
(setq remote-shell-program "/bin/bash")
(setq tramp-default-proxies-alist (quote ((".*" "\\`root\\'" "/ssh:%h:"))))
(setq explicit-shell-file-name "/bin/bash")
(setq shell-file-name "bash")
(setq explicit-bash-args '("--noediting" "--login" "-i"))
(setenv "SHELL" shell-file-name)
(put 'narrow-to-region 'disabled nil)
(winner-mode 1)
(tool-bar-mode 0)
(menu-bar-mode 0)
(desktop-save-mode 1)
(savehist-mode 1)
(scroll-bar-mode -1)
(delete-selection-mode 1)
(server-start)

(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'ssm 'shell-script-mode)
(defalias 'om 'org-mode)

;; use CPerlMode instead of PerlMode
(defalias 'perl-mode 'cperl-mode)

(recentf-mode t)
(setq recentf-auto-cleanup (quote never))
(setq recentf-save-file "~/.emacs.d/.recentf")
(load-file recentf-save-file) ; why is this necessary?
(global-set-key (kbd "C-x C-b") 'buffer-menu)
(global-set-key (kbd "C-c r f") 'recentf-open-files) ; open recent files
(global-set-key (kbd "C-x r v") 'register-list) ; open register list
(add-hook 'Man-mode-hook (lambda () (local-set-key "Q" 'kill-this-buffer)))

;; keybindings for switching windows
(global-set-key (kbd "<f9>") 'windmove-left)
(global-set-key (kbd "<f10>") 'windmove-down)
(global-set-key (kbd "<f11>") 'windmove-up)
(global-set-key (kbd "<f12>") 'windmove-right)

;; kill *Completions* windows after 60s
(add-hook 'completion-setup-hook
	  (lambda () (run-at-time 60 nil
				  (lambda () (delete-windows-on "*Completions*")))))

(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
(autoload 'csv-mode "csv-mode"
  "Major mode for editing comma-separated value files." t)

;; load my passwords
(load "blb-authinfo")

;;
;; auto-insert
;;
(setq auto-insert-directory "~/.emacs.d/auto-insert/")
(setq auto-insert-mode t)
(setq auto-insert t)
(setq auto-insert-query nil)
(setq auto-insert-alist ())
(add-hook 'find-file-hooks 'auto-insert)
;; insert mode-based default contents
(add-hook 'markdown-mode-hook
	  (lambda ()
	    (auto-insert)
	    (end-of-line)))
(add-to-list 'auto-insert-alist '(markdown-mode . "insert.markdown"))

(add-hook 'cperl-mode-hook
	  (lambda ()
        (auto-insert)
        (goto-char (point-max))))
(add-to-list 'auto-insert-alist '(cperl-mode . "insert.perl"))

(add-hook 'python-mode-hook
	  (lambda ()
        (auto-insert)
        (goto-char (point-max))))
(add-to-list 'auto-insert-alist '(python-mode . "insert.python"))

(add-hook 'sh-mode-hook
	  (lambda ()
        (auto-insert)
        (goto-char (point-max))))
(add-to-list 'auto-insert-alist '(sh-mode . "insert.bash"))

;;
;; MELPA
;;
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(package-initialize)

;;
;; line numbering
;;
(add-hook 'cperl-mode-hook (lambda() (linum-mode t)))
(add-hook 'php-mode-hook (lambda() (linum-mode t)))
(add-hook 'python-mode-hook (lambda() (linum-mode t)))

;;
;; w3m
;;
(add-hook 'w3m-load-hook (lambda () (local-set-key (kbd "C-c b") 'w3m-browse-url)))
(setq w3m-async-exec nil)

;;
;; Workgroups (window configuration)
;;
;; (require 'workgroups)
;; (workgroups-mode 1)
;; (wg-load "~/.emacs.d/workgroups")


;; C-d twice to exit shell and kill buffer
(defun comint-delchar-or-eof-or-kill-buffer (arg)
  (interactive "p")
  (if (null (get-buffer-process (current-buffer)))
      (kill-buffer)
    (comint-delchar-or-maybe-eof arg)))

(add-hook 'shell-mode-hook
          (lambda ()
            (define-key shell-mode-map
              (kbd "C-d") 'comint-delchar-or-eof-or-kill-buffer)
            (define-key shell-mode-map
              (kbd "C-c n") 'rename-buffer)))

(defun dos-to-unix ()
  "Cut all visible ^M from the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\r" nil t)
      (replace-match ""))))

(defun unix-to-dos ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\n" nil t)
      (replace-match "\r\n"))))

(defun toggle-current-window-dedication ()
 (interactive)
 (let* ((window (selected-window))
        (dedicated (window-dedicated-p window)))
   (set-window-dedicated-p window (not dedicated))
   (message "Window %sdedicated to %s"
            (if dedicated "no longer " "")
            (buffer-name))))
(global-set-key (kbd "C-c d") 'toggle-current-window-dedication)

(defun blb-copy-region-as-kill-yank ()
  "Save to the kill ring and immediately yank"
  (interactive)
  (copy-region-as-kill (region-beginning) (region-end))
  (yank))
(global-set-key (kbd "C-M-y") 'blb-copy-region-as-kill-yank) ; copy and paste

(defun blb-scroll-down ()
  "Scroll down without moving point"
  (interactive)
  (scroll-up 1))

(defun blb-scroll-up ()
  "Scroll up without moving point"
  (interactive)
  (scroll-down 1))

(global-set-key (kbd "M-n") 'blb-scroll-down)
(global-set-key (kbd "M-p") 'blb-scroll-up)

;;
;; Term mode
;;
(defun blb-term-exec-hook ()
  (let* ((buff (current-buffer))
         (proc (get-buffer-process buff)))
    (set-process-sentinel
     proc
     `(lambda (process event)
        (if (string= event "finished\n")
            (kill-buffer ,buff))))))

(add-hook 'term-exec-hook 'blb-term-exec-hook)(eval-after-load "term"
  '(define-key term-raw-map (kbd "C-c C-y") 'term-paste))

(defadvice term-sentinel (around blb-advice-term-sentinel (proc msg))
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))
(ad-activate 'term-sentinel)

(defvar blb-term-shell "/bin/bash")
(defadvice ansi-term (before force-bash)
  (interactive (list blb-term-shell)))
(ad-activate 'ansi-term)

(defun blb-term-use-utf8 ()
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
(add-hook 'term-exec-hook 'blb-term-use-utf8)

;;
;; Org mode
;;

(require 'org-mouse)
;; (setq org-cycle-include-plain-lists t)
(setq org-table-export-default-format "orgtbl-to-csv")
(setq org-link-mailto-program (quote (compose-mail "%a" "%s")))
(setq org-link-frame-setup (quote ((vm . vm-visit-folder)
                                   (gnus . org-gnus-no-new-news)
                                   (file . find-file))))
; Use the current window for C-c ' source editing
(setq org-src-window-setup 'current-window)
(setq org-src-preserve-indentation nil)
(setq org-edit-src-content-indentation 0)
(require 'org-crypt)
; Encrypt all entries before saving
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
; (setq org-crypt-disable-auto-save encrypt)
; GPG key to use for encryption
(setq org-crypt-key "656F0F02")
; Load exporters
(eval-after-load "org" '(require 'ox-odt nil t))
(eval-after-load "org" '(require 'ox-mediawiki))
(eval-after-load "org" '(require 'ox-confluence))
(setq org-edit-src-content-indentation 0)
(setq org-export-with-sub-superscripts nil)
(setq org-footnote-auto-adjust t)
(setq org-src-fontify-natively t)
(setq org-startup-folded (quote content))
(setq org-confirm-babel-evaluate nil)
(setq org-ditaa-jar-path "/usr/share/ditaa/ditaa.jar")
(global-set-key (kbd "C-c l") 'org-store-link)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (ditaa . t)
   ))
(add-hook 'org-babel-after-execute-hook
	  (lambda () (org-redisplay-inline-images)))

;; start checkbox list in org mode with C-c c
(defun blb-begin-checkbox-list ()
  "Start an org-mode checkbox list."
  (interactive)
  (org-return-indent)
  (insert "- [ ] "))
(add-hook 'org-mode-hook (lambda () (local-set-key (kbd "C-c c") 'blb-begin-checkbox-list)))

;;
;; Dired
;;
(setq dired-filetype-execute-regexp "^  .*\\.\\(exe\\|EXE\\|bat\\|BAT\\|msi\\|MSI\\|\\|sh\\|run\\|reg\\|REG\\|COM\\|\\|vbx\\|VBX\\|bin\\|xpi\\|bundle\\)$")
(setq dired-filetype-source-regexp "^  .*\\.\\(c\\|cpp\\|java\\|JAVA\\|C\\|php\\|h\\|rb\\|pl\\|css\\|el\\|lua\\|py\\)$")
(setq dired-listing-switches "-alh")
(setq dired-load-hook (quote ((lambda nil (load "dired-x")) (lambda nil (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))) (lambda nil (setq-default dired-omit-files-p t)))))
(setq dired-no-confirm (quote (chgrp chmod chown copy delete hardlink move symlink touch)))
(put 'dired-find-alternate-file 'disabled nil)
(eval-after-load 'dired '(progn (require 'dired-filetype-face)))
(require 'dired-tar)

;;
;; Magit
;;
(global-set-key (kbd "C-c g") 'magit-status)
(add-hook 'magit-mode-hook (lambda () (local-set-key (kbd "T") 'magit-annotated-tag)))

;;
;; bitlbee for AIM
;;
(require 'bitlbee)
(defun bitlbee-identify ()
  "If we're on the bitlbee server, send the identify command to the &bitlbee channel."
  (when (and (string= "localhost" erc-session-server)
             (string= "&bitlbee" (buffer-name)))
    (erc-message "PRIVMSG" (format "%s identify %s"
                                   (erc-default-target)
                                   blb-bitlbee-password))))
(defun bitlbee-dedicate-window (server nick)
  "If we're on the bitlbee server, dedicate this window to the &bitlbee buffer."
  (if (eq erc-server "localhost") (set-window-dedicated-p (selected-window) t)))
;; (bitlbee-start)

;;
;; erc
;;
(require 'tls) ;; redundant when vm is already loaded
(setq erc-hide-list '("JOIN" "PART" "QUIT"))
(setq erc-truncate-buffer-on-save t)
(defvar erc-insert-post-hook)
(add-hook 'erc-insert-post-hook
	  'erc-truncate-buffer)
(setq erc-truncate-buffer-on-save t)

;;
;; colors: couldn't get these to work via Customize
;;
(setq erc-keywords '((".*Online.*" (:foreground "green"))
                     (".*Busy" (:foreground "lightpink"))
                     (".*Away" (:foreground "hotpink"))
                     (".*Idle" (:foreground "orange"))
                     ))

;;
;; use notify-send for ERC's change notifications
;;
(defvar blb-modified-channels-length 0
  "Last recorded length of `erc-modified-channels-alist'.
This is updated each time `blb-erc-notify-send' gets called from
`erc-track-list-changed-hook'.")

(defun blb-erc-notify-send ()
  "Use notify-send for ERC track change notifications."
  (let ((modified-channels-length (length erc-modified-channels-alist)))
    (when (> modified-channels-length blb-modified-channels-length)
      (let ((msg (format "New messages in %s"
                         (mapconcat (lambda (pair)
                                      (buffer-name (car pair)))
                                    erc-modified-channels-alist
                                    ", "))))
        (notify-send "ERC" msg)
        (message "%s" msg)))
    (setq blb-modified-channels-length modified-channels-length)))

;;
;; play a sound upon receipt of a message
;;
(defun blb-notify-new-im (str)
  "Send notifications for new instant message."
  ;; Don't play any audio for messages from root or in regular IRC channels
  (unless (or (string-match "<root>" str) (string-match "#" (buffer-name)))
    ;; Play an audio sound when someone types a message
    (when (string-match "<[a-zA-Z0-9]+>" str)
      (notify-send "ERC" "New IM"))))
      ;; (blb-play-sound-file "/usr/share/sounds/freedesktop/stereo/message-new-instant.oga"))))
(add-hook 'erc-insert-pre-hook 'blb-notify-new-im)

;; by Nikolaj Schumacher, 2008-10-20. Released under GPL.
(provide 'extend-selection)
(defun semnav-up (arg)
  (interactive "p")
  (when (nth 3 (syntax-ppss))
    (if (> arg 0)
        (progn
          (skip-syntax-forward "^\"")
          (goto-char (1+ (point)))
          (decf arg))
      (skip-syntax-backward "^\"")
      (goto-char (1- (point)))
      (incf arg)))
  (up-list arg))

;; by Nikolaj Schumacher, 2008-10-20. Released under GPL.
(defun extend-selection (arg &optional incremental)
  "Select the current word.
Subsequent calls expands the selection to larger semantic unit."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     (or (use-region-p)
                         (eq last-command this-command))))
  (if incremental
      (progn
        (semnav-up (- arg))
        (forward-sexp)
        (mark-sexp -1))
    (if (> arg 1)
        (extend-selection (1- arg) t)
      (if (looking-at "\\=\\(\\s_\\|\\sw\\)*\\_>")
          (goto-char (match-end 0))
        (unless (memq (char-before) '(?\) ?\"))
          (forward-sexp)))
      (mark-sexp -1))))

(global-set-key (kbd "M-8") 'extend-selection)

;;
;; connect to AIM via bitlbee
;;
(defun aim ()
  "Log into bitlbee with erc for AIM."
  (interactive)
  (if (not (eq major-mode "erc-mode"))
      (select-frame-set-input-focus (make-frame)))
  (set-window-dedicated-p (selected-window) t)
  (erc :server "localhost" :port "6667" :nick "blb")
  (delete-other-windows))

(defun freenode ()
  "Log into Freenode with erc."
  (interactive)
  (erc-tls :server "chat.freenode.net" :port "6697" :nick "RhubarbSin" :password blb-freenode-password))

(defun soylentnews ()
  "Log into SoylentNews IRC with erc."
  (interactive)
  (erc-tls :server "irc.soylentnews.org" :port "6697" :nick "RhubarbSin" :full-name "Rhubarb Sin" :password blb-soylentnews-password))

;;
;; Helm
;;
(setq helm-split-window-in-side-p nil)  ; default
(setq helm-full-frame nil)  ; default
(setq helm-split-window-default-side 'same)
(setq multi-term-kill-buffer t)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x M-b") 'helm-mini)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
;; (define-key helm-map (kbd "C-z")  'helm-select-action)
(defalias 'man 'helm-man-woman)
(require 'helm-multi-term)
(setq helm-sources helm-source-multi-term)
(setq multi-term-default-init-string "export TERM=ansi")
(defun ssh () (interactive) (helm :sources '(helm-source-multi-term)))

;;
;; Atlassian
;;
(setq confluence-url "https://confluence.medhokapps.com/rpc/xmlrpc")
(setq confluence-default-space-alist (list (cons confluence-url "ITOPS")))
(setq jira-url "https://jira.medhokapps.com/rpc/xmlrpc")

(setq blb-confluence-page-sig "~/doc/confluence-rd-snippet.xml")
(defun blb-confluence-org-get-export ()
  (interactive)
  (confluence-toggle-page-content-type)
  (let ((export-buffer (get-buffer "*org CONFLUENCE Export*")))
    (buffer-swap-text export-buffer)
    (kill-buffer export-buffer))
  (confluence-toggle-page-content-type)
  (save-excursion
    (goto-char (point-max))
    (beginning-of-line)
    (insert-file-contents blb-confluence-page-sig)))

;;
;; remember
;;
;; (require 'remember)
;; (require 'org-remember)
;; (org-remember-insinuate)
;; (setq org-directory "~/doc/")
;; (setq org-default-notes-file "~/.emacs.d/notes")
;; (define-key global-map "\C-cr" 'org-remember)

;;
;; Others
;;
(require 'cl)
(load-file "~/.emacs.d/make-password.el")
(load-file "~/.emacs.d/buff-menu.el")
(require 'buff-menu+)
(add-to-list 'same-window-buffer-names "*Buffer List*")
(setq sunshine-location "Tampa, FL")
(load-file "~/.emacs.d/host-list.el")
(load-file "~/.emacs.d/nyan-mode-master/nyan-mode.el")
;; (nyan-mode)
(elpy-enable)

;;
;; mildly modified Zenburn theme
;;
;; (require 'blb-zenburn-theme)
;;
;; use latest Zenburn for Helm support
;;
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'zenburn t)

;; shouldn't use custom-set-faces here
(custom-set-faces
 '(Buffer-menu-mode-default ((t (:inherit special-mode-default :background "#000000"))) t)
 '(buffer-menu-buffer-name ((t (:foreground "#00bfff"))))
 '(buffer-menu-delete-mark ((t (:background "#8b0000" :foreground "Aquamarine"))))
 '(buffer-menu-directory-buffer ((t (:foreground "#afeeee"))))
 '(buffer-menu-file-name ((t (:foreground "#ffb5c5"))))
 '(buffer-menu-flagged-buffer ((t (:foreground "#ffb5c5"))))
 '(buffer-menu-mode ((t (:foreground "#00cd66"))))
 '(buffer-menu-mode-line-marked ((t (:foreground "GreenYellow"))))
 '(buffer-menu-read-only-mark ((t (:foreground "#eedd82"))))
 '(buffer-menu-size ((t (:foreground "#cdc0b0"))))
 '(buffer-menu-star-buffer ((t (:foreground "#87cefa"))))
 '(buffer-menu-time ((t (:foreground "#eedd82"))))
 '(erc-error-face ((t (:foreground "orangered"))))
 '(erc-input-face ((t (:foreground "yellow1"))))
 '(erc-my-nick-face ((t (:foreground "yellow4" :weight bold))))
 '(erc-notice-face ((((class color) (min-colors 88)) (:foreground "CornflowerBlue" :weight bold)))))
