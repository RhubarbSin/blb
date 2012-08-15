;; $Id: blb-init.el,v 1.8 2012/08/15 15:35:39 blb Exp $

;; This file is loaded by ~/emacs.d/init.el after all of the Customize stuff.
;;
;; TODO:
;;
;; - get passwords out of this file!
;; - probably want to split into multiple files
;; - be more platform-independent

;; load my passwords
(load "blb-authinfo")

;; define on-darwin if running on MacOS
(defvar on-darwin (eq system-type 'darwin))
;; define on-linux if running on Linux
(defvar on-linux (eq system-type 'gnu/linux))

;;
;; general and global stuff
;;
(defalias 'yes-or-no-p 'y-or-n-p)

;; obsolete since 23.1
;; (set-default-font "-apple-Menlo-medium-normal-normal-*-13-*-*-*-m-0-iso10646-1")
(set-frame-font "-apple-Menlo-medium-normal-normal-*-13-*-*-*-m-0-iso10646-1" t t)
(setq sh-shell-file "/bin/bash")
(setq explicit-shell-file-name "/bin/bash")
(setq midnight-period 3600)
(setq ring-bell-function 'ignore)
(setq visible-bell 'top-bottom)

;; these unfortunately don't work in org mode
(global-set-key (kbd "M-<left>") 'windmove-left)          ; move to left window
(global-set-key (kbd "M-<right>") 'windmove-right)        ; move to right window
(global-set-key (kbd "M-<up>") 'windmove-up)              ; move to upper window
(global-set-key (kbd "M-<down>") 'windmove-down)          ; move to lower window

(global-set-key (kbd "C-c r f") 'recentf-open-files)      ; open recent files
(global-set-key (kbd "C-c m") 'vm-mail) ; open mail composition window

;; supports only AIFF
(defun blb-play-sound-file (sound-file)
  (interactive)
  (when on-darwin
    (start-process-shell-command "afplay" nil (concat "/usr/bin/afplay " sound-file))))

(defun toggle-current-window-dedication ()
 (interactive)
 (let* ((window    (selected-window))
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

(put 'narrow-to-region 'disabled nil)

;; M-; works in place of these
(defalias 'cr 'comment-region)
(defalias 'ucr 'uncomment-region)

(defalias 'ssm 'shell-script-mode)
(defalias 'om 'org-mode)

;; from http://www.emacswiki.org/cgi-bin/wiki/UntabifyUponSave
(defun ska-untabify ()
  (save-excursion
    (goto-char (point-min))
    (when (search-forward "\t" nil t)
      (untabify (1- (point)) (point-max)))
    nil))

(require 'cl)
(load-file "~/.emacs.d/make-password.el")

;; use CPerlMode instead of PerlMode
(defalias 'perl-mode 'cperl-mode)

;; kill *Completions* windows after 60s
(add-hook 'completion-setup-hook
	  (lambda () (run-at-time 60 nil
				  (lambda () (delete-windows-on "*Completions*")))))

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

;;
;; todochiku (growl notifications)
;;
(load-file "~/.emacs.d/todochiku.el")

;;
;; session
;;
(require 'session)
(add-hook 'after-init-hook 'session-initialize)

;;
;; mildly modified Zenburn theme
;;
(require 'blb-zenburn-theme)

;; (require 'color-theme)
;; (eval-after-load "color-theme"
;;   '(progn
;;      (color-theme-initialize)))
;; (setq color-theme-is-cumulative nil)
;; (load "color-theme-solarized")
;; (color-theme-solarized-dark)

;;
;; ViewMail
;;
(add-to-list 'load-path
	     (expand-file-name "~/.emacs.d/vm/lisp"))
(add-to-list 'Info-default-directory-list
	     (expand-file-name "~/.emacs.d/vm/info"))
(require 'vm-autoloads)
;; workaround for missing autoload in 8.2.0b
(load-library "vm-reply")

(setq vm-enable-external-messages '(imap)) ; what does this do?

;; probably from emacswiki
(defun jjf-vm-follow-summary-click (arg)
  "This function is based on vm-follow-summary-cursor, but its behavior is
    tailored for binding to mouse-1.  If the TextCursor is on the currently
    selected message, or at the end of the buffer, do nothing.  Otherwise,
    select the message that the cursor is on."
  (interactive "P")
  (and vm-follow-summary-cursor (eq major-mode 'vm-summary-mode)
       (let ((point (point))
             message-pointer message-list mp)
         (save-excursion
           (set-buffer vm-mail-buffer)
           (setq message-pointer vm-message-pointer
                 message-list vm-message-list))
         (cond ((or (null message-pointer)
                    (and (>= point (vm-su-start-of (car message-pointer)))
                         (< point (vm-su-end-of (car message-pointer))))
                    (eobp))
                nil)
               (t 
                (if (< point (vm-su-start-of (car message-pointer)))
                    (setq mp message-list)
                  (setq mp (cdr message-pointer) message-pointer nil))
                (while (and (not (eq mp message-pointer))
                            (>= point (vm-su-end-of (car mp))))
                  (setq mp (cdr mp)))
                (if (not (eq mp message-pointer))
                    (save-excursion
                      (set-buffer vm-mail-buffer)
                      (vm-record-and-change-message-pointer
                       vm-message-pointer mp)
                      (vm-preview-current-message)
                      ;; return non-nil so the caller will know that
                      ;; a new message was selected.
                      t )))))))
(define-key vm-summary-mode-map (kbd "<mouse-1>")
  'jjf-vm-follow-summary-click)

(require 'u-vm-color)
(add-hook 'vm-summary-mode-hook 'u-vm-color-summary-mode)
(add-hook 'vm-select-message-hook 'u-vm-color-fontify-buffer)
(defadvice vm-show-current-message (after u-vm-color activate)
  (u-vm-color-fontify-buffer-even-more))
(defadvice vm-decode-mime-message (after u-vm-color activate)
  (u-vm-color-fontify-buffer-even-more))

;; silly mail headers
(add-to-list 'load-path "~/.emacs.d/silly-mail")
(require 'silly-mail)
(add-hook 'mail-setup-hook 'sm-add-random-header)

;; announce new mail
(when on-darwin
  (add-hook 'vm-arrived-messages-hook
            (lambda ()(blb-play-sound-file "/System/Library/Sounds/Pop.aiff")))
  (add-hook 'vm-arrived-messages-hook
            (lambda ()(growl "VM" "New mail"))))

;; visit IMAP folders instead of files
;; (defalias 'vm-visit-folder 'vm-visit-imap-folder)
;; doesn't work; edited vm-vars.el instead!
;; Perhaps that could be done with defadvice.

;; unfortunately, this puts the signature after the cited text
;; (defun blb-mail-insert-signature ()
;;   "Hack to avoid damned double dashes above mail signature."
;;   (interactive)
;;   (mail-cc)
;;   (mail-text)
;;   (insert "\n\n--Brian\n")
;;   (mail-to))

(defun blb-insert-mail-signature ()
  "Insert signature because the built-in support doesn't do what I want."
  (interactive)
  (insert "\n--Brian\n\n"))

;; keybinding for inserting signature
(add-hook 'vm-mail-mode-hook '(lambda () (local-set-key (kbd "C-c s") 'blb-insert-mail-signature)))

;; put a blank line above mail citations
;; No, this causes all headers to be cited!
;; (add-hook 'mail-citation-hook '(lambda () (insert "\n")))

;; make the Read and Send Mail from the menu use VM
(define-key menu-bar-tools-menu [rmail] '("Read Mail" . vm))
(define-key-after menu-bar-tools-menu [smail] '("Send Mail" . vm-mail) 'rmail)

;;
;; BBDB
;;
(add-to-list 'load-path
	     (expand-file-name "~/.emacs.d/bbdb/lisp"))
(add-to-list 'Info-default-directory-list
	     (expand-file-name "~/.emacs.d/bbdb/tex"))
(require 'bbdb)            ; may go in .emacs or .vm
(bbdb-initialize)          ; may go in .emacs or .vm

;;
;; Dired
;;
(put 'dired-find-alternate-file 'disabled nil)
(setq dired-chown-program "chown")
(eval-after-load 'dired '(progn (require 'dired-filetype-face)))
(defun dired-open-mac ()
   (interactive)
   (let ((file-name (dired-get-file-for-visit)))
     (if (file-exists-p file-name)
	 (call-process "/usr/bin/open" nil 0 nil file-name))))
(add-hook 'dired-mode-hook '(lambda () (local-set-key (kbd "C-c o") 'dired-open-mac)))
(require 'dired-tar)

;;
;; others
;;
(require 'buff-menu+)
(add-to-list 'same-window-buffer-names "*Buffer List*")

(require 'org-mouse)

(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
(autoload 'csv-mode "csv-mode"
  "Major mode for editing comma-separated value files." t)

(require 'dig)
(require 'dig-browser)

(require 'dns-mode)
(setq auto-mode-alist (cons '("\\.hosts\\'" . dns-mode) auto-mode-alist))

;;
;; markdown
;;
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist
   (cons '("\\.md" . markdown-mode) auto-mode-alist))
;; haven't test this for converting tabs to spaces:
;; (add-hook 'markdown-mode-hook  
;;           '(lambda () 
;;              (make-local-hook 'write-contents-hooks) 
;;              (add-hook 'write-contents-hooks 'ska-untabify nil t)))
;; unsuccessful attempt at dealing with tabs:
;; (add-hook 'markdown-mode-hook
;; 	  (lambda () (setq indent-tabs-mode nil)))

;;
;; line numbering
;;
(add-hook 'cperl-mode-hook (lambda() (linum-mode t)))
(add-hook 'php-mode-hook (lambda() (linum-mode t)))
(add-hook 'python-mode-hook (lambda() (linum-mode t)))

;;
;; auto-insert
;;
(add-hook 'find-file-hooks 'auto-insert)
;; insert export options template for org files under ~/doc
(define-auto-insert "/doc/.*\\.org" "doc.org")
;; insert mode-based default contents
(add-hook 'markdown-mode-hook
	  (lambda ()
	    (auto-insert)
	    (end-of-line)))
(add-to-list 'auto-insert-alist '(markdown-mode . "insert.markdown"))

(add-hook 'cperl-mode-hook
	  '(lambda ()
	     (auto-insert)
	     (goto-char (point-max))))
(add-to-list 'auto-insert-alist '(cperl-mode . "insert.perl"))

(add-hook 'python-mode-hook
	  '(lambda ()
	     (auto-insert)
	     (goto-char (point-max))))
(add-to-list 'auto-insert-alist '(python-mode . "insert.python"))

(add-hook 'sh-mode-hook
	  '(lambda ()
	     (auto-insert)
	     (goto-char (point-max))))
(add-to-list 'auto-insert-alist '(sh-mode . "insert.bash"))

;;
;; try to give buffer menu a legible bg color
;;
;; OK, I gave up and changed faces in buffer menu's Customize.
;;
;; (cust-set-faces
;;   '(Buffer-menu-mode-default ((t (:inherit special-mode-default :background "#3a0938" :foreground "#ff886e" :height 140 :family "Menlo"))) t))

;;
;; AIM
;;
;; switched to bitlbee
;;
;; (load "tnt")
;; (setq tnt-default-username blb-tnt-username)
;; (setq tnt-default-password blb-tnt-password)

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

;;
;; This isn't really necessary.
;;
;; (defun bitlbee-connect ()
;;   (interactive)
;;   (save-window-excursion
;;     (when (get-buffer "&bitlbee")
;;       (switch-to-buffer "&bitlbee")
;;       (erc-message "PRIVMSG" (concat (erc-default-target) " identify " bitlbee-password))
;;       (erc-message "PRIVMSG" (concat (erc-default-target) " account 0 on"))
;;       (erc-message "PRIVMSG" (concat (erc-default-target) " account 1 on")))))
;; (setq bitlbee-reconnect-timer (run-with-timer 0 60 'bitlbee-connect))

(bitlbee-start)

;;
;; erc
;;
;; (require 'tls) ;; redundant when vm is already loaded

;;
;; colors: couldn't get these to work via Customize
;;
(setq erc-keywords '((".*Online.*" (:foreground "green"))
                     (".*Busy" (:foreground "lightpink"))
                     (".*Away" (:foreground "hotpink"))
                     (".*Idle" (:foreground "orange"))
                     ))

;;
;; use growl for ERC's change notifications
;;
(defvar blb-modified-channels-length 0
  "Last recorded length of `erc-modified-channels-alist'.
This is updated each time `blb-erc-growl' gets called from
`erc-track-list-changed-hook'.")

(defun blb-erc-growl ()
  "Use growl for ERC track change notifications."
  (let ((modified-channels-length (length erc-modified-channels-alist)))
    (when (> modified-channels-length blb-modified-channels-length)
      (let ((msg (format "New messages in %s"
                         (mapconcat (lambda (pair)
                                      (buffer-name (car pair)))
                                    erc-modified-channels-alist
                                    ", "))))
        (growl "ERC" msg)
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
      (growl "ERC" "New IM")
      (blb-play-sound-file "/System/Library/Sounds/Submarine.aiff"))))
(add-hook 'erc-insert-pre-hook 'blb-notify-new-im)

;;
;; connect to AIM via bitlbee
;;
(defun aim ()
  "Log into bitlbee with erc for AIM."
  (interactive)
  (if (not (eq major-mode "erc-mode"))
      (select-frame-set-input-focus (make-frame)))
  (erc :server "localhost" :port "6667" :nick "blb"))

(defun freenode ()
  "Log into Freenode with erc."
  (interactive)
  (erc :server "chat.freenode.net" :port "6667" :nick "RhubarbSin" :password blb-freenode-password))

;;
;; iTunes controller; seems broken now
;;
(when on-darwin
  (require 'itunes nil 'noerror)
  (defalias 'iu 'itunes-volume-up)
  (defalias 'id 'itunes-volume-down))

;;
;; I guess this loads all of the nxhtml stuff
;;
(load "nxhtml/autostart")

;;
;; w3m for rendering HTML in vm
;;
(add-to-list 'load-path "~/.emacs.d/emacs-w3m")
(require 'w3m-load)

;;
;; sunrise commander
;;
;; (add-to-list 'load-path "~/.emacs.d/sunrise-commander")
;; (require 'sunrise-commander)
;; (require 'sunrise-x-buttons)

(require 'multi-term)
(setq multi-term-program "/bin/csh")
(setq multi-term-switch-after-close nil)
;; (custom-set-variables
;;   '(term-default-bg-color "#000000")        ;; background color (black)
;;   '(term-default-fg-color "#00FF00"))       ;; foreground color (green)
(global-set-key (kbd "C-c t") 'multi-term)
;; (setq multi-term-buffer-name "shell")

;; Emacs 23: bundled EasyPG
(require 'epa)
;; (epa-file-enable)
(put 'downcase-region 'disabled nil)

;;
;; Emms for streaming audio
;;
(add-to-list 'load-path "~/.emacs.d/emms/")
(require 'emms-setup)
(emms-all)
(emms-default-players)
(require 'emms-streams)

;;
;; cfengine mode for editing CFEngine files
;;
(require 'cfengine)
;; (add-to-list 'auto-mode-alist '("\\.cf$" . cfengine3-mode))
(define-auto-insert ".*\\.cf" "insert.cfengine3")

;;
;; Magit
;;
(add-to-list 'load-path
	     (expand-file-name "~/.emacs.d/magit-1.1.1"))
(add-to-list 'Info-default-directory-list
	     (expand-file-name "~/.emacs.d/magit-1.1.1"))
(autoload 'magit-status "magit" nil t)
