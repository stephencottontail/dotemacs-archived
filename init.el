;; Use MELPA for packages
(require (quote package))
(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Global keybinds
(global-set-key (kbd "M-<SPC>") 'set-mark-command)

;; Setup stacking hydras
;;
;; see https://github.com/abo-abo/hydra/wiki/Nesting-Hydras
(defvar hydra-stack nil "Holds the current and previous hydra.")

(defun hydra-push (expr)
  (push '(lambda () ,expr) hydra-stack))

(defun hydra-pop ()
  (interactive)
  (let ((x (pop hydra-stack)))
    (when x
      (funcall x))))

;; Editorconfig
(use-package editorconfig
  :ensure t
  :config (editorconfig-mode 1))

;; Ivy
;;
;; LATER: fix colors of matches in minibuffer?
;;
;; i couldn't get `ivy-minibuffer-match-face` to work in my color theme
;; nor could i get a satisfactory color scheme in `M-x customize`, so
;; let's look at it later
(use-package ivy
  :ensure t
  :demand
  :config (setq ivy-use-virtual-buffers t
		ivy-count-format "%d/%d "
		ivy-display-style 'fancy)
  (ivy-mode 1))

;; Projectile
(use-package projectile
  :ensure t
  :bind (("C-c p" . skd/projectile-hydra/body))
  :config (setq projectile-completion-system (quote ivy)
		projectile-switch-project-action (quote projectile-dired))
  (projectile-mode 1))

;; INFO: the headings line up with the colon of their associated
;; options when Emacs displays the hydra; the headings need
;; to be placed where they are in the definition because Emacs
;; doesn't display the underscores
(defhydra skd/projectile-hydra (:color blue :hint nil)
  "
    Find              Search              Buffers
----------------------------------------------------------
 _f_: file dwim        _R_: rg             _b_: switch to buffer
 _r_: recent file      _o_: multi-occur    _K_: kill buffers
 _d_: dir
 _p_: other project
"
  ("f"  projectile-find-file-dwim)
  ("r"  projectile-recentf)
  ("d"  projectile-find-dir)
  ("p"  projectile-switch-project)
  ("R"  projectile-ripgrep)
  ("o"  projectile-multi-occur)
  ("b"  projectile-switch-to-buffer)
  ("K"  projectile-kill-buffers)
  ("q"  nil "cancel" :color blue))

;; Flycheck
;;
;; FIXME: checking doesn't work for JS
;;
;; this is because Emacs can't find a configuration file
;; for `eslint`, but i'm not confident enough yet to
;; introduce it to LC projects; i need to research if
;; i can set a global configuration file that wouldn't
;; need to be included in an LC repo
(require 'flymake)








;; Custom mode line
;; 
;; This custom face deliberately has no attributes so we can target
;; all tags in my color theme
(defface skd/replacement-tags '((t)) "Custom face for replacement tags in the mode line" :group (quote skd-custom-font-faces))

;; A list of (REGEXP REPLACEMENT) used to save space in the mode
;; line. IMO, the Emacs default of just using the file name is
;; insufficient in today's web development world where file
;; names are often duplicated across the folder structure, but
;; using a full path uses up too much space.
(setq replacements
      (list
       '("/home/stephen/" "")
       '("Documents/webdev/limecuda" ":LC:")
       '("Documents/webdev/personal" ":PP:")
       '("wordpress/wp-content/themes" ":WCT:")
       '("wordpress/wp-content/plugins" ":WCP:")
       '(".emacs.d" ":CFG:")
       ))

(defun generate-replacement-tags (in)
  (let ((out in))
    (dolist (cur replacements)
      (setq out (replace-regexp-in-string (car cur) (car (cdr cur)) out)))
    out))

(setq-default mode-line-format
	      (list
	       '(:eval
		 (cond
		  (buffer-read-only (propertize " & " (quote face) (quote bold)))
		  ((buffer-modified-p) (propertize " * " (quote face) (quote bold)))
		  (t (propertize " - " (quote face) (quote bold)))))
	       '(:eval
		 (cond (buffer-file-name
			(propertize (generate-replacement-tags (buffer-file-name)) (quote face) (quote skd/replacement-tags)))
		       (t
			mode-line-buffer-identification)))
	       "    ("
	       '(:eval (propertize mode-name (quote face) (quote bold)))

	       ")    "
	       ))

;; Control how Emacs makes backups
(setq backup-directory-alist '(("." . "~/.emacs.d/backup")))

;; Use relative line numbers
(global-display-line-numbers-mode 1)
(setq-default display-line-numbers-type (quote relative))

;; Magit
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

;; Set up Alabaster color scheme
;; <C-u> <M-x> describe-char to see what text properties
;;
;; FIXME: colors for search highlights and visual indicator
;; FIXME: line numbers in *Occur* buffers
(add-to-list (quote custom-theme-load-path) (concat (file-name-directory user-init-file) "themes"))
(load-theme (quote alabaster) t)

;; HTML/PHP/Sass development
(use-package web-mode
  :ensure t
  :init (setq web-mode-block-padding -1)
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.blade\\.php" . web-mode))
  (add-hook 'web-mode-hook 'flymake-mode))

(use-package flymake-php
  :init (add-hook 'web-mode-hook 'flymake-php-load))

;; flymake-sass seems to be a bit out-of-date and
;; I don't have the time or the inclination to wrangle it
;; into shape

;; JS/React development
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js-mode))
;; flymake-eslint.el exposes a variable
;; 'flymake-eslint-executable-args` which could in theory
;; allow me to pass a different config file depending on
;; the file, must examine this further
(use-package flymake-eslint
  :ensure t
  :init
  (add-hook 'js-mode-hook 'flymake-eslint-enable)
  (add-hook 'js2-mode-hook 'flymake-eslint-enable)
  (add-hook 'js-mode-hook (lambda ()
			    (pcase (file-name-extension (buffer-file-name))
			      ("jsx" (funcall 'js2-minor-mode))))))

;; Setup LSP
;;
;; FIXME: `lsp-ui` needs to be configured
;; FIXME: keyboard shortcuts need to be configured (hydra?)
(use-package lsp-mode
  :ensure t
  :bind (("C-c l" . skd/lsp-hydra/body))
  :hook (((rjsx-mode js2-mode emacs-lisp-mode) . lsp)
	 (lsp-mode . register-php-lsp)
	 (web-mode . check-lsp))
  :init (setq lsp-file-watch-threshold '10000
	      lsp-intelephense-storage-path (expand-file-name (locate-user-emacs-file "intelephense"))
	      lsp-intelephense-stubs '(
				       ["apache" "bcmath" "bz2" "calendar"
					"com_dotnet" "Core" "ctype" "curl" "date" "dba" "dom" "enchant"
					"exif" "fileinfo" "filter" "fpm" "ftp" "gd" "hash" "iconv" "imap" "interbase"
					"intl" "json" "ldap" "libxml" "mbstring" "mcrypt" "meta" "mssql" "mysqli"
					"oci8" "odbc" "openssl" "pcntl" "pcre" "PDO" "pdo_ibm" "pdo_mysql"
					"pdo_pgsql" "pdo_sqlite" "pgsql" "Phar" "posix" "pspell" "readline" "recode"
					"Reflection" "regex" "session" "shmop" "SimpleXML" "snmp" "soap" "sockets"
					"sodium" "SPL" "sqlite3" "standard" "superglobals" "sybase" "sysvmsg"
					"sysvsem" "sysvshm" "tidy" "tokenizer" "wddx" "wordpress" "xml" "xmlreader" "xmlrpc"
					"xmlwriter" "Zend OPcache" "zip" "zlib"])))

(require (quote lsp-intelephense))

(defun register-php-lsp ()
  "Define the LSP client for PHP."
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("/usr/local/bin/intelephense" "--stdio"))
		    :major-modes '(web-mode)
		    :server-id (quote skd/iph)
		    :notification-handlers (ht ("indexingStarted" #'ignore)
					       ("indexingEnded" #'ignore))
		    :initialization-options (lambda ()
					      (list :storagePath lsp-intelephense-storage-path)))))

(defun check-lsp ()
  "Load the appropriate LSP client depending on the file type."
  (pcase (file-name-extension (buffer-file-name))
    ;; LATER: start servers programmatically based on file extension
    ;;
    ;; oh lord--so it looks like `lsp` works like `flycheck`: it chooses
    ;; the server to use based on the major mode, which is a reasonable
    ;; thing to do except that I want to use `web-mode` which can handle
    ;; multiple file types; think what i'll have to do is emulate the
    ;; `C-u` prefix when calling `lsp` and pass the server as the second argument
    ;;
    ;; oh lord--`lsp` doesn't take a server name as an argument; the only
    ;; argument represents whether `lsp` should start a new server even
    ;; if a currently-running server could handle the mode
    ;;
    ;; see ~/.emacs.d/lsp-hack.el for a possible solution, but don't worry
    ;; about it for now, projectile & ivy are more important
    ;; 
    ;; see https://stackoverflow.com/questions/6156286/emacs-lisp-call-function-with-prefix-argument-programmatically
    ("html" (funcall (lambda ()
		       (let ((current-prefix-arg '(4)))
			 (call-interactively 'lsp)))))
    ;; FIXME: check if server was started correctly before
    ;; starting `lsp`
    ;;
    ;; why do i call `lsp` with `doesnt-exist`? because you need to call
    ;; it with an truthy argument so a new server will start
    ;; even if an existing server could be applied
    ("php" (funcall (lambda ()
		      (register-php-lsp)
		      (lsp))))
    (- (message "this is something else"))))


(use-package lsp-ui
  :ensure t
  :requires lsp-mode
  :config (setq lsp-ui-doc-enable t
		lsp-ui-doc-use-childframe 5
		lsp-ui-doc-position 'top
		lsp-ui-doc-include-signature t
		lsp-ui-sideline-enable nil
		lsp-ui-peek-enable t
		lsp-ui-peek-list-width 60
		lsp-ui-peek-peek-height 25)

  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package company
  :ensure t
  :config (setq company-idle-delay 0.3)

  (global-company-mode 1)
  (global-set-key (kbd "C-<tab>") 'company-complete))

(use-package company-lsp
  :ensure t
  :requires company
  :config (push (quote company-lsp) company-backends)

  (setq company-transformers nil
	company-lsp-async t
	company-lsp-cache-candidates nil))

(defhydra skd/lsp-hydra (:exit t :hint nil)
  "
 Buffer^^               Server^^                   Symbol
---------------------------------------------------------------------------------------------------------------
 [_f_] format           [_M-r_] restart            [_d_] declaration  [_i_] implementation  [_o_] documentation
 [_m_] imenu            [_S_]   shutdown           [_D_] definition   [_t_] type            [_r_] rename
 [_x_] execute action   [_M-s_] describe session   [_R_] references   [_s_] signature"
  ("d" lsp-find-declaration)
  ("D" lsp-ui-peek-find-definitions)
  ("R" lsp-ui-peek-find-references)
  ("i" lsp-ui-peek-find-implementation)
  ("t" lsp-find-type-definition)
  ("s" lsp-signature-help)
  ("o" lsp-describe-thing-at-point)
  ("r" lsp-rename)

  ("f" lsp-format-buffer)
  ("m" lsp-ui-imenu)
  ("x" lsp-execute-code-action)

  ("M-s" lsp-describe-session)
  ("M-r" lsp-restart-workspace)
  ("S" lsp-shutdown-workspace))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("998f0949f8abd0ad3ca9a210c455ccf2f75e7273bedfd1da48e889acc38bacf9" "9324e79bc126f49a289aaccbe207b3ba6f2ce7ebc077ca6eb96f9864ecf860c2" default))
 '(package-selected-packages
   '(flymake-sass flymake-css flymake-php flymake-eslint undo-tree editorconfig projectile-ripgrep ripgrep hydra ivy-hydra counsel-projectile projectile counsel ivy swiper forge magit rjsx-mode key-chord yasnippet web-mode company-lsp lsp-mode lsp-ui use-package abyss-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
