;; ada-ts-mode & friends, from dotemacs-ada.

(message ".emacs-ada.el is here!")

(setq init.el/preferred-lsp-client 'lsp-mode)

;;;; Ada

(use-package ada-ts-mode
  :defines (org-src-lang-modes)
  :custom ((ada-ts-mode-grammar-install 'auto)
           (ada-ts-mode-indent-backend 'lsp)) ; Use LSP-based indenting
  :bind (:map ada-ts-mode-map
              (("C-c C-b" . ada-ts-mode-defun-comment-box)
               ("C-c C-o" . ada-ts-mode-find-other-file)
               ("C-c C-p" . ada-ts-mode-find-project-file)))
  :init
  (with-eval-after-load 'org-src
    (add-to-list 'org-src-lang-modes '("ada" . ada-ts)))
  (add-hook
   'ada-ts-mode-hook
   (lambda()
     (setq-local comment-padding "  ")
     (add-hook `before-save-hook `delete-trailing-whitespace nil t)
     (add-hook 'before-save-hook
               (lambda () (untabify (point-min) (point-max))) nil t)     
     )))

;;;; GNAT Project

(use-package gpr-ts-mode
  :defines (org-src-lang-modes)
  :custom (gpr-ts-mode-grammar-install 'auto)
  :init
  (with-eval-after-load 'org-src
    (add-to-list 'org-src-lang-modes '("gpr" . gpr-ts)))
  (add-hook
   'gpr-ts-mode-hook
   (lambda()
     (setq-local comment-padding "  ")
     (add-hook `before-save-hook `delete-trailing-whitespace nil t)
     (add-hook 'before-save-hook
               (lambda () (untabify (point-min) (point-max))) nil t)     
     )))

;;;; Company

(use-package company
  :commands (global-company-mode)
  :config (global-company-mode))

;;;; Compile

(use-package compile
  :ensure nil ; built-in
  :custom (compilation-scroll-output t)
  :init
  (put 'compile-command 'safe-local-variable #'stringp))

;;;; Emacs

(use-package emacs
  :ensure nil ; built-in
  :init
  ;; Recommended settings when using LSP
  :custom ((gc-cons-threshold 100000000)             ; 100MB
           (read-process-output-max (* 1024 1024)))) ; 1MB

;;;; Electric Pair

(use-package elec-pair
  :ensure nil ; built-in
  :hook ((ada-ts-mode gpr-ts-mode) . electric-pair-local-mode))

;;;; Imenu

(use-package imenu
  :ensure nil ; built-in
  :custom (imenu-auto-rescan t)
  :hook ((ada-ts-mode gpr-ts-mode) . imenu-add-menubar-index))

;;;; lsp-mode

(use-package lsp-mode
  :if (eq init.el/preferred-lsp-client 'lsp-mode)
  :preface
  ;; Workaround for https://github.com/AdaCore/ada_language_server/issues/1204
  (defun init.el/fix-eol/lsp--render-string (args)
    (let ((strings (list (car args))))
      (dolist (eol '("\r\n" "\n" "\r"))
        (setq strings
              (flatten-list (mapcar (lambda (value)
                                      (split-string value eol))
                                    strings))))
      (cons (string-join strings "\n") (cdr args))))
  :init
  (advice-add 'lsp--render-string
              :filter-args #'init.el/fix-eol/lsp--render-string)
  :custom ((lsp-auto-guess-root t)
           (lsp-enable-indentation nil) ; Let major mode control indentation
           (lsp-enable-on-type-formatting nil) ; Interferes with Emacs indenting
           (lsp-headerline-breadcrumb-enable nil)
           (lsp-keymap-prefix "C-c l")
           (lsp-semantic-tokens-enable t)
           (lsp-enable-imenu nil)) ; Let major mode control Imenu
  ;; Add mapping for `lsp-mode' "xref" functions
  :bind (:map lsp-mode-map (("M-." . lsp-find-definition)
                            ("M-?" . lsp-find-references)))
  :custom-face
  (lsp-face-semhl-number ((t (:inherit font-lock-number-face))))
  :hook ((ada-ts-mode gpr-ts-mode) . lsp))

;;;; Which Function

(use-package which-func
  :ensure nil ; built-in
  :demand t
  :config (which-function-mode))

;;;; Xref

(use-package xref
  :ensure nil ; built-in
  :preface
  (defun init.el/fix-point/xref-find-definitions-at-mouse (event)
    (interactive "e")
    (mouse-set-point event))
  :init
  ;; Workaround for https://debbugs.gnu.org/cgi/bugreport.cgi?bug=65578
  (when (< emacs-major-version 30)
    (advice-add 'xref-find-definitions-at-mouse
                :before #'init.el/fix-point/xref-find-definitions-at-mouse))
  :bind (("C-<mouse-1>" . #'ignore) ; Prevent "undefined" message on mouse up
         ("C-<down-mouse-1>" . #'xref-find-definitions-at-mouse)))

;;;; YASnippet

(use-package yasnippet
  :hook (gpr-ts-mode . yas-minor-mode-on))

(use-package gpr-yasnippets)

