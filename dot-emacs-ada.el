;;  Ada aspects for .emacs, as for Stephe's ada-mode.

;; Stephe's logging for ada-mode
(setq ada-process-parse-exec-opts
      '("--recover-log" "/Users/simon/tmp/ada-mode-log/recover.log"))

;; override tiresome default skeleton-insert behaviour
(setq skeleton-end-newline nil)

;; whence ada-mode?
(setq ada-mode-local nil)

(if ada-mode-local
    (progn
      (add-to-list 'load-path
                   (expand-file-name "~/Developer/ada-mode/org.emacs.ada-mode"))
      (add-to-list 'load-path
                   (expand-file-name "~/Developer/ada-mode/org.emacs.wisi"))
      (load-library "autoloads.el")
      (setenv
       "PATH"
       (concat (expand-file-name "~/Developer/ada-mode/org.emacs.ada-mode")
               ":"
               (getenv "PATH")))))

(use-package eglot
  :ensure t
  :defer t
  :config
  (add-to-list 'eglot-server-programs
               '(gpr-el-mode . ("ada_language_server" "--language-gpr"))))

(defun ada-mode--find-alire (dir)
  (let ((alire (locate-dominating-file dir "alire.toml")))
    (if alire
      (cons 'transient alire)
      nil)))

(defun ada-mode--find-gpr (dir)
  (let ((gpr (locate-dominating-file
              dir
              (lambda (dir) (directory-files dir nil "gpr"))
              )))
    (if gpr
      (cons 'transient gpr)
      nil)))

(use-package project
  :config
  ;; last-in, first-used.
  ;; can't use :hook because the hook variable doesn't end with -hook.
  (add-hook 'project-find-functions #'ada-mode--find-gpr)
  (add-hook 'project-find-functions #'ada-mode--find-alire)
  ;; :custom
  ;; (project-vc-extra-root-markers '("alire.toml" "*.gpr"))
  )

(use-package ada-mode
    :ensure t
    :defer t
    
    ;; :hook
    ;; (before-save . delete-trailing-whitespace)
                  
    :custom
    (ada-auto-case t)
    (ada-build-prompt-prj 'search-prompt)
    (ada-case-strict nil)
    (ada-diagnostics-backend 'eglot)
    (ada-face-backend 'none)
    (ada-xref-backend 'eglot)
    ;; (tab-width 3)
    )

(add-hook
 'ada-mode-hook
 (lambda ()
   (setq-local tab-width ada-indent)
   (setq-local comment-padding "  ")
   (add-hook `before-save-hook #'delete-trailing-whitespace nil t)
   (add-hook 'before-save-hook
             (lambda () (untabify (point-min) (point-max))) nil t)))

;; (require 'other-frame-window)
;; (require 'ada-mode)
;; (add-hook
;;  'ada-mode-hook
;;  (lambda ()
;;    (setq-local tab-width ada-indent)
;;    (setq-local comment-padding "  ")
;;    (add-hook `before-save-hook #'delete-trailing-whitespace nil t)
;;    (add-hook 'before-save-hook
;;              (lambda () (untabify (point-min) (point-max))) nil t)
;;    (add-hook 'project-find-functions #'wisi-prj-current-cached nil t)
;;    (add-hook 'xref-backend-functions #'wisi-prj-xref-backend nil t)
;;    ))

;;; this for .gpr file editing

(use-package gpr-el-mode
  :ensure nil
  :hook
  (gpr-el-mode . eglot-ensure)
  )

(require 'gpr-el-mode)
(add-hook
 'gpr-el-mode-hook
 (lambda()
   (setq-local ada-indent 3)
   (setq-local tab-width ada-indent)
   (setq-local comment-padding "  ")
   (add-hook 'before-save-hook
             (lambda () (untabify (point-min) (point-max))) nil t)
   (add-hook `before-save-hook #'delete-trailing-whitespace) nil t))

;;; Imenu for Ada (also calls in ada-mode, but see above)
;; (require 'ada-imenu)

;;; gnatchop-mode
(autoload 'gnatchop-mode "gnatchop.el"
  "Minor mode for running gnatchop on save" t)
