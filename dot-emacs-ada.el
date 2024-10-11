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

(require 'other-frame-window)
(require 'ada-mode)
(add-hook
 'ada-mode-hook
 (lambda ()
   (setq-local comment-padding "  ")
   (add-hook `before-save-hook #'delete-trailing-whitespace nil t)
   (add-hook 'before-save-hook
             (lambda () (untabify (point-min) (point-max))) nil t)
   (add-hook 'project-find-functions #'wisi-prj-current-cached nil t)
   (add-hook 'xref-backend-functions #'wisi-prj-xref-backend nil t)
   ))

;;; this for .gpr file editing
(require 'gpr-mode)
(add-hook
 'gpr-mode-hook
 (lambda()
   (setq-local comment-padding "  ")
   (add-hook 'before-save-hook
             (lambda () (untabify (point-min) (point-max))) nil t)
   (add-hook `before-save-hook #'delete-trailing-whitespace) nil t))

;;; Imenu for Ada (also calls in ada-mode, but see above)
(require 'ada-imenu)

;;; gnatchop-mode
(autoload 'gnatchop-mode "gnatchop.el"
  "Minor mode for running gnatchop on save" t)
