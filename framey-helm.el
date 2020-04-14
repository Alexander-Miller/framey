;;; framey-helm.el -*- lexical-binding: t -*-

;;; Commentary:
;;; Helm-specific framey parts

;;; Code:

(require 'framey)
(require 'helm)

(defun framey--helm-persistent-action-advice (fun &rest args)
  "Advice to allow tabbing in helm to work.
Will call original FUN with ARGS with `helm--buffer-in-new-frame-p' set to t."
  (let ((helm--buffer-in-new-frame-p t))
    (apply fun args)))

(defun framey--helm-cleanup (orig-func)
  "Call ORIG-FUNC without helm's delete-frame-fuction."
  (cl-letf (((symbol-function 'bury-buffer) #'ignore)
            ((symbol-function 'helm--delete-frame-function) #'ignore))
    (funcall orig-func)))

(defun framey--display-helm (buffer-name _)
  "Display the given helm BUFFER-NAME in a child frame."
  (-let [b (get-buffer buffer-name)]
    (with-current-buffer b (setq-local helm--buffer-in-new-frame-p t))
    (framey--display b)
    (selected-window)))

(defun framey--enable-helm ()
  (setf helm-display-function #'framey--display-helm)
  (advice-add 'helm-cleanup :around #'framey--helm-cleanup)
  (advice-add 'helm-execute-persistent-action :around #'framey--helm-persistent-action-advice))

(defun framey--disable-helm ()
  (setf helm-display-function #'helm-default-display-buffer)
  (advice-remove 'helm-cleanup #'framey--helm-cleanup)
  (advice-remove 'helm-execute-persistent-action #'framey--helm-persistent-action-advice))

(add-to-list 'framey--enable-functions #'framey--enable-helm)
(add-to-list 'framey--disable-functions #'framey--disable-helm)

(when framey-mode (framey-mode))

(provide 'framey-helm)

;;; framey-helm.el ends here
