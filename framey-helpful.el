;;; framey-helpful.el -*- lexical-binding: t -*-

;;; Commentary:
;;; Helpful-specific framey parts

;;; Code:

(require 'framey)
(require 'helpful)

(defvar framey--shackle-help-rule '(helpful-mode :custom framey--custom-help-rule))

(defun framey--custom-help-rule (buffer __alist __plist)
  "Custom shackle rule to show helpful BUFFER using framey."
  ;; XXX need a minibuffer here or the frame won't have a border with helpful
  (-let [framey-show-minibuffer t]
    (framey--display buffer)))

(define-key helpful-mode-map [remap quit-window] #'framey-quit-window)

(defun framey--enable-helpful ()
  (add-to-list 'shackle-rules framey--shackle-help-rule))

(defun framey--disable-helpful ()
  (setf shackle-rules (delete framey--shackle-help-rule shackle-rules)))

(add-to-list 'framey--enable-functions #'framey--enable-helpful)
(add-to-list 'framey--disable-functions #'framey--disable-helpful)

(when framey-mode (framey-mode))

(provide 'framey-helpful)

;;; framey-helpful.el ends here
