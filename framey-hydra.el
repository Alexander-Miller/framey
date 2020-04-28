;;; framey-hydra.el -*- lexical-binding: t -*-

;;; Commentary:
;;; Hydra-specific framey parts

;;; Code:

(require 'framey)
(require 'hydra)

(defvar framey--hydra-frame nil)

(defvar framey--hide-timer nil)

(defun framey--display-hydra (str)
  (when framey--hide-timer
    (cancel-timer framey--hide-timer)
    (setf framey--hide-timer nil))
  (let ((buffer (get-buffer-create "*Hydra Framey*"))
        (frame (selected-frame))
        (width 0))
    (with-current-buffer buffer
      (erase-buffer)
      (insert str)
      (goto-char 0)
      (while (/= (forward-line 1) 1)
        (setf width (max width (- (point-at-eol) (point-at-bol))))))
    (if (frame-live-p framey--hydra-frame)
        (select-frame framey--hydra-frame)
      (framey--display buffer))
    (setf framey--hydra-frame (selected-frame))
    (set-frame-width framey--hydra-frame width)
    (redirect-frame-focus (selected-frame) frame)
    (select-frame frame)
    (x-focus-frame frame)))

(defun framey--hide-hydra ()
  (unless framey--hide-timer
    (setf framey--hide-timer
          (run-with-timer
           0.05 nil
           (lambda ()
             (setf framey--hide-timer nil)
             (when (frame-live-p framey--hydra-frame)
               (-let [parent (frame-parent framey--hydra-frame)]
                 (delete-frame framey--hydra-frame)
                 (x-focus-frame parent))))))))

(let ((display-list (list 'hydra-framey
                          #'framey--display-hydra
                          #'framey--hide-hydra)))
  (progn
    (add-to-list 'hydra-hint-display-alist display-list)
    (setf hydra-hint-display-type 'hydra-framey)))

(when framey-mode (framey-mode))

(provide 'framey-helpful)

;;; framey-helpful.el ends here
