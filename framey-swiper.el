;;; framey.el --- -*- lexical-binding: t -*-

;;; Commentary:
;;; Framey pop-up frame for swiper's search.

;;; Code:

(require 'framey)
(require 'swiper)
(require 'ivy)

(defvar framey--swiper-buffer nil)

(defun framey--swiper-display-fn (str)
  "Swiper display function for STR.

See also `ivy-display-functions-alist'."
  (let* ((prompt (with-current-buffer (window-buffer (active-minibuffer-window))
                   (buffer-string)))
         (prompt-len (length prompt)))
    (remove-text-properties 0 prompt-len '(read-only nil) prompt)
    (with-current-buffer framey--swiper-buffer
      (-let [buffer-read-only nil]
        (erase-buffer)
        (insert prompt "  \n")
        (add-text-properties (1+ prompt-len) (+ 2 prompt-len) '(face cursor))
        (insert str)))))

(defun framey--setup-swiper-frame (&rest _)
  "Create a frame for swiper."
  (setf framey--swiper-buffer (get-buffer-create " *SWIPER*"))
  (framey--display framey--swiper-buffer :keep-focus))

(defun framey--cleanup-swiper ()
  "Delete swiper's framey when the search is done."
  (dolist (frame (frame-list))
    (when (equal (frame-parameter frame 'name) " *SWIPER*")
      (delete-frame frame))))

(defun framey--enable-swiper ()
  "Framey-swiper setup."
  (add-to-list 'ivy-display-functions-alist '(swiper . framey--swiper-display-fn))
  (add-to-list 'ivy-display-functions-props '(framey--swiper-display-fn :cleanup framey--cleanup-swiper))
  (advice-add #'swiper :before #'framey--setup-swiper-frame))

(defun framey--disable-swiper ()
  "Framey-swiper tear-down."
  (setf
   ivy-display-functions-alist (remove '(swiper . framey--swiper-display-fn) ivy-display-functions-alist)
   ivy-display-functions-props (remove '(framey--swiper-display-fn :cleanup framey--cleanup-swiper) ivy-display-functions-props))
  (advice-remove #'swiper #'framey--setup-swiper-frame))

(add-to-list 'framey--enable-functions #'framey--enable-swiper)
(add-to-list 'framey--disable-functions #'framey--disable-swiper)

(when framey-mode (framey-mode))

(provide 'framey-swiper)

;;; framey-swiper.el ends here
