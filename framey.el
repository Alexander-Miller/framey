;;; framey.el ---  -*- lexical-binding: t -*-

;; Package-Requires: ((emacs "26.1") (cl-lib "0.5") (dash "2.11.0") (s "1.10.0") (ht "2.2"))

;;; Commentary:

;;; Code:

(require 'dash)
(require 'shackle)
(require 's)
(require 'ht)
(require 'cl-lib)
(require 'inline)

(cl-declaim (optimize (speed 3) (safety 0)))

(defcustom framey-show-minibuffer t
  "TODO."
  :type 'boolean
  :group 'framey)

(defcustom framey-show-modeline t
  "TODO."
  :type 'boolean
  :group 'framey)

(defcustom framey-default-width 50
  "TODO."
  :type 'number
  :group  'framey)

(defcustom framey-default-height 14
  "TODO."
  :type 'number
  :group  'framey)

(cl-defstruct (framey-pos-info
               (:conc-name framey-pos-info->)
               (:constructor framey-pos-info))
  (height framey-default-height)
  (width framey-default-width)
  (pos 'top))

(defconst framey-pos-info
  (ht ("*helm imenu*"           (framey-pos-info :height 20 :width 45))
      ("*helm mini*"            (framey-pos-info :height 12 :width 100))
      ("*helm-ag*"              (framey-pos-info :height 25 :width 120))
      ("*helm-xref*"            (framey-pos-info :height 25 :width 100))
      ("*helm find files*"      (framey-pos-info :height 12 :width 60))
      ("*helm locate library*"  (framey-pos-info :height 25 :width 100))
      ("*helm locate*"          (framey-pos-info :height 25 :width 100))
      ("*helm org inbuffer*"    (framey-pos-info :height 25 :width 75))
      ("*helm-mode-org-refile*" (framey-pos-info :height 20 :width 100))
      ('helpful-mode            (framey-pos-info :height 35 :width 82))
      (" *SWIPER*"              (framey-pos-info :height 5  :width 80 :pos 'bottom))))

(defvar framey--enable-functions nil)

(defvar framey--disable-functions nil)

(defvar framey-default-size
  (framey-pos-info :width 60 :height 14))

(define-inline framey--get-buffer-size-info (buffer)
  "Fetches the size info for BUFFER, with `framey-default-size' as fallback."
  (declare (side-effect-free t))
  (inline-letevals (buffer)
    (inline-quote
     (ht-get framey-pos-info (buffer-name ,buffer)
             (ht-get framey-pos-info (buffer-local-value 'major-mode ,buffer)
                     framey-default-size)))))

(defun framey-kill ()
  "Kill all framey frames."
  (interactive)
  (--each (frame-list)
    (when (string= "FRAMEY" (frame-parameter it 'name))
      (delete-frame it))))

(defun framey-quit-window (&optional arg)
  "Cancels framey if current window is a child frames.
Otherwise calls `quit-window' with given prefix ARG."
  (interactive "P")
  (-if-let (parent (frame-parameter (selected-frame) 'parent-frame))
      (progn
        (make-frame-invisible (selected-frame))
        (x-focus-frame parent))
    (quit-window arg)))

(defun framey--on-kill (frame)
  "Select the parent when framey FRAME is killed."
  (-when-let (parent (frame-parent frame))
    (select-frame-set-input-focus parent)))

(define-inline framey--poshandler (info)
  "Framey's position handler.
Returns the (X . Y) coordinates for the frame based on the given INFO."
  (inline-letevals (info)
    (inline-quote
     (let ((frame (selected-frame))
           (width (framey-pos-info->width ,info))
           (pos   (framey-pos-info->pos ,info)))
       (cons
        (- (/ (frame-pixel-width frame) 2)
           (/ (* width (frame-char-width frame)) 2))
        (pcase pos
          (`top    (round (* 0.05 (frame-pixel-height frame))))
          (`bottom (round (* 0.75 (frame-pixel-height frame))))))))))

(defun framey--display (buffer &optional keep-focus)
  "Display the given BUFFER in a child frame.
When KEEP-FOCUS is non-nil do NOT redirect focus to the resulting child-frame
and do NOT select it."
  (let* ((inf    (framey--get-buffer-size-info buffer))
         (height (framey-pos-info->height inf))
         (width  (framey-pos-info->width inf))
         (pos    (framey--poshandler inf))
         (window
          (display-buffer-in-child-frame
           buffer
           `((child-frame-parameters
              .
              ((left . ,(car pos))
               (top  . ,(cdr pos))
               (height . ,height)
               (width . ,width)
               (menu-bar-lines . 0)
               (tool-bar-lines . 0)
               (line-spacing . 0)
               (unsplittable . t)
               (no-other-frame . t)
               (minibuffer . ,framey-show-minibuffer)
               (no-special-glyphs . t)
               (undecorated . t)
               (vertical-scroll-bars . nil)
               (internal-border-width . 2)
               ;; (delete-before . TODO)
               (horizontal-scroll-bars . nil)))))))
    (with-selected-window window
      (delete-other-windows)
      (setf truncate-lines t
            mode-line-format nil)
      (add-hook 'delete-frame-functions #'framey--on-kill)
      (set-window-parameter (selected-window) 'mode-line-format 'none))
    (unless keep-focus
      (select-window window)
      (select-frame-set-input-focus (selected-frame) :norecord))
    (selected-window)))

;;;###autoload
(define-minor-mode framey-mode
  ""
  :init-value nil
  :global     t
  :lighter    nil
  (if framey-mode
      (dolist (fn framey--enable-functions)
        (funcall fn))
    (dolist (fn framey--disable-functions)
      (funcall fn))))

(provide 'framey)

;;; framey.el ends here
