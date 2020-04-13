;;; framey.el --- TODO -*- lexical-binding: t -*-
;; Package-Requires: ((emacs "26.1") (cl-lib "0.5") (dash "2.11.0") (s "1.10.0") (ht "2.2"))

;;; Commentary:
;; TODO

;;; Code:

(require 'dash)
(require 'shackle)
(require 's)
(require 'ht)
(require 'cl-lib)
(require 'inline)

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

(cl-defstruct framey-pos-info
  height
  width)

(defvar framey--shackle-help-rule '(helpful-mode :custom framey--custom-help-rule))

(defconst framey-pos-info
  (ht ("*helm semantic/imenu*"  (make-framey-pos-info :height 20 :width 45))
      ("*helm mini*"            (make-framey-pos-info :height 12 :width 100))
      ("*helm-ag*"              (make-framey-pos-info :height 25 :width 120))
      ("*helm-xref*"            (make-framey-pos-info :height 25 :width 100))
      ("*helm find files*"      (make-framey-pos-info :height 12 :width 60))
      ("*helm locate library*"  (make-framey-pos-info :height 25 :width 100))
      ("*helm locate*"          (make-framey-pos-info :height 25 :width 100))
      ("*helm org inbuffer*"    (make-framey-pos-info :height 25 :width 75))
      ("*helm-mode-org-refile*" (make-framey-pos-info :height 20 :width 100))
      ('helpful-mode            (make-framey-pos-info :height 35 :width 82))))

(defvar framey-default-size
  (make-framey-pos-info :width 60 :height 14))

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

(define-inline framey--poshandler (width)
  "Framey's position handler.
Sets the frame in the upper center based on INFO."
  (inline-letevals (width)
    (inline-quote
     (-let [fr (selected-frame)]
       (cons
        (- (/ (frame-pixel-width fr) 2)
           (/ (*,width (frame-char-width fr)) 2))
        (round (* 0.05 (frame-pixel-height fr))))))))

(defun framey--display (buffer)
  (let* ((inf    (framey--get-buffer-size-info buffer))
         (height (framey-pos-info-height inf))
         (width  (framey-pos-info-width inf))
         (pos    (framey--poshandler width)))
    (setf (buffer-local-value 'mode-line-format buffer) nil)
    (select-window
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
          (minibuffer . nil)
          (no-special-glyphs . t)
          (undecorated . t)
          (vertical-scroll-bars . nil)
          (internal-border-width . 2)
          ;; (delete-before . TODO)
          (horizontal-scroll-bars . nil))))))
    (delete-other-windows)
    (setf truncate-lines t)
    (setf mode-line-format nil)
    (selected-window)))

;;; Helpful -----------------------------------

(defun framey--custom-help-rule (buffer __alist __plist)
  "Custom shackle rule to show helpful BUFFER using framey."
  (framey--display buffer))

(with-eval-after-load 'helpful
  (with-no-warnings
    (define-key helpful-mode-map [remap quit-window] #'framey-quit-window)))

;;;###autoload
(define-minor-mode framey-mode
  ""
  :init-value nil
  :global     t
  :lighter    nil
  (if framey-mode
      (progn
        (when (featurep 'framey-helm)
          (setf helm-display-function #'framey--display-helm)
          (advice-add 'helm-cleanup :around #'framey--helm-cleanup)
          (advice-add 'helm-execute-persistent-action :around #'framey--helm-persistent-action-advice))
        (add-to-list 'shackle-rules framey--shackle-help-rule))
    (when (featurep 'framey-helm)
      (setf helm-display-function #'helm-default-display-buffer)
      (advice-remove 'helm-cleanup #'framey--helm-cleanup)
      (advice-remove 'helm-execute-persistent-action #'framey--helm-persistent-action-advice))
    (setf shackle-rules (delete framey--shackle-help-rule shackle-rules))))

(provide 'framey)

;;; framey.el ends here
