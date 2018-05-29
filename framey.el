;;; framey.el --- TODO -*- lexical-binding: t -*-
;;;

;;; Commentary:
;; TODO

;;; Code:

(require 'dash)
(require 'shackle)
(require 'helm)
(require 's)
(require 'ht)
(require 'cl-lib)

(defcustom framey-show-minibuffer nil
  "TODO."
  :type 'boolean
  :group 'framey)

(defcustom framey-show-modeline nil
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

(defvar framey--shackle-rule '("*helm.*" :custom framey--custom-helm-rule :select t :align below :size 0.33 :regexp t))

(defvar framey-pos-info nil)

(cl-defstruct framey-pos-info
  height
  width)

(defvar framey-default-size
  (make-framey-pos-info :width 50 :height 14))

(setq framey-pos-info
      (ht ("*helm semantic/imenu*" (make-framey-pos-info :height 20 :width 45))
          ("*helm mini*"           (make-framey-pos-info :height 12 :width 100))
          ("*helm-ag*"             (make-framey-pos-info :height 25 :width 120))
          ("*helm find files*"     (make-framey-pos-info :height 12 :width 60))))

(defsubst framey--get-buffer-size-info (buffer)
  "Fetches the size info for BUFFER, with `framey-default-size' as fallback."
  (ht-get framey-pos-info (buffer-name buffer) framey-default-size))

(defsubst framey-make-frame ()
  "Create a new framey frame."
  (make-frame
   `((name                   . "FRAMEY")
     (width                  . ,framey-default-width)
     (parent-frame           . ,(selected-frame))
     (no-accept-focus        . nil)
     (height                 . ,framey-default-height)
     (min-width              . t)
     (min-height             . t)
     (internal-border-width  . 2)
     (unsplittable           . t)
     (no-other-frame         . t)
     (undecorated            . t)
     (vertical-scroll-bars   . nil)
     (horizontal-scroll-bars . nil)
     (minibuffer             . ,framey-show-minibuffer)
     (no-special-glyphs      . t)
     (visibility             . nil)
     (cursor-type            . nil)
     (left-fringe            . 2)
     (right-fringe           . 2))))

(defvar framey--frame (framey-make-frame))

(defun framey--horizontal-center (frame &optional y-pos)
  "Horizontally center FRAME on screen.
If Y-POS is not given position frame 10% off the top of the screen."
  (let* ((move-frame-functions)
         (scr-w (x-display-pixel-width))
         (scr-h (x-display-pixel-height))
         (frm-w (frame-pixel-width frame)))
    (set-frame-position frame
                        (- (/ scr-w 2) (/ frm-w 2))
                        (or y-pos (round (* 0.1 scr-h))))))

(defun framey--helm-canceller (&rest _)
  "Make framey invisible after a helm action or abort."
  (when (and framey-frame
             (frame-live-p framey-frame)
             (frame-visible-p framey-frame))
    (set-frame-parameter framey-frame 'parent-frame nil)
    (set-frame-position framey-frame 1366 768)
    (delete-frame framey-frame)
    (select-window (selected-window))))

(add-hook 'helm-quit-hook #'framey--helm-canceller)
(add-hook 'helm-after-action-hook #'framey--helm-canceller)

(defun framey--custom-helm-rule (buffer __alist __plist)
  "Custom shackle rule to show BUFFER using framey."
  (condition-case _
      (-let [framey (framey-make-frame)]
        (setq framey--frame framey)
        (-let [[_ height width] (framey--get-buffer-size-info buffer)]
          (set-frame-size framey width height)
          (framey--horizontal-center framey))
        (select-frame framey)
        (delete-other-windows)
        (display-buffer-record-window 'window (selected-window) buffer)
        (unless framey-show-modeline
          (set-window-parameter (selected-window) 'mode-line-format 'none))
        (set-window-dedicated-p (selected-window) nil)
        (set-window-buffer (selected-window) buffer)
        (framey--horizontal-center framey)
        (make-frame-visible framey)
        (select-window (selected-window))
        (helm-update)
        (message "")
        (helm-window))
    (error (framey--helm-canceller))))

;;;###autoload
(define-minor-mode framey-mode
  ""
  :init-value nil
  :global     t
  :lighter    nil
  (if framey-mode
      (add-to-list 'shackle-rules framey--shackle-rule)
    (setq shackle-rules (delete framey--shackle-rule shackle-rules))))

(provide 'framey)

;;; framey.el ends here
