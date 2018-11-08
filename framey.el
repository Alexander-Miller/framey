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

(defvar framey--shackle-rule '("*helm.*" :custom framey--custom-helm-rule :select t :align below :size 0.33 :regexp t))
(defvar framey--shackle-help-rule '(helpful-mode  :custom framey--custom-help-rule))

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

(defvar framey--frame nil)

(defsubst framey--get-buffer-size-info (buffer)
  "Fetches the size info for BUFFER, with `framey-default-size' as fallback."
  (ht-get framey-pos-info (buffer-name buffer) framey-default-size))

(defsubst framey-make-frame ()
  "Create a new framey frame."
  (when (or (null framey--frame)
            (not (frame-live-p framey--frame)))
    (setq framey--frame
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
             (right-fringe           . 2)))))
  (set-frame-position framey--frame 9999 9999)
  (set-frame-parameter framey--frame 'parent-frame (selected-frame))
  framey--frame)

(defun framey-kill ()
  "Kill all framey frames."
  (interactive)
  (--each (frame-list)
    (when (string= "FRAMEY" (frame-parameter it 'name))
      (delete-frame it))))

(defun framey--horizontal-center (frame &optional y-pos)
  "Horizontally center FRAME on screen.
If Y-POS is not given position frame 10% off the top of the screen."
  (let* ((move-frame-functions)
         (selfr (selected-frame))
         (scr-w (frame-pixel-width selfr))
         (scr-h (frame-pixel-height selfr))
         (frm-w (frame-pixel-width frame)))
    (set-frame-position frame
                        (- (/ scr-w 2) (/ frm-w 2))
                        (or y-pos (round (* 0.1 scr-h))))))

(defun framey--helm-canceller (&rest _)
  "Make framey invisible after a helm action or abort."
  (when (frame-live-p framey--frame)
    (set-frame-position framey--frame 9999 9999)
    (x-focus-frame (frame-parent framey--frame))))

(add-hook 'helm-quit-hook #'framey--helm-canceller)
(add-hook 'helm-after-action-hook #'framey--helm-canceller)

(defun framey--custom-helm-rule (buffer __alist __plist)
  "Custom shackle rule to show BUFFER using framey."
  (condition-case _
      (-let [framey (framey-make-frame)]
        (-let [[_ height width] (framey--get-buffer-size-info buffer)]
          (set-frame-size framey width height)
          (framey--horizontal-center framey))
        (select-frame framey t)
        (delete-other-windows)
        (display-buffer-record-window 'window (selected-window) buffer)
        (unless framey-show-modeline
          (set-window-parameter (selected-window) 'mode-line-format 'none))
        (set-window-dedicated-p (selected-window) nil)
        (set-window-buffer (selected-window) buffer)
        (helm-update)
        (force-mode-line-update t)
        (run-with-timer 0.01 nil #'helm-update)
        (x-focus-frame framey)
        (helm-window))
    (error (framey--helm-canceller))))

(defun framey--custom-help-rule (buffer __alist __plist)
  "Custom shackle rule to show helpful BUFFER using framey."
  (-let [framey (framey-make-frame)]
    (-let [[_ height width] [20 33 80]]
      (set-frame-size framey width height)
      (framey--horizontal-center framey))
    (select-frame framey)
    (delete-other-windows)
    (switch-to-buffer buffer t t)
    (unless framey-show-modeline
      (set-window-parameter (selected-window) 'mode-line-format 'none))
    (selected-window)))

(defun framey-quit-window (&optional arg)
  "Cancels framey if current window is a child frames.
Otherwise calls `quit-window' with given prefix ARG."
  (interactive "P")
  (if (string= "FRAMEY" (frame-parameter (selected-frame) 'name))
      (framey--helm-canceller)
    (quit-window arg)))

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
        (add-to-list 'shackle-rules framey--shackle-rule)
        (add-to-list 'shackle-rules framey--shackle-help-rule))
    (setq shackle-rules (delete framey--shackle-rule shackle-rules))
    (setq shackle-rules (delete framey--shackle-help-rule shackle-rules))))

(provide 'framey)

;;; framey.el ends here
