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

(defvar framey-default-width 50)
(defvar framey-default-height 14)
(defvar framey-describe-y-pos 90)

(cl-defstruct framey-pos-info
  height
  width)

(defvar framey-pos-info nil)

(defvar framey-default-size
  (make-framey-pos-info :width 50 :height 14))

(setq framey-pos-info
      (ht ("*helm semantic/imenu*" (make-framey-pos-info :width 45  :height 20))
          ("*helm mini*"           (make-framey-pos-info :width 100 :height 12))
          ("*helm find files*"     (make-framey-pos-info :width 60  :height 12))))

(defsubst framey--get-buffer-size-info (buffer)
  "Fetches the size info for BUFFER, with `framey-default-size' as fallback."
  (ht-get framey-pos-info (buffer-name buffer) framey-default-size))

(defsubst framey-make-frame ()
  "Create a new framey frame."
   (make-frame
    `((name . "FRAMEY")
      (width . ,framey-default-width)
      (height . ,framey-default-height)
      (minibuffer . t)
      (left-fringe . 2)
      (right-fringe . 2))))

(defvar framey-frame (-let [f (framey-make-frame)]
                       (make-frame-invisible f)
                       f))

(defun framey-kill ()
  "Kill framey."
  (interactive)
  (--each (frame-list)
    (when (string= "FRAMEY" (frame-parameter it 'name))
      (delete-frame it))))

(defun framey-get-or-create ()
  "Get framey.
Create it if needed."
  (unless (frame-live-p framey-frame)
    (setq framey-frame (framey-make-frame)))
  framey-frame)

(defun framey--horizontal-center (frame &optional y-pos)
  "Horizontally center FRAME on screen.
If Y-POS is not given position frame 10% off the top of the screen."
  (let ((scr-w (x-display-pixel-width))
        (scr-h (x-display-pixel-height))
        (frm-w (frame-pixel-width frame)))
    (set-frame-position frame
                        (- (/ scr-w 2) (/ frm-w 2))
                        (or y-pos (round (* 0.1 scr-h))))))

(defun framey--helm-canceller (&rest _)
  "Make framey invisible after a helm action or abort."
  (when (and (frame-live-p framey-frame)
             (frame-visible-p framey-frame))
    (make-frame-invisible framey-frame)))
(add-hook 'helm-quit-hook #'framey--helm-canceller)
(add-hook 'helm-after-action-hook #'framey--helm-canceller)

(defun framey--custom-helm-rule (buffer __alist __plist)
  "Custom shackle rule to show BUFFER using framey."
  (-let [framey (framey-get-or-create)]
    (select-frame framey)
    (-let [[_ height width] (framey--get-buffer-size-info buffer)]
      (set-frame-size framey width height)
      (framey--horizontal-center framey))
    (make-frame-visible framey)
    (delete-other-windows)
    (condition-case _
        (cl-flet ((select-window (&rest _) (ignore)))
          (display-buffer-record-window 'window (selected-window) buffer)
          (set-window-dedicated-p (selected-window) nil)
          (set-window-buffer (select-window) buffer)
          (selected-window))
      (error (framey--helm-canceller)))))

(setq shackle-rules
        '(("*helm-ag*"              :select t   :align right :size 0.5)
          ;; ("*helm semantic/imenu*"  :custom framey--custom-helm-rule  :select t   :align right :size 0.4)
          ("*helm org inbuffer*"    :select t   :align right :size 0.4)
          ("*helm fcomp*"           :select t   :size 1.0)
          (flycheck-error-list-mode :select nil :align below :size 0.25)
          (ert-results-mode         :select t   :align below :size 0.5)
          (calendar-mode            :custom framey--custom-helm-rule :select t   :align below :size 0.25)
          (racer-help-mode          :select t   :align right :size 0.5)
          (help-mode                :select t   :align right :size 0.5)
          (helpful-mode             :select t   :align right :size 0.5)
          (compilation-mode         :select t   :align right :size 0.5)
          ("*Org Select*"           :select t   :align below :size 0.33)
          ("*Org Note*"             :select t   :align below :size 0.33)
          ("*Org Links*"            :select t   :align below :size 0.2)
          (" *Org todo*"            :select t   :align below :size 0.2)
          ("*Man.*"                 :select t   :align below :size 0.5  :regexp t)
          ("*helm.*"                :custom framey--custom-helm-rule :select t   :align below :size 0.33 :regexp t)
          ("*Org Src.*"             :select t   :align below :size 0.5  :regexp t)))

(provide 'framey)

;;; framey.el ends here
