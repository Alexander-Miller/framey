;;; framey.el --- TODO -*- lexical-binding: t -*-
;;;

;;; Commentary:
;; TODO

;;; Code:

(require 'dash)
(require 'shackle)
(require 'helm)
(require 'posframe)
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

(defvar framey--shackle-rule '("*helm.*" :custom framey--custom-helm-rule :regexp t))
(defvar framey--shackle-help-rule '(helpful-mode :custom framey--custom-help-rule))

(defconst framey-pos-info
  (ht ("*helm semantic/imenu*"  (make-framey-pos-info :height 20 :width 45))
      ("*helm mini*"            (make-framey-pos-info :height 12 :width 100))
      ("*helm-ag*"              (make-framey-pos-info :height 25 :width 120))
      ("*helm-xref*"            (make-framey-pos-info :height 25 :width 100))
      ("*helm find files*"      (make-framey-pos-info :height 12 :width 60))
      ("*helm locate library*"  (make-framey-pos-info :height 25 :width 100))
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

(defun framey--poshandler (info)
  "Framey's position handler.
Sets the frame in the upper center based on INFO."
  (cons (/ (- (plist-get info :parent-frame-width)
              (plist-get info :posframe-width))
           2)
        (round (* 0.05 (plist-get info :parent-frame-height)))))

;;; Helpful -----------------------------------

(defun framey--custom-help-rule (buffer __alist __plist)
  "Custom shackle rule to show helpful BUFFER using framey."
  (-let [[_ height width] (framey--get-buffer-size-info buffer)]
    (posframe-show
     buffer
     :poshandler #'framey--poshandler
     :respect-header-line t
     :min-width width
     :min-height height
     :internal-border-width 2
     :internal-border-color "#1D1D1D"
     :override-parameters '((no-accept-focus . nil)))
    (-let [frame (buffer-local-value 'posframe--frame buffer)]
      (setf truncate-lines t)
      (select-frame frame)
      (selected-window))))

(with-eval-after-load 'helpful
  (with-no-warnings
    (define-key helpful-mode-map [remap quit-window] #'framey-quit-window)))

;;; Helm -----------------------------------

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
  (-let* ((buffer (get-buffer buffer-name))
          ([_ height width] (framey--get-buffer-size-info buffer)))
    (posframe-show
     buffer
     :poshandler #'framey--poshandler
     :respect-header-line t
     :min-width width
     :min-height height
     :internal-border-width 2
     :internal-border-color "#1D1D1D")
    (with-current-buffer buffer
      (setq-local truncate-lines t))))

;;;###autoload
(define-minor-mode framey-mode
  ""
  :init-value nil
  :global     t
  :lighter    nil
  (if framey-mode
      (progn
        (setf helm-display-function #'framey--display-helm)
        (add-to-list 'shackle-rules framey--shackle-help-rule)
        (advice-add 'helm-cleanup :around #'framey--helm-cleanup)
        (advice-add 'helm-execute-persistent-action :around #'framey--helm-persistent-action-advice))
    (setf helm-display-function #'framey--display-helm)
    (setf shackle-rules (delete framey--shackle-help-rule shackle-rules))
    (advice-remove 'helm-cleanup #'framey--helm-cleanup)
    (advice-remove 'helm-execute-persistent-action #'framey--helm-persistent-action-advice)))

(provide 'framey)

;;; framey.el ends here
