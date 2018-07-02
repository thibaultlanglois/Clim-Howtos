(defpackage :formatting-item-list
  (:use :clim-lisp :clim))

(in-package :formatting-item-list)

(define-application-frame formatting-item-list ()
  ((item-gap :initform 3 :accessor item-gap))
  (:panes (app :application
               :display-time :command-loop
               :height 300 :width 400
               :display-function 'formatting-item-list-display)
          (in :interactor :height 100 :width 400))
  (:layouts
   (default (vertically () app in))))

(defun formatting-item-list-display (frame pane)
  (formatting-item-list 
      (pane
       :x-spacing (item-gap frame)
       :y-spacing (item-gap frame)
       ;; the idea is to use the full width of the pane for items rendering:
       :max-width (bounding-rectangle-width (window-viewport pane)))
    (loop
       repeat 1000
       do
         (formatting-cell (pane)
           (draw-rectangle* pane 0 0 10 10 
                            :ink +AQUAMARINE+)))))

(defun cl-user::run ()
  (run-frame-top-level (make-application-frame 'formatting-item-list)))

(define-formatting-item-list-command (com-quit :name t) ()
  (frame-exit *application-frame*))

(define-formatting-item-list-command (com-set-item-gap :name t) ((gap integer))
  (setf (item-gap *application-frame*) gap))

(define-formatting-item-list-command (com-redisplay :name t) ()
  )



