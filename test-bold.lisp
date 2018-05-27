(defpackage :test-bold
  (:use :clim-lisp :clim))

(in-package :test-bold)

(define-application-frame test-bold ()
  ()
  (:pointer-documentation t)
  (:panes (app :application
               :display-time nil
               :height 200 :width 400)
          (interactor :interactor :height 200 :width 400))
  (:layouts
   (default (vertically () app interactor))))

(defun run ()
  (run-frame-top-level (make-application-frame 'test-bold)))

(define-test-bold-command (com-parity :name t) ((number 'integer)) 
  (let ((pane (get-frame-pane *application-frame* 'app)))
    (with-sheet-medium (medium pane)
      (with-text-face (medium :bold)
        (draw-text* medium (format nil "~A is ~A~%" number (if (evenp number) 'even 'odd))
                    20 20)))))

(define-test-bold-command (com-quit :name t) ()
  (frame-exit *application-frame*))

