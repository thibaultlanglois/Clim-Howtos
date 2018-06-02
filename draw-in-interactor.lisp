
(defpackage :draw-in-interactor
  (:use :clim-lisp :clim))

(in-package draw-in-interactor)

(define-application-frame draw-in-interactor ()
  ()
  (:panes (in :interactor))
  (:layouts
   (default (vertically () in))))

(define-draw-in-interactor-command (com-quit :name t) ()
  (frame-exit *application-frame*))

(define-draw-in-interactor-command (com-draw-circle :name t) ()
  (let ((pane (get-frame-pane *application-frame* 'in)))
    (with-sheet-medium (medium pane)
      ;; When drawing on a medium, the circle is not recorded and it
      ;; will not appear if the pane is redrawn (when scrolling for example)
      (draw-circle* medium 50 20 10 :ink +blue+))))

(define-draw-in-interactor-command (com-draw-line :name t) ()
  (let ((stream (frame-standard-input *application-frame*)))
    ;; When drawn on a stream the line is recorded and it will be
    ;; automatically redrawn if necessary.
    (draw-line* stream 0 0 50 70 :ink +red+)))

(define-draw-in-interactor-command (com-draw-rectangle :name t) ()
  (let ((stream (frame-standard-input *application-frame*)))
    ;; In order to make the rectangle appear in the flow of commands
    ;; and commands outputs of an interactor, it can be drawn in a
    ;; formatting table.
    (formatting-table (stream :x-spacing 2 :y-spacing 2)
      (formatting-row (stream)
        (formatting-cell (stream)
          (draw-rectangle* stream 0 0 30 20 :ink +yellow+))))))

(defun cl-user::run ()
  (let ((app (make-application-frame 'draw-in-interactor)))
    (run-frame-top-level app)))
;;;----------------------------------------------------------------------
