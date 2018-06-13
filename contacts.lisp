
(defpackage :contacts
  (:use :clim-lisp :clim))

(in-package contacts)

(defparameter *contacts-file*
  "persons.lisp")

;;----------------------------------------------------------------------
(defclass person ()
  ((first-name :accessor first-name :initarg :first-name
               :initform "?")
   (last-name :accessor last-name :initarg :last-name
              :initform "?")
   ;; more slots
   ))

(defun find-person-by-first-name (first-name)
  (find first-name (persons *application-frame*)
        :key #'first-name :test #'equal))

(define-presentation-type person () :options (full))

(define-presentation-method present
    (person (type person) stream
            (view textual-view) &key)
  (cond (full
         (with-text-family (stream :serif)
           (present (first-name person) 'string :stream stream))
         (write-string " " stream)
         (with-text-face (stream :bold)
           (present (last-name person) 'string :stream stream))
         ;; show more info here
         )
        (t
         (with-text-face (stream :bold)
           (present (last-name person) 'string :stream stream))
         (write-string " " stream)
         (with-text-family (stream :serif)
           (present (first-name person) 'string :stream stream)))))

(defparameter *textual-view* (make-instance 'textual-view))

;; This is the accept method:
(define-presentation-method accept
    ((type person) stream (view textual-view) &key)
  (let* ((input (read-token stream))
         (persons (remove-if-not #'(lambda (a) (equal a input))
                                 (persons *application-frame*)
                                 :key #'first-name))
         person)
    (if (= 1 (length persons))
        (setq person (find-person-by-first-name input))
        (progn
          #+nil
          (loop
             for p in persons
             do (present p 'person :stream stream))
          (setq person (accept `(member ,@persons) :stream stream))))
    (when person
      (return-from accept person))
    (input-not-of-required-type input type)))


;;----------------------------------------------------------------------
(define-application-frame contacts ()
  ((persons :initform '() :accessor persons)
   (selection :initform nil :accessor selection)
   (show :initform :all :accessor show))
  (:pointer-documentation t)
  (:panes (app :application
               :display-time :command-loop
               :height 300 :width 400
               :display-function 'contacts-display)
          (in :interactor :height 100 :width 400))
  (:layouts
   (default (vertically () app in))))

(defun contacts-display (frame pane)
  (declare (ignore pane))
  (ecase (show frame)
    (:all (show-all frame))
    ;; (:selection (show-selection frame))
    ))

(define-contacts-command (com-quit :name t) ()
  (frame-exit *application-frame*))

(defun show-all (frame)
  (let ((stream (frame-standard-output frame)))
    (window-clear stream)
    (with-slots (persons) frame
      (formatting-item-list
          (stream :x-spacing 5 :y-spacing 5
                  :n-columns nil :n-rows nil
                  :max-width nil :max-height nil)        
        (loop
           for p in persons
           do
             (formatting-cell (stream)
               (present p '((person) :full t)
                        :stream stream :view *textual-view*
                        :single-box :highlighting)
               #+nil
               (surrounding-output-with-border
                   (stream :shape ':rectangle
                           :padding 1 :ink +foreground-ink+)
                 )))))))

(define-contacts-command (com-show-all :name t) ()
  (setf (show *application-frame*) :all))

(define-contacts-command (com-add-person :name t) ()
  (let ((new (accept 'string
                     :stream (frame-standard-input *application-frame*)
                     :prompt "Enter a name")))
    (push (make-instance 'person :first-name new) (persons *application-frame*))
    (save)))

(define-contacts-command (com-change-last-name :name t) ()
  (let* ((stream (frame-standard-input *application-frame*))
         (person (accept 'person
                         :stream stream
                         :prompt "Enter a person (first name)"))
         (name (accept 'string
                       :stream stream
                       :prompt "Enter last name"))
         (the-person (find person (persons *application-frame*) :test #'equal))
         )
    (cond (the-person
           (setf (last-name person) name)
           (save)))))

(define-contacts-command (com-change-first-name :name t) ()
  (let* ((stream (frame-standard-input *application-frame*))
         (person (accept 'person
                         :stream stream
                         :prompt "Enter a person (first name)"))
         (name (accept 'string
                       :stream stream
                       :prompt "Enter first name"))
         (the-person (find person (persons *application-frame*) :test #'equal)))
    (cond (the-person
           (setf (first-name person) name)
           (save)))))

(defparameter *frame* nil)

(defun cl-user::run ()
  (let ((app (make-application-frame 'contacts)))
    (let ((*application-frame* app))
      (setf (persons app)
            (or (mapcar #'(lambda (l) (from-alist 'person l))
                        (read-value *contacts-file*))
                (list (make-instance 'person
                                     :first-name "Albert"
                                     :last-name "Einstein")
                      (make-instance 'person
                                     :first-name "Albert"
                                     :last-name "Camus")))))
    (setf (selection app) nil)
    (setq *frame* app)
    (run-frame-top-level app)
    ))


;;;----------------------------------------------------------------------


(defmethod to-alist ((p person))
  (list (cons :first-name (first-name p))
        (cons :last-name (last-name p))
        ;; (cons :tags (mapcar #'name (tags p)))
        ))

(defmethod from-alist ((class (eql 'person)) (alist cons))
  (make-instance class
                 :first-name (rest (assoc :first-name alist))
                 :last-name (rest (assoc :last-name alist))
                 ;; :tags
                 #+nil(mapcar #'(lambda (n)
                                   (find-tag n))
                               (rest (assoc :tags alist)))))

(defun read-value (filename &key default-value)
  (multiple-value-bind (result error)
      (ignore-errors
        (with-open-file (f filename :direction :input
                           :if-does-not-exist :error)
          (read f nil default-value)))
    (if error default-value result)))

(defun write-value (value filename &key (external-format :utf-8) header comments)
  "Keywords parameters: 
     :header (t|nil) : includes an emacs-friendly header indicating the 
     coding used and a lispy header indicating the lisp implementation and version.
     :comments : may be a string or a list of strings. If a list each one is 
     printed on a separted line."
  (with-open-file (f filename :direction :output :if-exists :supersede
                     :external-format external-format)
    (let ((*print-readably* t))
      (when header 
        (format f ";; -*- mode: lisp; coding: ~A -*-~%;; Common Lisp implementation: ~A ~A~%"
                (string-downcase (symbol-name external-format))
                (lisp-implementation-type)
                (lisp-implementation-version)))
      (when comments 
        (if (listp comments)
            (loop for line in comments  
               do (format f ";; ~A~%" line))
            (format f ";; ~A~%" comments)))
      (print value f)
      value)))

(defun save ()
  (write-value (mapcar #'to-alist (persons *application-frame*)) *contacts-file*))
