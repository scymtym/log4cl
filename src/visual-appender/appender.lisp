(cl:in-package #:log4cl.visual-appender)

;;; `log-session'

(clim:define-application-frame log-session ()
  ((%model :initarg :model
           :reader  model))
  (:panes
   (messages              log-session-pane)
   (interactor            :interactor)
   (pointer-documentation :pointer-documentation))
  (:layouts
   (default (clim:vertically ()
              (:fill (clim:scrolling () messages)) ; TODO name
              (1/16  interactor)
              (1/32  pointer-documentation))))
  (:default-initargs
   :title "Log Session"))

(defmethod clim:layout-frame :after ((frame log-session) &optional width height)
  (declare (ignore width height))
  (let ((messages (clim:find-pane-named frame 'messages)))
    (setf (%model messages) (model frame))))

(define-log-session-command inspect ((object t))
  (uiop:symbol-call '#:clouseau '#:inspector object :new-process t))

(clim:define-presentation-to-command-translator
    document->inspect (argument inspect log-session
                       :documentation "Inspect the argument")
    (object)
  (list object))

;;; `appender'

(defclass appender (log4cl:appender)
  ((%model :initarg  :model
           :reader   model
           :initform (make-instance 'session))
   (%frame :accessor frame
           :initform nil)))

(defmethod log4cl:appender-added :after ((logger t) (appender appender))
  (when (= (log4cl:appender-logger-count appender) 1)
    (let ((frame (clim:make-application-frame 'log-session
                                              :width  600
                                              :height 800
                                              :model  (model appender))))
      (setf (frame appender) frame)
      (bt:make-thread (lambda ()
                        (clim:run-frame-top-level frame))))))

(defmethod log4cl:appender-do-append ((appender  appender)
                                      (logger    t)
                                      (level     t)
                                      (log-func  t)
                                      (arguments t))
  ;; HACK this relies on removing the dx declaration at the point
  ;; where log-func originally escapes
  (add-log-data! (model appender) logger level (log4cl::log-event-time) log-func arguments))
