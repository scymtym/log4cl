(cl:in-package #:log4cl.visual-appender)

;;; `event'

(defclass message ()
  ((%logger         :initarg :logger
                    :reader  logger)
   (%level          :initarg :level
                    :reader  level)
   (%timestamp      :initarg :timestamp
                    :reader  timestamp)
   (%format-control :initarg :format-control
                    :reader  format-control)
   (%arguments      :initarg :arguments
                    :reader  arguments)))

;;; `session'

(defclass session ()
  ((%messages    :reader   messages
                 :initform (make-array 0 :adjustable t :fill-pointer 0))
   (%change-hook :initarg  :change-hook
                 :accessor change-hook
                 :initform nil)))

;;;

(defmethod add-message! ((container session) (message t))
  (vector-push-extend message (messages container)))

(defmethod add-message! :after ((container session) (message t))
  (when-let ((change-hook (change-hook container)))
    (funcall change-hook (1- (length (messages container))))))

(defmethod clear! ((container session))
  (setf (fill-pointer (messages container)) 0))

(defmethod add-log-data! ((container      session)
                          (logger         t)
                          (level          t)
                          (timestamp      t)
                          (format-control t)
                          (arguments      t))
  (add-message! container (make-instance 'message
                                         :logger         logger
                                         :level          level
                                         :timestamp      timestamp
                                         :format-control format-control
                                         :arguments      arguments)))
