(cl:in-package #:log4cl.visual-appender)


(defclass appender ()
  ((%model :initarg :model
           :reader  model)))

(defmethod log4cl:appender-do-append ((appender appender)
                                      (logger   t)
                                      (level    t)
                                      (log-func t)
                                      (argumnts t))
  (add-log-data! (model appender) logger (log4cl::log-event-time) arguments))
