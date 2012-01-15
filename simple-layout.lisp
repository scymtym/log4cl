(in-package #:log4cl)

(declaim (inline write-log-level))

(defun write-log-level (level stream)
  "Write the log level name to the stream"
  (write-string (log-level-to-string level) stream)
  (values))

(defmethod layout-to-stream ((layout default-layout)
			     stream
                             logger
			     level
                             log-func)
  "Format the user log statement with the default layout"
  (declare (type stream stream)
           (type fixnum level)
           (type function log-func))
  (write-log-level level stream)
  (loop repeat *log-indent* do (write-string  "  " stream))
  (let ((category (logger-category logger)))
    (when (plusp (length category))
      (write-string " " stream)
      (write-string category stream)))
  (write-string ": " stream)
  (funcall log-func stream)
  (terpri stream)
  (values))

(defmethod layout-to-stream ((layout simple-layout)
			     stream
                             logger
			     level
                             log-func)
  "Format the user log statement with the simple layout"
  (declare (type stream stream)
           (type fixnum level)
           (type function log-func))
  (write-log-level level stream)
  (write-string " - " stream)
  (funcall log-func stream)
  (terpri stream)
  (values))

