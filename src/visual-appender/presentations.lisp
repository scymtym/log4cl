(cl:in-package #:log4cl.visual-appender)

;;; Log level presentation type

(clim:define-presentation-type log-level ())

(clim:define-presentation-method clim:present ((object t) (type log-level) stream view &key)
  (clim:with-drawing-options (stream :text-face :bold)
    (princ (log4cl:log-level-to-string object) stream)))

;;; Timestamp presentation type

(clim:define-presentation-type timestamp ())

(clim:define-presentation-method clim:present ((object t) (type timestamp) stream view &key)
  (clim:with-drawing-options (stream :ink clim:+dark-violet+)
    (princ object stream)))

;;; Argument presentation type

(clim:define-presentation-type argument ())

(clim:define-presentation-method clim:present ((object t) (type argument) stream view &key)
  (clim:with-drawing-options (stream :text-style (clim:make-text-style :fix nil :small))
    (let ((*print-level* 4)
          (*print-length* 20))
      (princ object stream))))

;;;


(defvar *as-presentation-pprint-dispatch*
  (let* ((default-pprint-dispatch *print-pprint-dispatch*)
         (dispatch                (copy-pprint-dispatch default-pprint-dispatch)))
    (set-pprint-dispatch 't (lambda (stream object)
                              (let ((*print-pprint-dispatch* default-pprint-dispatch)
                                    (stream (sb-pretty::pretty-stream-target stream)))
                                (clim:with-drawing-options (stream :ink clim:+dark-blue+)
                                  (clim:present object 'argument :stream stream ; :view view
                                                                 :single-box t
                                                                 ))))
                         100 dispatch)
    dispatch))

(clim:define-presentation-type message ())

(clim:define-presentation-method clim:present ((object message)
                                               (type   message)
                                               stream
                                               view
                                               &key)
  (clim:surrounding-output-with-border
      (stream :shape                :rounded
              :background           clim:+gray95+
              :highlight-background clim:+lightgray+
              :shadow               clim:+darkgray+)
    (clim:present (level object) 'log-level :stream stream :view view)
    (write-string " " stream)
    (clim:present (timestamp object) 'timestamp :stream stream :view view)
    (terpri stream)
    (clim:surrounding-output-with-border (stream :padding 1
                                                 :background clim:+lightyellow1+)
      (clim:with-text-family (stream :fix)
        (let ((*print-pprint-dispatch* *as-presentation-pprint-dispatch*))
          (funcall (format-control object) stream))))
    #+no (terpri stream)
    #+no (clim:with-text-family (stream :fix)
           (clim:formatting-item-list (stream)
             (loop :for argument :in (rest (arguments object))
                   :do (clim:formatting-cell (stream)
                         (clim:present argument 'argument :stream stream :view view :single-box t)))))))
