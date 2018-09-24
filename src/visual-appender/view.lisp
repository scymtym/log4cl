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

(defun display-message (message stream)
  (clim:present message 'message :stream stream))

#+not-needed? (defun display-trace (frame pane)
                #+no (map nil (lambda (event)
                                (display-event event pane)
                                (terpri pane))
                          (events (trace pane)))
                #+no  (let ((history (clim:stream-output-history pane)))
                        (map nil (lambda (event)
                                   (let ((record  (clim:with-output-to-output-record (pane)
                                                    (display-event event pane))))
                                     (setf (clim:output-record-position record)
                                           (values (if (typep (message event) '(or protocol.language-server.connection::request
                                                                                protocol.language-server.connection::notification))
                                                       16
                                                       (- (clim:bounding-rectangle-width pane)
                                                          (clim:bounding-rectangle-width record)
                                                          16))
                                                   (+ (clim:bounding-rectangle-max-y history) 16)))
                                     (clim:add-output-record record history)))
                             (events (trace pane)))
                        (clim:change-space-requirements pane :height (clim:bounding-rectangle-max-y history))
                        (let* ((viewport (climi::sheet-parent pane))
                               (amount   (- (clim:bounding-rectangle-height history)
                                            (clim:bounding-rectangle-height (or viewport pane)))))
                          (clim:scroll-extent pane 0 (max 0 amount)))))

;;; Thanks to jackdaniel in #clim
(defparameter *grid*
  (let ((array (make-array '(10 10) :initial-element 0)))
    (dotimes (v 10)
      (setf (aref array 0 v) 1
            (aref array v 0) 1))
    (let ((pattern (clim:make-pattern array (list clim:+white+ clim:+dark-grey+))))
      (clim:make-rectangular-tile pattern 10 10))))

(defclass log-session-pane (clim:application-pane)
  ((%model        :reader   model
                  :accessor %model)
   (%filter       :accessor filter
                  :initform nil)
   (%auto-scroll? :accessor auto-scroll?
                  :initform t))
  (:default-initargs
   :display-time nil
   ;; :background       *grid*
   ))

(defmethod shared-initialize :after ((instance   log-session-pane)
                                     (slot-names t)
                                     &key
                                     (model nil model-supplied?))
  (when model-supplied?
    (setf (%model instance) model)))

;;;

(defmethod position-for-added-message ((container     log-session-pane)
                                       (event         message)
                                       (output-record t))
  (let ((history (clim:stream-output-history container)))
    (values 16 (+ (clim:bounding-rectangle-max-y history) 16))))

(defmethod scroll-to-bottom ((container log-session-pane))
  (let* ((history  (clim:stream-output-history container))
         (viewport (climi::sheet-parent container))
         (amount   (- (clim:bounding-rectangle-height history)
                      (clim:bounding-rectangle-height (or viewport
                                                          container)))))
    (clim:scroll-extent container 0 (max 0 amount))))

(defmethod add-message! :around ((container log-session-pane) (message t))
  (let ((filter (filter container)))
    (when (or (not filter) (funcall filter message))
      (call-next-method))))

(defmethod add-message! ((container log-session-pane) (message t))
  (let ((history (clim:stream-output-history container))
        (record  (clim:with-output-to-output-record (container)
                   (display-message message container))))
    (setf (clim:output-record-position record)
          (position-for-added-message container message record))
    (clim:add-output-record record history)
    (clim:change-space-requirements container :height (clim:bounding-rectangle-max-y history))
    (when (auto-scroll? container)
      (scroll-to-bottom container))))

(defmethod clear! ((container log-session-pane))
  (clear! (model container))
  (clim:clear-output-record (clim:stream-output-history container))
  (clim:scroll-extent container 0 0))

(defmethod redisplay ((container log-session-pane))
  (clim:clear-output-record (clim:stream-output-history container))

  (let ((filter (filter container)))
    (map nil (lambda (message)
               (when (or (not filter) (funcall filter message))
                 (add-message! container message)))
         (messages (model container)))))

;;; Filtering

(defmethod (setf filter) :after ((new-value t) (pane log-session-pane))
  (redisplay pane))

;;; Change events

(defmethod (setf %model) :after (new-value (object log-session-pane))
  (setf (change-hook new-value)
        (lambda (index)
          (clim:queue-event object (make-instance 'changed-event
                                                  :sheet object
                                                  :index index)))))

(defclass changed-event (climi::standard-event)
  ((index :initarg :index ; TODO carry the event instead of the index?
          :reader  index)))

(defmethod clim:handle-event ((client log-session-pane)
                              (event  changed-event))
  (let* ((messages (messages (model client)))
         (message  (aref messages (index event))))
    (add-message! client message)))
