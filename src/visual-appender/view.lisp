(cl:in-package #:log4cl.visual-appender)

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

(defun display-message (message stream)
  (clim:present message 'message :stream stream))

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
    (clim:repaint-sheet container record)
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
