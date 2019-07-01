(asdf:defsystem "log4cl.visual-appender"
  :version (:read-file-form "version-string.sexp")
  :depends-on ("alexandria" "log4cl" "mcclim" "clouseau")
  :components ((:module "visual-appender"
                :pathname "src/visual-appender"
                :serial t
                :components ((:file "package")
                             (:file "model")
                             (:file "presentations")
                             (:file "view")
                             (:file "appender")))))
