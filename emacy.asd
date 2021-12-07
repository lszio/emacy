(defsystem "emacy"
  :version "0.1.0"
  :author "Liszt21"
  :license ""
  :depends-on ("str"
               "log4cl"
               "clish" ;; Liszt21/Clish
               )
  :components ((:module "src"
                :components
                ((:file "emacy"))))
  :description ""
  :in-order-to ((test-op (test-op "emacy/tests"))))

(defsystem "emacy/tests"
  :author ""
  :license ""
  :depends-on ("emacy"
               "fiveam")
  :components ((:module "tests"
                :components
                ((:file "emacy"))))
  :description "Test system for emacy"
  :perform (test-op (op c) (symbol-call :fiveam :run! :emacy)))
