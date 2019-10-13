(defsystem cl-slabot
  :version "0.1"
  :license "MIT"
  :depends-on (:hunchentoot
               :cl-json
               :drakma
               :jonathan)
  :components ((:module "src"
                :serial t
                :components
                ((:file "cl-slabot")))))
