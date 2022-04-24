(asdf:defsystem :erjoalgo-webutil
                :serial t
                :description "Framework for defining [oauth] http api client & sever."
                :license "GPLv3"
                :author "Ernesto Alfonso <erjoalgo@gmail.com>"
                :depends-on (:cl-json
                             :drakma
                             :cl-ppcre
                             :hunchentoot
                             :gzip-stream
                             :vom
                             :fiasco)
                :components ((:file "packages")
                             (:file "util")
                             (:file "json")
                             (:file "http")
                             (:file "api-client")
                             (:file "defendpoint")
                             (:file "oauth")
                             (:file "site-integrations")))
