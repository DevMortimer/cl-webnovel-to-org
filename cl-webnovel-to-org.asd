;;;; cl-webnovel-to-org.asd

(asdf:defsystem #:cl-webnovel-to-org
  :description "Scrapes a web novel and puts the content into an org file."
  :author "DevMort"
  :license  "AGPLV3"
  :version "0.0.1"
  :serial t
  :depends-on (#:dexador
	       #:lquery
	       #:local-time)
  :components ((:file "package")
               (:file "cl-webnovel-to-org")))
