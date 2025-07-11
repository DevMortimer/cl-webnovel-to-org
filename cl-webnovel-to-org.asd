;;;; cl-webnovel-to-org.asd

(asdf:defsystem #:cl-webnovel-to-org
  :description "Scrapes a web novel and puts the content into an org file."
  :author "DevMort"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:dexador
	       #:lquery
	       #:local-time
	       #:split-sequence
	       #:quri
	       #:jonathan
	       #:cl-ppcre)
  :components ((:file "package")
               (:file "cl-webnovel-to-org")))
