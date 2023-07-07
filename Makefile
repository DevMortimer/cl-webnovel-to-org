build:
  sbcl --eval '(ql:quickload :cl-webnovel-to-org)' \
  --eval "(sb-ext:save-lisp-and-die #p\"cl-webnovel-to-org\" :executable t :compression 22 :toplevel #'cl-webnovel-to-org:run-binary)"

clean:
  rm cl-webnovel-to-org
