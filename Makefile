build:
	sbcl --eval '(ql:quickload :cl-webnovel-to-org)' \
	--eval "(sb-ext:save-lisp-and-die #p\"cl-webnovel-to-org\" :executable t :compression 10 :toplevel #'cl-webnovel-to-org:setup)"

install: build
	mkdir -p ~/.local/bin/
	mv cl-webnovel-to-org ~/.local/bin/

clean:
	rm cl-webnovel-to-org
