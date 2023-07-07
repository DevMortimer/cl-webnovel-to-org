;;;; cl-webnovel-to-org.lisp

(in-package #:cl-webnovel-to-org)

;; variables
(defparameter *page-url* nil) ; the page url to scrape. MAKE SURE TO GIVE THE FIRST CHAPTER
(defparameter *page-last-chapter* nil) ; the last chapter to scrape
(defparameter *page-content-tag* nil) ; the tag of the text body
(defparameter *page-title-tag* nil) ; the tag of the title text
(defparameter *page-button-tag* nil) ; the button tag to get to the next chapter
(defparameter *page-full-of-p-p* nil) ; indicator if the page is full of <p>
(defparameter *org-file* nil) ; the org file to append the content
(defparameter *org-title* nil) ; the title of the novel
(defparameter *org-author* nil) ; the author
(defvar *html-plump* nil) ; the data structure used by lquery

;; macros
(defmacro interactive-when-not-exists (case-value var)
  `(if ,case-value
       (setf ,var ,case-value)
       (setf ,var (read-line))))

;; functions
(defun get-plump ()
  "Initializes the page to be used by lquery."
  (lquery:$ (initialize (dex:get *page-url*)))
  )

(defun get-hostname-from-url (url)
  (if (ppcre:scan-to-strings "^https?://([^/]+)" url)
      (aref (nth-value 1 (ppcre:scan-to-strings "^https?://([^/]+)" url)) 0)))
      

(defun get-next-chapter ()
  "Gets the next chapter."
  (let ((next-chap (aref (lquery:$ *html-plump* *page-button-tag* (attr :href)) 0)))
    (if (ppcre:scan "^https?://" next-chap)
	next-chap
	(concatenate 'string
		     "https://"
		     (get-hostname-from-url *page-url*)
		     next-chap)))
  )

(defun get-content ()
  "Gets the text body of the chapter."
  (if (not *page-full-of-p-p*)
      (aref (lquery:$
	     *html-plump*
	     *page-content-tag* (text)) 0)
      (let ((paragraphs (remove-if
			 #'(lambda (s)
			     (or (search "http" s :test 'char-equal)
				 (search "©" s :test 'char-equal)
				 (cl-ppcre:scan "^\\*+[\\* ]*$" s)))
			 (lquery:$ *html-plump* "p" (text)))))
	(format nil "~{~A~%~%~}"
		(coerce paragraphs 'list))))
  )

(defun get-title ()
  "Gets the title of the chapter."
  (aref (lquery:$
	  *html-plump*
	  *page-title-tag* (text)) 0)
  )

(defun append-to-org ()
  "Appends the content and title to the org file."
  (with-open-file (stream *org-file* :direction :output :if-exists :append)
    (format stream
	    "* ~A~%~A~%" (get-title) (get-content)))
  
  t
  )

(defun run ()
  "Runs the program until it reaches the last chapter."
  (format t "This may take a while. Please standby... ~%~A~%" *org-title*)
  (finish-output)
  
  (loop for i from 0 below (parse-integer *page-last-chapter*) do
    (format t "~A...~%" (get-title))
    (finish-output)
    (append-to-org)
    (when (not (= i (1- (parse-integer *page-last-chapter*))))
      (setf *page-url* (get-next-chapter))
      (setf *html-plump* (get-plump))))

  ;; TODO: when not using buttons? idk if needed... hmmm

  (format t "~%Done.~%")
  (finish-output)
  
  t
  )

(defun setup (&key url last-chap content-tag title-tag)
  "Setups the needed information on what to scrape.
Will run interactively when not given the arguments."

  (format t "Give the first chapter link: ~%")
  (finish-output)
  (interactive-when-not-exists url *page-url*)

  (format t "What's the tag for the next chapter buttons? ")
  (finish-output)
  (setf *page-button-tag* (read-line))
  
  (format t "Upto how many chapters would you like to scrape? ~%")
  (finish-output)
  (interactive-when-not-exists last-chap  *page-last-chapter*)

  (format t "Now give the tag that houses the content of each chapter: ~%")
  (finish-output)
  (interactive-when-not-exists content-tag  *page-content-tag*)

  (format t "Lastly, give the tag that houses the titles of each chapter: ~%")
  (finish-output)
  (interactive-when-not-exists title-tag *page-title-tag*)

  ;; sets the rest of the variables
  (loop while (not (or (string= *page-full-of-p-p* "y")
		       (string= *page-full-of-p-p* "n")))
	do
	   (format t "Is the page full of <p>? This is important. (y/n): ")
	   (finish-output)
	   (setf *page-full-of-p-p* (read-line)))
				
  (setf *html-plump* (get-plump))
  (format t "What's the title of the novel? ")
  (finish-output)
  (setf *org-title* (read-line))
  (setf *org-file* (concatenate 'string
				*org-title*
				".org"))
  (format t "Who's the author? ")
  (finish-output)
  (setf *org-author* (read-line))

  ;; initialize the org file
  (with-open-file (stream *org-file* :direction :output)
    (format stream "#+TITLE: ~A~%#+AUTHOR: ~A~%#+DATE: ~A~%#+UID: 0~%~%"
	    *org-title* *org-author*
	    (local-time:format-timestring nil (local-time:now)
					  :format '(:year))))

  (run)
  
  t
  )
