;;;; cl-webnovel-to-org.lisp

(in-package #:cl-webnovel-to-org)

;; variables
(defparameter *page-url* nil) ; the page url to scrape. MAKE SURE TO GIVE THE FIRST CHAPTER
(defparameter *page-use-buttons-p* nil) ; instead of incrementing the url, use the next button
(defparameter *page-last-chapter* nil) ; the last chapter to scrape
(defparameter *page-content-tag* nil) ; the tag of the text body
(defparameter *page-title-tag* nil) ; the tag of the title text
(defparameter *page-button-tag* nil) ; the button tag to get to the next chapter 
(defparameter *org-file* nil) ; the org file to append the content
(defparameter *org-title* nil) ; the title of the novel
(defparameter *org-author* nil) ; the author
(defvar *html-plump* nil) ; the data structure used by lquery

;; macros
(defmacro interactive-when-not-exists (case-value prompt var)
  `(if ,case-value
       (setf ,var ,case-value)
       (progn
	 (format t ,prompt)
	 (setf ,var (read-line)))))

;; functions
(defun get-plump ()
  "Initializes the page to be used by lquery."
  (lquery:$ (initialize (dex:get *page-url*)))
  )

(defun get-next-chapter ()
  "Gets the next chapter."
  (if (string= *page-use-buttons-p* "y")
      (aref (lquery:$ *html-plump* *page-button-tag* (attr :href)) 0)
      nil)
  )

(defun get-content ()
  "Gets the text body of the chapter."
  (aref (lquery:$
	  *html-plump*
	  *page-content-tag* (text)) 0)
  )

(defun get-title ()
  "Gets the title of the chapter."
  (aref (lquery:$
	  *html-plump*
	  *page-title-tag* (text)) 0)
  )

(defun append-to-org ()
  "Appends the content and title to the org file."
  )

(defun setup (&key url use-buttons-p last-chap content-tag title-tag)
  "Setups the needed information on what to scrape.
Will run interactively when not given the arguments."

  (interactive-when-not-exists url "Give the first chapter link: " *page-url*)
  
  (interactive-when-not-exists use-buttons-p "Use the next button to get to the next page (y/n)? " *page-use-buttons-p*)
  (when (or (string= *page-use-buttons-p* "y") (string= *page-use-buttons-p* "t"))
    (format t "What's the tag for the next chapter buttons? ")
    (setf *page-button-tag* (read-line)))
  
  (loop while (not (or (string= *page-use-buttons-p* "y")
		       (string= *page-use-buttons-p* "t")
		       (string= *page-use-buttons-p* "n")
		       (string= *page-use-buttons-p* "nil")))
	do
	   (format t "~%[ERROR] Answer only y/n.~%")
	   (interactive-when-not-exists use-buttons-p "Use the next button to get to the next page (y/n)? " *page-use-buttons-p*))
  
  (interactive-when-not-exists last-chap "Upto how many chapters would you like to scrape? " *page-last-chapter*)
  
  (interactive-when-not-exists content-tag "Now give the tag that houses the content of each chapter: " *page-content-tag*)
  
  (interactive-when-not-exists title-tag "Lastly, give the tag that houses the titles of each chapter: " *page-title-tag*)

  ;; sets the rest of the variables
  (setf *html-plump* (get-plump))
  (format t "What's the title of the novel? ")
  (setf *org-title (read-line))
  (setf *org-file* (concatenate 'string
				*org-title*
				".org"))
  (format t "Who's the author? ")
  (setf *org-author* (read-line))

  ;; initialize the org file
  (with-open-file (stream *org-file* :direction :output)
    (format stream "#+TITLE: ~A~%#+AUTHOR: ~A~%#+DATE: ~A~%#+UID: 0~%~%"
	    *org-title* *org-author*
	    (local-time:format-timestring nil (local-time:now)
					  :format '(:year))))
  
  t
  )
