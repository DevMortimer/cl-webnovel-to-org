;;;; cl-webnovel-to-org.lisp

(in-package #:cl-webnovel-to-org)

;;;----------------------------------------------------------------;
;;;                         Global Variables                       ;
;;;----------------------------------------------------------------;

(defparameter *page-url* nil)
(defparameter *page-last-chapter* nil)
(defparameter *page-title-tag* nil)
(defparameter *page-button-tag* nil)
(defparameter *org-file* nil)
(defparameter *org-title* nil)
(defparameter *org-author* nil)
(defparameter *comments-tag* nil)
(defparameter *stop-after* nil)
(defvar *html-plump* nil)

;;;----------------------------------------------------------------;
;;;                          Helper Macro                          ;
;;;----------------------------------------------------------------;

(defmacro interactive-when-not-exists (prompt case-value var)
  `(if ,case-value
       (setf ,var ,case-value)
       (progn
         (format t ,prompt)
         (finish-output)
         (setf ,var (read-line)))))

;;;----------------------------------------------------------------;
;;;                       Core Scraping Logic                      ;
;;;----------------------------------------------------------------;

(defun get-plump ()
  "Fetches the current URL and initializes the DOM tree."
  (lquery:$ (initialize (dex:get *page-url*))))

(defun query-text (node-or-collection)
  "Safely gets text from a node or collection, returning the first result."
  (when node-or-collection
    (let ((results (lquery:$ node-or-collection (text))))
      (when (> (length results) 0)
        (aref results 0)))))

(defun query-attr (node-or-collection attr)
  "Safely gets an attribute from a node or collection, returning the first result."
  (when node-or-collection
    (let ((results (lquery:$ node-or-collection (attr attr))))
      (when (> (length results) 0)
        (aref results 0)))))

(defun resolve-url (base-url href)
  "Resolves a potentially relative href into a full URL."
  (when href
    (quri:render-uri (quri:merge-uris (quri:uri href) (quri:uri base-url)))))

;;; --- Next Chapter Strategies ---

(defun strategy-user-selector ()
  "Strategy 1: Use the user's provided CSS selector."
  (let* ((nodes (lquery:$ *html-plump* *page-button-tag*))
         (href (query-attr nodes :href)))
    (when href (resolve-url *page-url* href))))

(defun strategy-keyword-search ()
  "Strategy 2: Manually search all links for common 'next' keywords."
  (let ((candidates '("Next" "next" "NEXT" ">" "»" "Next Chapter")))
    (block keyword-search
      (lquery:$ *html-plump* "a"
        (each (lambda (node)
                (let ((text (query-text node)))
                  (when (and text (member text candidates :test #'string-equal))
                    (let ((href (query-attr node :href)))
                      (when href
                        (return-from keyword-search (resolve-url *page-url* href)))))))))))
  nil)

(defun strategy-rel-next ()
  "Strategy 3: Search for a link with the 'rel=next' attribute."
  (let* ((nodes (lquery:$ *html-plump* "a[rel='next']"))
         (href (query-attr nodes :href)))
    (when href (resolve-url *page-url* href))))

(defun strategy-embedded-data ()
  "Strategy 4: Parse embedded JSON data for modern JS-heavy sites."
  (let* ((app-div (lquery:$ *html-plump* "#app"))
         (app-div-json (query-attr app-div "data-page")))
    (when app-div-json
      (handler-case
          (let* ((data (jojo:parse app-div-json :as :alist))
                 (props (cdr (assoc :props data)))
                 (ziggy (cdr (assoc :ziggy props)))
                 (base-url (cdr (assoc :url ziggy)))
                 (routes (cdr (assoc :routes ziggy)))
                 (chapter-route-info (cdr (assoc :|chapter.show| routes)))
                 (uri-template (cdr (assoc :uri chapter-route-info)))
                 (series-info (cdr (assoc :series props)))
                 (next-chapter-info (cdr (assoc :|nextChapter| props)))
                 (series-slug (cdr (assoc :slug series-info)))
                 (next-slug (cdr (assoc :slug next-chapter-info))))
            (when (and base-url uri-template series-slug next-slug)
              (let ((path (ppcre:regex-replace "{series}" uri-template series-slug)))
                (setf path (ppcre:regex-replace "{chapterSlug}" path next-slug))
                (quri:render-uri (quri:merge-uris (quri:uri path) (quri:uri base-url))))))
        (error () nil)))))

(defun get-next-chapter ()
  "Tries to find the next chapter URL using a multi-stage strategy."
  (or (strategy-user-selector)
      (strategy-keyword-search)
      (strategy-rel-next)
      (strategy-embedded-data)))

(defun get-content ()
  "Gets the text body of the chapter."
  (let ((content '())
        (stop nil))
    (dolist (p (coerce (lquery:$ *html-plump* "p") 'list))
      (if stop
          (return)
          (let ((img-src (query-attr (lquery:$ p "img") :src)))
            (if img-src
                (push (format nil "[[~A]]~%~%" (resolve-url *page-url* img-src)) content)
                (when (or (string= *comments-tag* "n")
                          (zerop (length (lquery:$ p (closest *comments-tag*)))))
                  (let ((text (query-text p)))
                    (when (and text (> (length text) 0))
                      (if (and (not (string= *stop-after* "n"))
                               (member text
                                       (split-sequence:split-sequence #\, *stop-after*)
                                       :test #'string=))
                          (setf stop t)
                          (push text content)))))))))
    (format nil "~{~A~%~%~}" (reverse content))))

(defun get-title ()
  "Gets the title of the chapter. Falls back to p > b for some sites."
  (let ((title (query-text (lquery:$ *html-plump* *page-title-tag*))))
    (if (or (null title) (string= title ""))
        (query-text (lquery:$ *html-plump* "p > b"))
        title)))

(defun append-to-org ()
  "Appends the content and title to the org file."
  (with-open-file (stream *org-file* :direction :output :if-exists :append)
    (format stream "* ~A~%~A~%" (get-title) (get-content)))
  t)

;;;----------------------------------------------------------------;
;;;                         Main Program Flow                      ;
;;;----------------------------------------------------------------;

(defun run ()
  "Runs the program until it reaches the last chapter."
  (format t "This may take a while. Please standby... ~%~A~%" *org-title*)
  (finish-output)

  (loop for i from 0 below (parse-integer *page-last-chapter*) do
       (format t "~A...~%" (get-title))
       (finish-output)
       (append-to-org)
       (when (< i (1- (parse-integer *page-last-chapter*)))
         (let ((next-url (get-next-chapter)))
           (if next-url
               (progn
                 (setf *page-url* next-url)
                 (setf *html-plump* (get-plump)))
               (progn
                 (format t "Could not find next chapter link. Stopping.~%")
                 (return))))))

  (format t "~%Done.~%")
  (finish-output)
  t)

(defun setup (&key url button-tag last-chap stop-after comment-tag title-tag novel-title novel-author)
  "Setups the needed information on what to scrape.
Will run interactively when not given the arguments."

  (interactive-when-not-exists "Give the first chapter link: ~%" url *page-url*)
  (interactive-when-not-exists "Give the tag for the next button: ~%" button-tag *page-button-tag*)
  (interactive-when-not-exists "Upto how many chapters would you like to scrape? ~%" last-chap *page-last-chapter*)
  (interactive-when-not-exists "If there are comments, give the class of the div above the comments (n if there isn't): " comment-tag *comments-tag*)
  (interactive-when-not-exists "Stop after encountering a phrase/text? If so what is it (n if there isn't ; separate it with commas if there are multiple hits you want): " stop-after *stop-after*)
  (interactive-when-not-exists "Lastly, give the tag that houses the titles of each chapter: ~%" title-tag *page-title-tag*)
  
  (setf *html-plump* (get-plump))
  
  (interactive-when-not-exists "What's the title of the novel? " novel-title *org-title*)
  (setf *org-file* (concatenate 'string *org-title* ".org"))
  (interactive-when-not-exists "Who's the author? " novel-author *org-author*)

  ;; initialize the org file
  (with-open-file (stream *org-file* :direction :output :if-exists :supersede)
    (format stream "#+TITLE: ~A~%#+AUTHOR: ~A~%#+DATE: ~A~%#+UID: ~A~%#+OPTIONS: html-postamble:nil~%~%"
            *org-title* *org-author*
            (local-time:format-timestring nil (local-time:now) :format '(:year))
            *org-title*))

  (run)
  t)
