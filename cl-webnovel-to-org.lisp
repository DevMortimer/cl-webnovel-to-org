;;;; cl-webnovel-to-org.lisp

(in-package #:cl-webnovel-to-org)

;;;----------------------------------------------------------------;
;;;                        Scraper Configuration                   ;
;;;----------------------------------------------------------------;

(defclass scraper-config ()
  ((url
    :initarg :url
    :accessor url)
   (last-chapter
    :initarg :last-chap
    :accessor last-chapter)
   (title-tag
    :initarg :title-tag
    :accessor title-tag)
   (button-tag
    :initarg :button-tag
    :accessor button-tag)
   (comments-tag
    :initarg :comment-tag
    :accessor comments-tag)
   (stop-after
    :initarg :stop-after
    :accessor stop-after)
   (novel-title
    :initarg :novel-title
    :accessor novel-title)
   (novel-author
    :initarg :novel-author
    :accessor novel-author)
   (org-file
    :accessor org-file)
   (current-dom
    :accessor current-dom
    :documentation "The parsed DOM of the current page.")))

;;;----------------------------------------------------------------;
;;;                        Helper Functions                        ;
;;;----------------------------------------------------------------;

(defun safe-query (dom selector &key (attribute nil))
  "Safely queries a DOM node for a selector and optionally an attribute.
  Returns the value or NIL if not found."
  (let ((nodes (lquery:$ dom selector)))
    (when (plusp (length nodes))
      (if attribute
          (aref (lquery:$ nodes (attr attribute)) 0)
          (aref (lquery:$ nodes (text)) 0)))))

(defun make-absolute-url (base-url new-path)
  "Constructs an absolute URL from a base URL and a new path."
  (let ((base-uri (quri:uri base-url)))
    (quri:render-uri (quri:merge-uris (quri:uri new-path) base-uri))))

(defun prompt-for-input (prompt &key (validator #'identity) (error-message "Invalid input."))
  "Prompts the user for input and validates it."
  (loop
     (format t prompt)
     (finish-output)
     (let ((input (read-line)))
       (when (funcall validator input)
         (return input))
       (format t "~A~%" error-message))))

(defun is-integer-string (str)
  "Returns true if the string represents a valid integer."
  (and (plusp (length str)) (every #'digit-char-p str)))

;;;----------------------------------------------------------------;
;;;                        Scraping Logic                        ;
;;;----------------------------------------------------------------;

(defun fetch-page (config)
  "Fetches the content of the URL in the config and parses it."
  (handler-case
      (setf (current-dom config) (lquery:$ (initialize (dex:get (url config)))))
    (dex:http-request-failed (e)
      (format t "Error fetching URL ~A: ~A~%" (url config) e)
      nil)))

(defun get-chapter-title (config)
  "Gets the title of the chapter."
  (or (safe-query (current-dom config) (title-tag config)) "Untitled"))

(defun get-next-chapter-url (config)
  "Gets the URL for the next chapter."
  (let* ((nodes (lquery:$ (current-dom config) (button-tag config)))
         (href (when (plusp (length nodes))
                 (let ((direct-href (safe-query nodes nil :attribute :href))
                       (child-href (safe-query nodes "a" :attribute :href)))
                   (or direct-href child-href)))))
    (when href
      (make-absolute-url (url config) href))))

(defun get-chapter-content (config)
  "Gets the text body of the chapter, including images."
  (let ((content '())
        (stop-phrases (if (string= "n" (stop-after config))
                          '()
                          (split-sequence:split-sequence #\, (stop-after config)))))
    (dolist (p-node (coerce (lquery:$ (current-dom config) "p") 'list))
      ;; Handle images
      (let ((img-src (safe-query p-node "img" :attribute :src)))
        (when img-src
          (push (format nil "[[~A]]" (make-absolute-url (url config) img-src)) content)
          (return))) ; Assume paragraph with image has no text

      ;; Handle text
      (let ((text (safe-query p-node nil)))
        (when (and text (plusp (length text)))
          (when (member text stop-phrases :test #'string=)
            (return-from get-chapter-content (format nil "~{~A~%~%~}" (reverse content))))

          (when (or (string= "n" (comments-tag config))
                    (zerop (length (lquery:$ p-node (closest (comments-tag config))))))
            (push text content)))))
    (format nil "~{~A~%~%~}" (reverse content))))

(defun append-to-org-file (config title content)
  "Appends the content and title to the org file."
  (with-open-file (stream (org-file config) :direction :output :if-exists :append)
    (format stream "* ~A~%~A~%" title content)))

;;;----------------------------------------------------------------;
;;;                         Main Program Flow                      ;
;;;----------------------------------------------------------------;

(defun run-scraper (config)
  "Runs the scraper until it reaches the last chapter."
  (format t "Starting scrape of '~A'. This may take a while...~%" (novel-title config))
  (finish-output)

  (loop for i from 1 to (parse-integer (last-chapter config)) do
       (unless (fetch-page config)
         (format t "Failed to fetch page, stopping.~%")
         (return))

       (let ((title (get-chapter-title config))
             (content (get-chapter-content config)))
         (format t "Scraping Chapter ~A: ~A...~%" i title)
         (finish-output)
         (append-to-org-file config title content))

       (when (< i (parse-integer (last-chapter config)))
         (let ((next-url (get-next-chapter-url config)))
           (if next-url
               (setf (url config) next-url)
               (progn
                 (format t "Could not find next chapter link. Stopping.~%")
                 (return))))))

  (format t "~%Done. Scraped ~A chapters into ~A~%"
          (last-chapter config) (org-file config)))

(defun initialize-org-file (config)
  "Creates and initializes the org file with a header."
  (setf (org-file config) (concatenate 'string (novel-title config) ".org"))
  (with-open-file (stream (org-file config) :direction :output :if-exists :supersede)
    (format stream "#+TITLE: ~A~%#+AUTHOR: ~A~%#+DATE: ~A~%#+OPTIONS: html-postamble:nil~%~%"
            (novel-title config)
            (novel-author config)
            (local-time:format-timestring nil (local-time:now) :format '(:year)))))

(defun setup (&key url button-tag last-chap stop-after comment-tag title-tag novel-title novel-author)
  "Sets up the needed information for scraping, then runs the scraper.
   Will run interactively if arguments are not provided."
  (let ((config
         (make-instance 'scraper-config
           :url (or url (prompt-for-input "Enter the first chapter URL: "))
           :button-tag (or button-tag (prompt-for-input "Enter the CSS selector for the next button: "))
           :last-chap (or last-chap (prompt-for-input "How many chapters to scrape? "
                                                     :validator #'is-integer-string
                                                     :error-message "Please enter a valid number."))
           :comment-tag (or comment-tag (prompt-for-input "Enter the CSS selector for the comments section (or 'n'): "))
           :stop-after (or stop-after (prompt-for-input "Enter text to stop at (or 'n', comma-separated): "))
           :title-tag (or title-tag (prompt-for-input "Enter the CSS selector for the chapter title: "))
           :novel-title (or novel-title (prompt-for-input "Enter the novel's title: "))
           :novel-author (or novel-author (prompt-for-input "Enter the novel's author: ")))))

    (initialize-org-file config)
    (run-scraper config)
    t))
