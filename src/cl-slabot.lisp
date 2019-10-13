(in-package :cl-user)
(defpackage cl-slabot
  (:use :cl
        :hunchentoot
	:cl-json
        :drakma
        :jonathan)
  (:shadow :COOKIE-DOMAIN
           :COOKIE-NAME
           :URL-ENCODE
           :PARAMETER-ERROR
           :COOKIE-EXPIRES
           :COOKIE-VALUE
           :*HEADER-STREAM*
           :COOKIE-PATH
           :WITH-ARRAY
           :WITH-OBJECT)
  (:export :stop-server
	   :start-server
           :post-message
           :get-bot-setting
           :*verification-token*
           :*bot-oauth-token*
           :*acceptor*))
(in-package :cl-slabot)

(defvar *acceptor* nil)
(defvar *path* nil)
(defvar *verification-token* nil)
(defvar *bot-oauth-token* nil)

(defparameter *api-content-type* "application/json; charset=utf-8")
(setq drakma:*drakma-default-external-format* :utf-8)
(setq hunchentoot:*hunchentoot-default-external-format*
      (flex:make-external-format :utf-8 :eol-style :lf))
;(setq hunchentoot:*default-content-type* "text/html; charset=utf-8")

(defun stop-server ()
  (when *acceptor*
    (when (hunchentoot:started-p *acceptor*)
	  (hunchentoot:stop *acceptor*))))

(defun start-server ()
  (stop-server)
  (setf *path*               (get-bot-setting "slabot1" "path"))  
  (setf *verification-token* (get-bot-setting "slabot1" "verification-token"))
  (setf *bot-oauth-token*    (get-bot-setting "slabot1" "bot-oauth-token"))  
  (print *path*)
  (hunchentoot:start (setf *acceptor*
			   (make-instance
			    'hunchentoot:easy-acceptor :port 8080
			    :document-root #p"/path/to/your/html/"))))

;(hunchentoot:define-easy-handler (say-yo :uri "/yo") (name)
;  (setf (hunchentoot:content-type*) "text/plain")
;  (format nil "Hey~@[ ~A~]!" name))

(defun get-setting-hash ()
  (let* ((path (make-pathname :directory (pathname-directory (asdf:system-source-file :cl-slabot))
                              :name "settings"
                              :type "json"))
         (read-list (with-open-file (stream path)
                      (loop for line = (read-line stream nil)
                         while line
                         collect line))))
    (jonathan:parse (apply #'concatenate (cons 'string read-list))
                    :as :hash-table)))

(defun get-bot-setting (name key)
  (dolist (setting (gethash "settings" (get-setting-hash)))
    (let ((registered-name (gethash "name" setting)))
      (when (string= registered-name name)
	(return-from get-bot-setting (gethash key setting)))))
  nil)

(defun post-message(channel text)
  (let* ((drakma:*text-content-types* '(("application" . "json")))
	 (body          (list (cons :channel channel)
                              (cons :text    text)))
         (json-body     (json:encode-json-to-string body))
	 (extra-headers (list (cons "Authorization" (concatenate 'string "Bearer " *bot-oauth-token*)))))
    (multiple-value-bind (body status headers)
	(drakma:http-request "https://slack.com/api/chat.postMessage"
			     :close nil
			     :user-agent          :explorer
                             :method              :post
                             :content-type        *api-content-type*
                             :external-format-out :utf-8
                             :external-format-in  :utf-8
                             :content             json-body
                             :additional-headers  extra-headers))))

(hunchentoot:define-easy-handler (some-handler :uri *path* :default-request-type :post) ()
  (setf (hunchentoot:content-type*) "text/plain; charset=utf-8")
  (let* ((data-string (hunchentoot:raw-post-data :force-text t))
	 (n (json:decode-json-from-string data-string))
	 (token     (cdr (assoc ':token n)))
	 (type      (cdr (assoc ':type n))))
    (if (string= token *verification-token*)
	(cond
	  ((string= type "url_verification")
	   (let* ((challenge (cdr (assoc ':challenge n))))
	     (print challenge)))
	  ((string= type "event_callback")
	   (let* ((event      (cdr (assoc ':event   n)))
		  (event-type (cdr (assoc ':type    event)))
		  (channel    (cdr (assoc ':channel event)))
		  (text       (cdr (assoc ':text    event))))
	     (if (string= event-type "message")
		 (progn (post-message channel text)
			(print "ok"))
		 (setf (hunchentoot:return-code*) 400))))
	  (t
	   (setf (hunchentoot:return-code*) 404)))
	(setf (hunchentoot:return-code*) 403))))
