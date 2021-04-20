;;;; ql-browser.lisp

(in-package #:ql-browser)

(defun make-dist-url (dist)
  (format nil "http://beta.quicklisp.org/dist/quicklisp/~A/distinfo.txt" dist))

(defun dist-directory (dist)
    (concatenate 'string "_src/downloads/" dist "/"))

(defun make-download-pathname (dist-directory filename)
  (concatenate 'string dist-directory filename))

(defun save-page (url pathname)
  (multiple-value-bind (page code) (drakma:http-request url)
    (if (not (eq code 200)) (error "Call to ~A returned code ~A" url code))
    (with-open-file (s pathname :direction :output :if-exists :supersede :if-does-not-exist :create)
      (princ page s))
    page))

(defun get-dist (dist)
  (save-page (make-dist-url dist) (make-download-pathname (dist-directory dist) "dist.txt")))

(defun parse-dist (dist-page)
  (with-input-from-string (s dist-page)
    (read-line s)
    (read-line s)
    (list (subseq (read-line s) 18) (subseq (read-line s) 19))))

(defun download-dist-details (dist)
  (let ((dist-path (dist-directory dist))
	(dist-urls (parse-dist (get-dist dist))))
    (list (save-page (first dist-urls) (make-download-pathname dist-path "systems.txt"))
	  (save-page (second dist-urls) (make-download-pathname dist-path "releases.txt")))))
