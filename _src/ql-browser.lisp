;;;; ql-browser.lisp

(in-package #:ql-browser)

(defun make-dist-url (dist)
  (format nil "http://beta.quicklisp.org/dist/quicklisp/~A/distinfo.txt" dist))

(defun subdir (&rest subdirs)
  (let ((string-parts (if (pathnamep (car subdirs)) (cdr subdirs) subdirs))
	(root (if (pathnamep (car subdirs)) (car subdirs) *default-pathname-defaults*)))
    (print string-parts) (print root)
    (merge-pathnames (make-pathname :directory (append (pathname-directory root) string-parts)) root)))

(defun download-directory (dist)
  (subdir "_src" "downloads" dist))

(defun projects-directory ()
  (subdir "_data" "releases"))

(defun create-directories (dist)
  (ensure-directories-exist (subdir (download-directory dist) "releases")))

(defun save-page (url pathname)
  (multiple-value-bind (page code) (drakma:http-request url)
    (if (not (eq code 200)) (error "Call to ~A returned code ~A" url code))
    (with-open-file (s pathname :direction :output :if-exists :supersede :if-does-not-exist :create)
      (princ page s))
    page))

(defun get-dist (dist)
  (save-page (make-dist-url dist) (merge-pathnames "dist.txt" (download-directory dist))))

(defun parse-dist (dist-page)
  (with-input-from-string (s dist-page)
    (read-line s)
    (read-line s)
    (list (subseq (read-line s) 18) (subseq (read-line s) 19))))

(defun download-dist-details (dist)
  (let ((dist-path (download-directory dist))
	(dist-urls (parse-dist (get-dist dist))))
    (list (save-page (first dist-urls) (merge-pathnames "systems.txt" dist-path))
	  (save-page (second dist-urls) (merge-pathnames "releases.txt" dist-path)))))

(defun process-releases (releases-page)
  (with-input-from-string (s releases-page)
    (read-line s)
    (loop for line = (read-line s nil nil) while line collect (parse-release-line line))))

(defun parse-release-line (line)
  (let ((parts (cl-utilities:split-sequence #\space line)))
    (mapcar #'cons '(:name :url) (list (first parts) (second parts)))))

(defun write-release-json (release directory)
  (let ((name (cdr (assoc :name release))))
    (with-open-file (f (merge-pathnames (format nil "~A.json" name) directory) :direction :output)
      (yason:with-output (f :indent T)
	(yason:with-object ()
	  (dolist (x release)
	    (yason:encode-object-element (string-downcase (symbol-name (car x))) (cdr x)))))
      (fresh-line f))))

(defun write-all-release-json (releases)
  (let ((release-dir (subdir "docs" "_data" "releases")))
    (dolist (r releases) (write-release-json r release-dir))))
