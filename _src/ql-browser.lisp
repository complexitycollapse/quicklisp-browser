;;;; ql-browser.lisp

(in-package #:ql-browser)

(defvar *stopwords*)

(defun init ()
  (setf *stopwords* (load-stopwords)))

(defun make-dist-url (dist)
  (format nil "http://beta.quicklisp.org/dist/quicklisp/~A/distinfo.txt" dist))

(defun subdir (&rest subdirs)
  (let ((string-parts (if (pathnamep (car subdirs)) (cdr subdirs) subdirs))
	(root (if (pathnamep (car subdirs)) (car subdirs) *default-pathname-defaults*)))
    (merge-pathnames (make-pathname :directory (append (pathname-directory root) string-parts)) root)))

(defun download-directory (dist)
  (subdir "_src" "downloads" dist))

(defun projects-directory ()
  (subdir "docs" "_data" "releases"))

(defun prepare-directories (dist)
  (cl-fad:delete-directory-and-files (subdir "docs" "_data" "releases") :if-does-not-exist :ignore)
  (ensure-directories-exist (subdir (download-directory dist) "releases"))
  (ensure-directories-exist (projects-directory)))

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

(defun json-complete (json-pathname)
  (with-open-file (s json-pathname :if-does-not-exist nil)
    (if s (gethash "complete" (yason:parse s)))))

(defun construct-and-write-release-json (name json-pathname release)
  (format T "Downloading release ~A. " name)
  (let* ((repo (second (get-repo name)))
	 (complete (json-complete json-pathname))
	 (is-github (is-github-repo repo))
	 (readme (if (and is-github (not complete)) (get-readme repo)))
	 (keywords (if readme (get-top-words (get-keywords readme)))))
    (when complete
      (format T "Skipping ~A" name)
      (return-from construct-and-write-release-json))
    (write-release-json json-pathname
			(append release (list (cons "repository" repo) (cons "keywords" keywords) (cons "complete" "true")))))
  (fresh-line))

(defun write-release-json (json-pathname key-value-pairs)
  (with-open-file (f json-pathname :direction :output :if-does-not-exist :create :if-exists :supersede)
    (yason:with-output (f :indent T)
      (yason:with-object ()
	(dolist (x key-value-pairs)
	  (let ((key (if (stringp (car x)) (car x) (string-downcase (symbol-name (car x)))))
		(value (cdr x)))
	    (if (listp value)
		(yason:with-object-element (key) (yason:with-array () (dolist (v value) (yason:encode-array-element v))))
		(yason:encode-object-element key value))))))))

(defun for-all-release-json (releases fn)
  (let ((release-dir (projects-directory)))
    (dolist (release releases)
      (let* ((name (cdr (assoc :name release)))
	     (json-pathname (merge-pathnames (format nil "~A.json" name) release-dir)))
	(funcall fn name json-pathname release)))))

(defun write-all-release-json (releases)
  (for-all-release-json releases #'construct-and-write-release-json))

(defun get-repo (release)
  (with-open-file (s (merge-pathnames "source.txt" (subdir "quicklisp-projects" "projects" release)))
    (cl-utilities:split-sequence #\space (read-line s))))

(defun generate (releases-page)
  (write-all-release-json (process-releases releases-page)))

(defun do-whole-dist (dist)
  (prepare-directories dist)
  (generate (second (download-dist-details dist))))

(defun get-readme (project-url)
  (let ((readme (or (get-github-file project-url "README.md")
		    (get-github-file project-url "README.txt")
		    (get-github-file project-url "README"))))
    (cond (readme (format T "Got README for ~A" project-url))
	  (T (format T "Could not find README for ~A" project-url)))
    (sleep 60)
    readme))

(defun is-github-repo (project-url)
  (string= "https://github.com" project-url :end2 18))

(defun get-github-file (project-url filename)
  (destructuring-bind (owner repo &rest _) (cl-utilities:split-sequence #\/ project-url :start 19)
    (declare (ignore _))
    (let ((url (format nil "https://api.github.com/repos/~A/~A/contents/~A" owner (subseq repo 0 (- (length repo) 4)) filename)))
      (multiple-value-bind (page code) (drakma:http-request url)
	(format T "~A (~A) " url code)
	(if (eq 200 code)
	    (let* ((contents (yason:parse (stringify page)))
		   (readme-url (gethash "download_url" contents)))
	      (multiple-value-bind (page code) (drakma:http-request readme-url)
		(if (eq 200 code) (stringify page)))))))))

(defun stringify (array)
  (if (stringp array) array
      (let* ((length (car (array-dimensions array)))
	     (str (make-string length)))
	(dotimes (i length str)
	  (setf (aref str i) (code-char (aref array i)))))))

(defun get-keywords (str &optional (stopwords *stopwords*))
  (let ((pairs (hash-to-alist (extract-words str))))
      (setf pairs (remove-if (lambda (p) (nth-value 1 (gethash (car p) stopwords))) pairs))
      (sort pairs (lambda (a b) (> (cdr a) (cdr b))))))

(defun hash-to-alist (hash)
  (let ((pairs nil))
    (maphash (lambda (a b) (push (cons a b) pairs)) hash)
    (nreverse pairs)))

(defun extract-words (str)
  (let* ((text
	  (substitute-if #\space (lambda (c) (find c '(#\. #\newline #\, #\+ #\# #\; #\: #\tab #\( #\) #\\
						       #\/ #\" #\[ #\] #\{ #\} #\= #\! #\? #\< #\> #\| #\* #\` #\0 #\1 #\2 #\3 #\4
						       #\5 #\6 #\7 #\8 #\9)))
			 str))
	 (words (remove-if
		 (lambda (w) (find #\- w))
		 (mapcar (cl-utilities:compose #'string-downcase (lambda (x) (string-trim '(#\') x)))
			 (cl-utilities:split-sequence #\space text :remove-empty-subseqs T))))
	 (hash (make-hash-table :test 'equal)))
    (dolist (word words)
      (cond ((parse-integer word :junk-allowed T))
	    ((eq 1 (length word)))
	    ((gethash word hash) (incf (gethash word hash)))
	    (T (setf (gethash word hash) 1))))
    hash))

(defun load-stopwords ()
  (with-open-file (s (merge-pathnames "stoplist.txt" (subdir "_src")))
    (extract-words (with-output-to-string (o) (loop for c = (read-char s nil nil) while c do (write-char c o))))))

(defun get-top-words (word-pairs &optional (count 10) (min (/ (cdar word-pairs) 3)))
  (cond ((endp word-pairs) nil)
	((zerop count) nil)
	((< (cdar word-pairs) min) nil)
	(T (cons (caar word-pairs) (get-top-words (cdr word-pairs) (1- count) min)))))
