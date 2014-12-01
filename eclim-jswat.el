;;; eclim-jswat.el --- Eclim interface to JSwat

;; Works only with JSwat 2.x

(require 'eclim)
(require 'cl-lib)

(defgroup eclim-jswat nil
  "JSwat Options"
  :group 'tools
  :prefix "eclim-jswat-")

(defcustom eclim-jswat-path nil
  "Path to JSwat installation directory"
  :group 'jswat
  :type 'file)

(defconst eclim-jswat-classpath-separator
  (if (eq system-type ''windows-nt)
      ";"
    ":"))

(defun eclim-jswat-buffer-name (mode)
  "*Eclim-Jswat*")

(defun eclim-jswat-launch (cmd)
  (compilation-start cmd
                     'compilation-mode
                     'eclim-jswat-buffer-name))

(defun eclim-jswat-project-classpath ()
  (eclim/java-classpath eclim--project-name))

(defun eclim-jswat-main-run-class ()
  (eclim/project-setting eclim--project-name "org.eclim.java.run.mainclass"))

(defun eclim-jswat-dir-path (p-root &rest path-elements)
  (cl-reduce '(lambda (x &optional y)
                (concat (file-name-as-directory x) y))
             path-elements :initial-value p-root))

(defun eclim-jswat-project-src-folder ()
  (let* ((eclim-prj-classpath-file (eclim-jswat-dir-path (eclim--project-dir) ".classpath"))
	 (eclim-prj-classpath (xml-parse-file eclim-prj-classpath-file))
       (eclim-classpath-entries (xml-get-children (car eclim-prj-classpath) 'classpathentry))
       (eclim-prj-sources (mapcar #'(lambda (cp-entry)
				      (progn
                                      (let ((cp-kind (cdr (last (car (second cp-entry))))))
                                        (when (string= cp-kind "src")
                                           (cdr (assoc 'path (last (car (last cp-entry)))))))))
                                  eclim-classpath-entries)))
    (jswat-dir-path (eclim--project-dir) (car (remove-if 'null eclim-prj-sources)))))

(defun eclim-jswat-make-command ()
  (let* ((java-vm (eclim-jswat-dir-path (getenv "JAVA_HOME") "bin" "java"))
         (jpda-jar (eclim-jswat-dir-path (getenv "JAVA_HOME") "lib" "tools.jar"))
         (java-src-zip (eclim-jswat-dir-path (getenv "JAVA_HOME") "src.zip"))
         (jswat-cmd (concat (file-name-as-directory jswat-path) "jswat"))
         (proj-sourcepath (eclim-jswat-project-src-folder))
         (jswat-src-path (concat java-src-zip jswat-classpath-separator proj-sourcepath))
         (jswat-class-path (concat java-src-zip jswat-classpath-separator (eclim-jswat-project-classpath))))
    (concat
     jswat-cmd
     " 'sourcepath \"" jswat-src-path "\""
     "; classpath \"" jswat-class-path "\""
     "; view \"" (eclim-jswat-main-run-class) "\""
     "; run \"" (eclim-jswat-main-run-class) "\"'")))

;;;###autoload
(defun eclim-jswat-run ()
  "Invokes JSwat using the setup derived from the current JDE session.
   Assumes that Emacs is configured to use a Unix-like (e.g. bash) shell"
  (interactive)
  ;; Java interpreter
  (progn
    (eclim-jswat-launch (eclim-jswat-make-command))))

(provide 'eclim-jswat)

;; eclim-jswat.el ends here
