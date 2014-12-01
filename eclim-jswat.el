;;; eclim-jswat.el --- Eclim interface to JSwat

;; Works only with JSwat 2.x

(mapc #'require '(eclim cl-lib))

(defgroup eclim-jswat nil
  "JSwat Options"
  :group 'tools
  :prefix "eclim-jswat-")

(defcustom eclim-jswat-path nil
  "Path to JSwat installation directory."
  :group 'jswat
  :type 'file)

(defconst eclim-jswat-classpath-separator
  "Platform specific classpath separator."
  (if (eq system-type ''windows-nt)
      ";"
    ":"))

(defconst eclim-jswat-main-class-setting "org.eclim.java.run.mainclass")

(defun eclim-jswat--buffer-name (mode)
  "Returns the buffer name holding the Eclim JSwat process
within a compilation `mode'."
  "*Eclim-Jswat*")

(defun eclim-jswat--launch (cmd)
  "Start JSwat with a given command `cmd'."
  (compilation-start cmd 'compilation-mode 'eclim-jswat--buffer-name))

(defun eclim-jswat--project-classpath ()
  "Get the project classpath."
  (eclim/java-classpath eclim--project-name))

(defun eclim-jswat--main-run-class ()
  "Retrieve the project main class from Eclim settings."
  (eclim/project-setting eclim--project-name eclim-jswat-main-class-setting))

(defun eclim-jswat--dir-path (p-root &rest path-elements)
  "Build a path from a given root `p-root' and relative sub-paths `path-elements.'"
  (cl-reduce '(lambda (x &optional y)
                (concat (file-name-as-directory x) y))
             path-elements :initial-value p-root))

(defun eclim-jswat--project-src-path ()
  "Get the project source path by parsing the .classpath project file."
  (let* ((eclim-prj-classpath-file (eclim-jswat--dir-path (eclim--project-dir) ".classpath"))
         (eclim-prj-classpath (xml-parse-file eclim-prj-classpath-file))
         (eclim-classpath-entries (xml-get-children (car eclim-prj-classpath) 'classpathentry))
         (eclim-prj-sources (mapcar #'(lambda (cp-entry-def)
                                        (let ((cp-entry (second cp-entry-def)))
                                          (when (string= "src" (cdr (assoc 'kind cp-entry)))
                                            (eclim-jswat--dir-path (eclim--project-dir) (cdr (assoc 'path cp-entry))))))
                                    eclim-classpath-entries))
         (non-null-eclim-prj-sources (cl-remove-if #'null eclim-prj-sources)))
    (mapconcat #'identity non-null-eclim-prj-sources eclim-jswat-classpath-separator)))

(defun eclim-jswat--make-command ()
  "Construct the JSwat command line."
  (let* ((java-vm (eclim-jswat--dir-path (getenv "JAVA_HOME") "bin" "java"))
         (jpda-jar (eclim-jswat--dir-path (getenv "JAVA_HOME") "lib" "tools.jar"))
         (java-src-zip (eclim-jswat--dir-path (getenv "JAVA_HOME") "src.zip"))
         (jswat-cmd (concat (file-name-as-directory jswat-path) "jswat"))
         (jswat-src-path (concat java-src-zip eclim-jswat-classpath-separator (eclim-jswat--project-src-path)))
         (jswat-class-path (concat java-src-zip eclim-jswat-classpath-separator (eclim-jswat--project-classpath))))
    (concat
     jswat-cmd
     " 'sourcepath \"" jswat-src-path "\""
     "; classpath \"" jswat-class-path "\""
     "; view \"" (eclim-jswat--main-run-class) "\""
     "; run \"" (eclim-jswat--main-run-class) "\"'")))

;;;###autoload
(defun eclim-jswat-run ()
  "Invokes JSwat using the setup derived from the current Eclim session."
  (interactive)
  (eclim-jswat--launch (eclim-jswat--make-command)))

(provide 'eclim-jswat)

;; eclim-jswat.el ends here
