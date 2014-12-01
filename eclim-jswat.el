;;; eclim-jswat.el --- Eclim interface to JSwat 2.40.

;; Copyright (C) 2014, Yves Zoundi

;; Version: 0.0.1
;; Keywords: debugger, java, eclim
;; Author: Yves Zoundi <rimerosolutions@gmail.com>
;; Maintainer: Yves Zoundi
;; Package-Requires: ((emacs "24") (cl-lib "0.5") ("emacs-eclim"))
;; Contributors: The internet and people who surf it.
;; Last updated: 2014-12-01

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;  Eclim integration with JSwat debugger version 2.40 only.
;;  This runs JSwat with the following settings:
;;    - The main class of the project.
;;    - The project classpath with the JDK src.zip path prepended.
;;    - The project source path with the JDK src.zip path prepended.
;;    - The main class automatically opened in the JSwat editor to setup breakpoints.
;;
;; You can customize the mode using `M-x customize-group` [RET] eclim-jswat.
;;
;; Add the folder containing eclim-jswat.el.el in your load-path
;; (add-to-list 'load-path "~/.emacs.d/lisp/")
;;
;; (require 'eclim-jswat)
;;
;; Run jswat via `M-x eclim-jswat-run'.

;;; Code:

(mapc #'require '(eclim cl-lib))

(defgroup eclim-jswat nil
  "JSwat Options"
  :group 'tools
  :prefix "eclim-jswat-")

(defcustom eclim-jswat-path nil
  "Path to JSwat 2.x installation directory."
  :group 'jswat
  :type 'file)

(defconst eclim-jswat-classpath-separator
  (if (eq system-type ''windows-nt)
      ";"
    ":")
  "Platform specific classpath separator.")

(defconst eclim-jswat-main-class-setting
  "org.eclim.java.run.mainclass"
  "Eclipse preference setting used by Eclim to determine the project main class.")

(defconst eclim-jswat-msg-configure-path
  "Please configure the `eclim-jwat-path' variable using `M-x customize-group eclim-jswat.'"
  "Error message to configure the JSwat installation folder.")

(defconst eclim-jswat-msg-configure-eclim
  "Ensure that `eclim-mode is enabled and that you're within an Eclipse project."
  "Error message to validate eclim mode configuration.")

(defun eclim-jswat--sanity-check-warnings ()
  (if (null eclim-jswat-path)
      (warn eclim-jswat-msg-configure-path)
    (if (null (eclim--project-dir))
        (warn eclim-jswat-msg-configure-eclim))))

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
         (eclim-prj-classpath      (xml-parse-file eclim-prj-classpath-file))
         (eclim-classpath-entries  (xml-get-children (car eclim-prj-classpath) 'classpathentry))
         (eclim-prj-sources        (mapcar #'(lambda (cp-entry-def)
                                               (let ((cp-entry (second cp-entry-def)))
                                                 (when (string= "src" (cdr (assoc 'kind cp-entry)))
                                                   (eclim-jswat--dir-path (eclim--project-dir) (cdr (assoc 'path cp-entry))))))
                                           eclim-classpath-entries))
         (non-null-eclim-prj-sources (cl-remove-if #'null eclim-prj-sources)))
    (mapconcat #'identity non-null-eclim-prj-sources eclim-jswat-classpath-separator)))

(defun eclim-jswat--make-command ()
  "Construct the JSwat command line."
  (let* ((java-vm          (eclim-jswat--dir-path (getenv "JAVA_HOME") "bin" "java"))
         (jpda-jar         (eclim-jswat--dir-path (getenv "JAVA_HOME") "lib" "tools.jar"))
         (java-src-zip     (eclim-jswat--dir-path (getenv "JAVA_HOME") "src.zip"))
         (jswat-cmd        (concat (file-name-as-directory jswat-path) "jswat"))
         (jswat-src-path   (concat java-src-zip eclim-jswat-classpath-separator (eclim-jswat--project-src-path)))
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
  (when (null (eclim-jswat--sanity-check-warnings))
    (eclim-jswat--launch (eclim-jswat--make-command))))

(provide 'eclim-jswat)

;; eclim-jswat.el ends here
