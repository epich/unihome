;; For my projects

;; Classpath for JDEE
;;
;; To get a decent classpath from an Ant build system, hack:
;;   <echo message="Used test.run.classpath ${toString:test.run.classpath}"/>
;; Then massage that output into something like:
;;   (defvar my-classpath (quote (
;;           "path/to/a.jar"
;;           "path/to/another.jar"
;;           )) "Path for project .class or .jar files.")

;; Sourcepath for JDEE
;;   find . -name "*.java"
;;   Used Emacs to delete package directory structure
;;   Piped into sort then uniq
;;
;;   (defvar my-sourcepath (quote (
;;           "path/to/java/src"
;;           "path/to/java/src"
;;           )) "Path for project .java files.")

;; No longer needed: 
;; (defvar my-rhel-release 5 "Major release of RHEL. ")
;; (defvar goesr-sdf-ots-path
;;         (let (sdf-ots-path (getenv "SDF_OTS_PATH")) (if sdf-ots-path sdf-ots-path "/goesr/user/build/3rdparty/OTS"))
;;         "Path to GOESR OTS (Off the Shelf) software. ")

(defvar my-project-root
        (or ;; TODO: Do better: find if emacs/COPYING exists to reduce false positives, find emacs-* dirs
            ;; TODO: Find root based on .git, .hg, .bzr
            (my-find-file-upwards "emacs")
            "unihome" "OTS" "trunk" "sw")
        "Path to current project. " )
(require 'google-project nil t)

(defun my-proj-emacs-init ()
  (setq my-offset 2)
  ;; TODO: DRY
  (setq evil-shift-width my-offset)
  (setq tab-stop-list (cdr (number-sequence 0 256 my-offset)))
  (setq c-basic-offset my-offset)
  (setq-default c-default-style "gnu"))

;; Inside ede-expand-filename-impl name=BasicStatistics.h 
;; Inside ede-expand-filename-impl string dir=(void) 
;; Inside ede-expand-filename-impl ans=/psd15/linux/boreilly/cc/current/vobs/goes_r_dev/Software_Engineering/IPT_PG/Dev/src/ProductPerformance/ProductQualityMonitor/ProductQualityDatabase_cpp/inc/BasicStatistics.h
(defun my-locate-goesr-cxx-fcn (name dir)
  "Find file for GOESR EDE project.

NAME is the name of the file to find, basename including extension.  DIR is the system-absolute root of the project, with trailing / character. "
  )

(defun my-get-goesr-include-path ()
  "Get the include paths for GOESR C++ development as a list. "
  (let ((includes-buffer "*goesr-cxx-include-path*"))
    ;; Kill buffer if it already exists, so as we don't create duplicates everytime
    ;; this function is executed
    (when (bufferp (get-buffer includes-buffer))
      (kill-buffer includes-buffer))
    ;; Validate that my-project-root exists, for a modicum of security
    (if (not (file-exists-p my-project-root))
        nil
      (call-process "bash"
                    nil includes-buffer nil
                    "-c"
                    ;; We exclude Proxy_Data_Generator_CSC because it has wholesale copies of L0Processing code,
                    ;; which we don't want to mistakenly incorporate into semanticdb.
                    ;;
                    ;; INRQualityMonitor has DRY-principle-violating wholesale copies of L2Processing code.
                    (format "find %s/src -type d -name inc -o -name test -o -name cpp | grep -v Proxy_Data_Generator_CSC | grep -v INRQualityMonitor | sed 's|%s||'"
                            my-project-root my-project-root))
      ;; Safer way of invoking find, but can't use pipe syntax
      ;; (call-process "find" 
      ;;               nil includes-buffer nil
      ;;               (format "%s/src" my-project-root)
      ;;               (format "%s/AlgorithmTestTools" my-project-root)
      ;;               "-type" "d"
      ;;               "-name" "inc")
      (with-current-buffer includes-buffer
        (split-string (buffer-string) "\\s-+")))))
;; Excludes system, which causes quite a bit of delay when there are a lot of Boost includes

(defun my-proj-goesr-cxx-init ()
  "Initialize GOESR C++ project"
  (load-file "/goesr/user/boreilly/goesr-dev.el")
  (let ((my-root-file (format "%s/Makefile" my-project-root)))
    (when (file-exists-p my-root-file)
      (ede-cpp-root-project "goesr-cxx"
                            :file my-root-file
                            ;; This way of getting path is more robust, but can take time.
                            ;; :include-path (my-get-goesr-include-path)
                            :include-path goesr-cxx-include-path
                            ;; The sheer number of Boost header dependency files makes it impractical to use
                            ;; :system-include-path (list (format "%s/rhel%sx/gnu/local/boost-1.48.0-x86_64/include" goesr-sdf-ots-path my-rhel-release))
                            )))
  (setq-default semanticdb-find-default-throttle '(local project unloaded recursive)))

;; TODO: Have not tested Java in the new CEDET, may need to set other properties
;;(ede-java-root-project "goesr-java" :file (format "%s/Makefile" my-project-root))

(cond
 ((string= (file-name-nondirectory my-project-root) "emacs")
  (my-proj-emacs-init)))

(provide 'my-proj)

