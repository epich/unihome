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

(provide 'my-proj)

