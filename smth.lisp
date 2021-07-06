(load "~/quicklisp/setup.lisp")
(ql:quickload "cl-ppcre")
(defun concat (list)
  "A non-recursive function that concatenates a list of strings."
  (if (listp list)
      (let ((result ""))
        (dolist (item list)
          (if (stringp item)
              (setq result (concatenate 'string result item))))
        result)))

(defun aadi/yt-search () ()
  (run-shell-command
   (concat (list "brave --incognito --new-window youtube.com/results?search_query="
                 (cl-ppcre:regex-replace-all " "
                                             "asmongold"
                                             "+")))))
(aadi/yt-search)
