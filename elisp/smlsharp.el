;;; How to use:
;;;   (require 'sml-mode)
;;;   (require 'smlsharp)
;;;   (define-key sml-mode-map "\C-ct" #'smlsharp-query-type)

(eval-when-compile
  (require 'cl))

;;; ("file-name" (line col) (line col) "type")
(defsubst smlsharp-annot-file-name (annot)
  (car annot))

(defsubst smlsharp-annot-line-from (annot)
  (car (cadr annot)))

(defsubst smlsharp-annot-col-from (annot)
  (cadr (cadr annot)))

(defsubst smlsharp-annot-line-to (annot)
  (car (caddr annot)))

(defsubst smlsharp-annot-col-to (annot)
  (cadr (caddr annot)))

(defsubst smlsharp-annot-type (annot)
  (cadddr annot))

(defun smlsharp-query-type ()
  "Display the type of the expression at point."
  (interactive)
  (let ((line (line-number-at-pos (point)))
        (col (current-column))
        (file-name (buffer-file-name)))
    (unless file-name
      (error "the buffer does not visit a file"))
    (message "%s"
             (smlsharp-annot-type
              (car (smlsharp-filter-annot file-name line col))))))

(defun smlsharp-filter-annot (file-name line col)
  "Return annotations of enclosing expressions of the point
at line number LINE, column number COL in the file FILE-NAME,
from inner to outer."
  (let ((basename (file-name-nondirectory file-name))
        (annot (format "%s.annot" (file-name-sans-extension file-name)))
        (col (1+ col)))
    (unless (file-exists-p annot)
      (error "annot file `%s' is not found" annot))
    (when (file-newer-than-file-p file-name annot)
      (error "source code `%s' is newer than the annot." file-name))
    (with-temp-buffer
      (insert-file-contents annot)
      (loop with rs = '()
            for annot in (read (current-buffer))
            when (and (equal basename (smlsharp-annot-file-name annot))
                      (or (and (= line (smlsharp-annot-line-from annot))
                               (>= col (smlsharp-annot-col-from annot)))
                          (> line (smlsharp-annot-line-from annot)))
                      (or (and (= line (smlsharp-annot-line-to annot))
                               (<= col (smlsharp-annot-col-to annot)))
                          (< line (smlsharp-annot-line-to annot))))
            do (push annot rs)
            finally return rs))))

(provide 'smlsharp)
