(cl:in-package :bodge-util)


;; Feeling lucky about __LC_ALL = 6,
(bodge-util:define-constant +lc-all+ 6)


(defun translate-name-to-foreign (symbol)
  (cffi:translate-name-to-foreign symbol *package*))


(defun translate-name-from-foreign (name)
  (cffi:translate-name-from-foreign name *package*))


(cffi:defcfun ("setlocale" set-locale) :pointer (category :int) (locale :pointer))

(defmacro with-locale ((name) &body body)
  (bodge-util:with-gensyms (current-locale foreign-string)
    `(let ((,current-locale (cffi:foreign-string-to-lisp (set-locale +lc-all+ (cffi:null-pointer)))))
       (unwind-protect
            (cffi:with-foreign-string (,foreign-string ,name)
              (set-locale +lc-all+ ,foreign-string)
              ,@body)
         (cffi:with-foreign-string (,foreign-string ,current-locale)
           (set-locale +lc-all+ ,foreign-string))))))
