(cl:in-package :bodge-util)


(defun translate-name-to-foreign (symbol)
  (cffi:translate-name-to-foreign symbol *package*))


(defun translate-name-from-foreign (name)
  (cffi:translate-name-from-foreign name *package*))
