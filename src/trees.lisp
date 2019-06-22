(cl:in-package :bodge-util)

;;;
;;;
;;;
(defclass parent ()
  ((children :initform '() :reader children-of)))


(defgeneric adopted (child parent)
  (:method (child parent) (declare (ignore child parent))))


(defgeneric adopt (parent child)
  (:method ((this parent) child)
    (with-slots (children) this
      (prog1 (nconcf children (list child))
        (adopted child this)))))


(defgeneric abandoned (child parent)
  (:method (child parent) (declare (ignore child parent))))


(defgeneric abandon (parent child)
  (:method ((this parent) child)
    (with-slots (children) this
      (prog1 (deletef children child)
        (abandoned child this)))))


(defgeneric abandon-all (parent)
  (:method ((this parent))
    (with-slots (children) this
      (prog1 children
        (unwind-protect
             (loop for child in children
                   do (abandoned child this))
          (setf children nil))))))


(defmacro dochildren ((var parent) &body body)
  `(dolist (,var (children-of ,parent))
     ,@body))


(defun %do-tree-preorder (root action)
  (funcall action root)
  (dochildren (ch root)
    (%do-tree-preorder ch action)))


(defun %do-tree-postorder (root action)
  (dochildren (ch root)
    (%do-tree-postorder ch action))
  (funcall action root))


(defmacro dotree ((var root &optional (order :pre)) &body body)
  (let ((fn (ecase order
              (:pre '%do-tree-preorder)
              (:post '%do-tree-preorder))))
    `(,fn ,root (lambda (,var) ,@body))))


(defmacro parent-tree ((parent &optional child-ctor) &body children)
  (with-gensyms (ctor)
    (labels ((expand-child (parent child)
               (unless (listp child)
                 (error "Child descriptor must be a list, but got ~A" child))
               (with-gensyms (child-instance)
                 (destructuring-bind (child-class &rest initargs-and-children) child
                   (multiple-value-bind (initargs children)
                       (bodge-util:parse-initargs-and-list initargs-and-children)
                     `(let ((,child-instance (funcall ,ctor ',child-class ,@initargs)))
                        (adopt ,parent ,child-instance)
                        ,@(loop for child in children
                                collect (expand-child child-instance child))))))))
      `(prog1 ,parent
         (let ((,ctor (or ,child-ctor #'make-instance)))
           ,@(loop for child in children
                   collect (expand-child parent child)))))))
