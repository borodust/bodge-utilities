(cl:in-package :bodge-util)

;;;
;;;
;;;
(defclass parent ()
  ((children :initform '() :reader children-of)))


(defgeneric adopt (parent child)
  (:method ((this parent) child)
    (with-slots (children) this
      (nconcf children (list child)))))


(defgeneric abandon (parent child)
  (:method ((this parent) child)
    (with-slots (children) this
      (deletef children child))))


(defgeneric abandon-all (parent)
  (:method ((this parent))
    (with-slots (children) this
      (prog1 children
        (setf children nil)))))


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
    (labels ((expand-child (root)
               (unless (listp root)
                 (error "Child descriptor must be a list, but got ~A" root))
               (with-gensyms (parent)
                 (destructuring-bind (child-class &rest initargs-and-children) root
                   (multiple-value-bind (initargs children)
                       (bodge-util:parse-initargs-and-list initargs-and-children)
                     `(let ((,parent (funcall ,ctor ',child-class ,@initargs)))
                        ,@(loop for child in children
                                collect `(adopt ,parent ,(expand-child child)))
                        ,parent))))))
      `(prog1 ,parent
         (let ((,ctor (or ,child-ctor #'make-instance)))
           ,@(loop for child in (mapcar #'expand-child children)
                   collect `(adopt ,parent ,child)))))))
