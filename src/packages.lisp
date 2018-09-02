(uiop:define-package :bodge-util
  (:nicknames :ge.util)
  (:use :cl :local-time :alexandria :split-sequence :static-vectors :trivial-gray-streams)
  (:export ; from :alexandria
           with-gensyms
           once-only
           symbolicate
           make-keyword
           format-symbol
           parse-body
           parse-ordinary-lambda-list
           when-let
           when-let*
           if-let
           switch
           eswitch
           define-constant
           alist-hash-table
           plist-alist
           read-file-into-string
           read-file-into-byte-vector
           read-stream-content-into-string
           write-stream-content-into-string
           nconcf
           appendf
           nunionf
           unionf
           starts-with-subseq
           positive-integer
           copy-array
           deletef
           alist-hash-table
           alist-plist
           hash-table-plist
           ensure-list
           assoc-value
           doplist)
  (:import-from :uiop/package
                define-package)
  (:export define-package)
  (:export log-errors
           with-hash-entries
           make-hash-table-with-entries
           stream->byte-array
           file->byte-array
           defenum
           f
           epoch-seconds
           real-time-seconds
           universal-time->epoch
           definline
           ensure-not-null
           bound-symbol-value
           if-null
           if-bound
           when-bound
           class-name-of
           dolines
           parent
           adopt
           abandon
           abandon-all
           dochildren
           children-of
           dotree
           search-sorted
           list->array
           reexporting
           in-development-mode
           flatten-array
           expand-array
           float-array
           split-sequence
           stringify
           apply-argument-list
           make-mutable-string
           string->mutable
           string->immutable
           mutate-string
           replace-all
           current-file-truename
           translate-name-to-foreign
           translate-name-from-foreign
           current-executable-path
           make-bounded-input-stream
           system-relative-pathname
           parse-initargs-and-list
           shout
           bind-for-serious-condition
           with-simple-array-pointer))
