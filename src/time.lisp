(cl:in-package :bodge-util)


(defun epoch-seconds (&optional (timestamp (now)))
  (+ (timestamp-to-unix timestamp)
     (float (/ (nsec-of timestamp) 1000000000) 0d0)))


(defun real-time-seconds ()
  (/ (get-internal-real-time) (float internal-time-units-per-second 0d0)))


(defun universal-time->epoch (universal)
  (epoch-seconds (local-time:universal-to-timestamp universal)))
