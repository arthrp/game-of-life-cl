(defparameter *width* 40)
(defparameter *height* 20)

(defun make-board ()
  (make-array (list *height* *width*) :initial-element nil))

(defun randomize-board (board)
  (dotimes (i *height*)
    (dotimes (j *width*)
      (setf (aref board i j) (if (= (random 2) 0) nil t)))))

(defun init-board (board)
    ;Blinker
    (setf (aref board 2 2) t)
    (setf (aref board 2 3) t)
    (setf (aref board 2 4) t)
)

(defun count-neighbors (board x y)
  (let ((count 0))
    (dotimes (i 3)
      (dotimes (j 3)
        (unless (and (= i 1) (= j 1))
          (let* ((ni (mod (+ x i -1) *height*))
                 (nj (mod (+ y j -1) *width*)))
            (when (aref board ni nj)
              (incf count))))))
    count))

(defun game-step (board)
  (let ((new-board (make-board)))
    (dotimes (i *height*)
      (dotimes (j *width*)
        (let ((alive? (aref board i j))
              (neighbors (count-neighbors board i j)))
          (setf (aref new-board i j)
                (cond ((and alive? (or (= neighbors 2) (= neighbors 3))) t)
                      ((and (not alive?) (= neighbors 3)) t)
                      (t nil))))))
    new-board))

(defun display (board)
  (clear-screen)
  (dotimes (i *height*)
    (dotimes (j *width*)
      (princ (if (aref board i j) "#" " ")))
    (format t "~%"))
  (finish-output))

(defun clear-screen ()
  (format t "~c[2J" #\Escape))

(defun game-loop (board)
  (loop
     (display board)
     (sleep 0.5)
     (setf board (game-step board))))

(defun main ()
  (let ((board (make-board)))
    (randomize-board board)
    (game-loop board)))

(main)