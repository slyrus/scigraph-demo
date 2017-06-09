
(defpackage #:scigraph-demo
  (:use #:clim-lisp #:clim #:graph))

(in-package #:scigraph-demo)

(defparameter *indometh-file*
  (merge-pathnames
   #P"indometh.txt"
   (asdf:component-pathname
    (asdf:find-system 'scigraph-demo))))

(defparameter *indometh-pk*
  (with-open-file (f *indometh-file*)
    ;; skip header line
    (read-line f)
    (flet ((read-indo-line (f)
             (let ((line (read f nil)))
               (if line
                   (progn
                     (let ((patient (read f))
                           (time (read f))
                           (concentration (read f)))
                       (list (cons :line (parse-integer line))
                             (cons :patient-id (parse-integer patient))
                             (cons :time time)
                             (cons :concentration concentration))))
                   nil))))
      (loop for data = (read-indo-line f)
         while data
         collect data))))

(defun get-patient-pk-data (patient-id)
  (mapcar (lambda (x)
            (list (cdr (assoc :time x))
                  (cdr (assoc :concentration x))))
          (remove-if-not (lambda (x)
                           (equal patient-id
                                  (cdr (assoc :patient-id x))))
                         *indometh-pk*)))

(defun get-patient-pk-graph-data (patient-id &key (color :royal-blue))
  (make-instance 'graph-data
                 :name (format nil "Patient ~D" patient-id)
                 :data (get-patient-pk-data patient-id)
                 :color color
                 :thickness 3
                 :symbologies '(:scatter :line)))

(defparameter *patients*
    (remove-duplicates (mapcar (lambda (x)
                                 (cdr (assoc :patient-id x)))
                               *indometh-pk*)))

(defun make-indometh-graph ()
  (let* ((graph (make-instance 'annotated-graph
                               :name "Demo Graph"
                               :auto-scale :both
                               :x-label "Time (h)"
                               :y-label "Concentration (mcg/ml)"))
         (colors #(:red :royal-blue :salmon :green :aquamarine :khaki)))
    
    (loop for patient-id in *patients*
         for i from 0
       do
         (add-dataset graph
                      (get-patient-pk-graph-data patient-id
                                                 :color (elt colors (mod i 6)))))
    graph))

(defun make-indometh-graph-postscript-file (&optional (file "indometh-pk.ps"))
  (let ((graph (make-indometh-graph)))
    (with-open-file (s file :direction :output :if-exists :supersede)
      (clim:with-output-to-postscript-stream (stream s)
        (formatting-table (stream :x-spacing 20
                                  :y-spacing 20)
          (formatting-row (stream)
            (formatting-cell (stream :align-x :center
                                     :align-y :bottom
                                     :min-height 110)
              (draw-text* stream "indometh PK" 170 30
                          :text-style (make-text-style :fix :bold :normal))))
          (formatting-row (stream)
            (formatting-cell (stream :align-x :left
                                     :align-y :center)
              (display-graph graph :stream stream :width 600 :height 500)))))))
  file)

(let ((file
        #+nil
        "indometh-pk.ps"
        (make-indometh-graph-postscript-file)))
  (uiop:run-program `("ps2pdf" ,(uiop:unix-namestring file)))
  (uiop:run-program `("pdf2svg" ,(uiop:unix-namestring
                                  (merge-pathnames (make-pathname :type "pdf") file))
                                ,(uiop:unix-namestring
                                  (merge-pathnames (make-pathname :type "svg") file)))))

(defun make-indometh-graph-frame ()
  (let ((graph (make-indometh-graph)))
    (view-graphs (list graph)
                 :title "Scatter Plot"
                 :wait-until-done nil)))
