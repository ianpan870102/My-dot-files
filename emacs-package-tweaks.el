;; format-all yapf
(define-format-all-formatter yapf
  (:executable "yapf")
  (:install "pip install yapf")
  (:modes python-mode)
  (:format (format-all--buffer-easy executable)))

;; ivy-posframe height/width tweaks
(defun ivy-posframe-get-size ()
  "The default functon used by `ivy-posframe-size-function'."
  (list
   :height ivy-posframe-height
   :width ivy-posframe-width
   :min-height (or ivy-posframe-min-height
                   (let ((height (+ ivy-height 1)))
                     (min height (or ivy-posframe-height height))))
   :min-width (or ivy-posframe-min-width
                  (let ((width (round (* (frame-width) 0.8))))
                    (min width (or ivy-posframe-width width))))))
