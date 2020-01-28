;; format-all yapf
(define-format-all-formatter yapf
  (:executable "yapf")
  (:install "pip install yapf")
  (:modes python-mode)
  (:format (format-all--buffer-easy executable)))

