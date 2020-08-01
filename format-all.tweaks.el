(define-format-all-formatter yapf
  (:executable "yapf")
  (:install "pip install yapf")
  (:languages "Python")
  (:format (format-all--buffer-easy executable)))
