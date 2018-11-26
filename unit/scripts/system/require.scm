(let ((x (amb 1 2 3 4 5 6)))
  (begin
    (require (even? x))
    x))
