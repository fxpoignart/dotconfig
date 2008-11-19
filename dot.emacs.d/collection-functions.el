(defun select (list p)
  (if (eq () list)
      ()
    (let ((item (car list)))
      (if (funcall p item)
	  (cons item (select (cdr list) p))
	(select (cdr list) p)))))

(defun detect (list p)
  (if (eq () list)
      nil
    (let ((item (car list)))
      (if (funcall p item)
	  item
	(detect (cdr list) p)))))

(defun each (list f)
  (if (eq () (car list))
      ()
    (funcall f (car list))
    (each (cdr list) f)))

(provide 'collection-functions)