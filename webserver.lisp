; Converts an ascii code to a character.
;   (http-char \#2 \#0) ;=> #\Space
(defun http-char (c1 c2 &optional (default #\Space))
  (let ((code (parse-integer
                (coerce (list c1 c2) 'string)
                :radix 16
                :junk-allowed t)))
    (if code
      (code-char code)
      default)))

; Converts special characters in a string.
;   (decode-param "foo+bar%3fbaz") ;=> "foo bar?baz"
(defun decode-param (s)
  (labels ((f (lst)
              (when lst
                (case (car lst)
                  (#\% (cons (http-char (cadr lst) (caddr lst))
                             (f (cdddr lst))))
                  (#\+ (cons #\Space (f (cdr lst))))
                  (otherwise (cons (car lst) (f (cdr lst))))))))
    (coerce (f (coerce s 'list)) 'string)))

; Parses parameters from a URI string.
;   (parse-params "name=bob&age=25") ;=> ((NAME . "bob") (AGE . "25"))
(defun parse-params (s)
  (let* ((i1 (position #\= s))
         (i2 (position #\& s)))
    (cond (i1 (cons (cons (intern (string-upcase (subseq s 0 i1)))
                          (decode-param (subseq s (1+ i1) i2)))
                    (and i2 (parse-params (subseq s (1+ i2))))))
          ((equal s "") nil)
          (t s))))

; Parses a URL from a request header, giving the path and the parameters.
;   (parse-url "GET /home.html?foo=bar&x=y HTTP/1.1")
;   ;=> ("home.html" (FOO . "bar") (X . "y"))
(defun parse-url (s)
  (let* ((url (subseq s
                      (+ 2 (position #\Space s))
                      (position #\Space s :from-end t)))
         (x (position #\? url)))
    (if x
      (cons (subseq url 0 x) (parse-params (subseq url (1+ x))))
      (cons url '()))))

; Gets the HTTP headers from a stream, as pairs
(defun get-header (strm)
  (let* ((s (read-line strm))
         (h
           (let ((i (position #\: s)))
             (when i
               (cons (intern (string-upcase (subseq s 0 i)))
                     (subseq s (+ i 2)))))))
    (when h
      (cons h (get-header strm)))))

; Get the request body
(defun get-content-params (strm header)
  (let ((len (cdr (assoc 'CONTENT-LENGTH header))))
    (when len
      (let ((content (make-string (parse-integer len))))
        (read-sequence content strm)
        (parse-params content)))))

; Serve requests with request-handler on a port
(defun serve (request-handler port)
  (let ((socket (socket-server port)))
    (unwind-protect
      (loop (with-open-stream (strm (socket-accept socket))
              (let* ((url (parse-url (read-line strm)))
                     (path (car url))
                     (header (get-header strm))
                     (params (append (cdr url)
                                     (get-content-params strm header)))
                     (*standard-output* strm))
                (funcall request-handler path header params))))
      (socket-server-close socket))))

; A "Hello World!" request handler
(defun hello-request-handler (path header params)
  (if (equal path "greeting")
    (let ((name (assoc 'NAME params)))
      (if (not name)
        (progn
          (format t "HTTP/1.1 200 OK~%~%")
          (format t "<!doctype html><title>Hi</title><form>What is your name?<input name='name'></form>"))
        (progn
          (format t "HTTP/1.1 200 OK~%~%")
          (format t "<!doctype html><p>Nice to meet you, ~a!</p>" (cdr name)))))
    (progn
      (format t "HTTP/1.1 404 Not found~%~%")
      (format t "I don't know that page."))))
