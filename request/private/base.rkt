#lang racket

(require net/url
         racket/file
         fancy-app
         libuuid
         "call-response.rkt"
         "struct.rkt")

(provide http-requester)

(define +crlf+ "\r\n")

(define (make-boundary)
  (string-replace (uuid-generate) "-" ""))

(define (make-content-disposition boundary filename filepath)
  (define contents (file->bytes filepath))
  (bytes-append
   (string->bytes/utf-8
    (string-append boundary
                   +crlf+
                   (format "Content-Disposition: form-data; name=\"~a\"; filename=\"~a\""
                           filename filename)
                   +crlf+
                   "Content-Type: application/octet-stream"
                   +crlf+))
   contents
   (string->bytes/utf-8 +crlf+)))

(define (make-multipart-form-data-body files boundary)
  (define boundary* (string-append "--" boundary))
  (define parts (hash-map files
                          (lambda (filename filepath)
                            (make-content-disposition boundary* filename filepath))))
  (apply bytes-append
         (append parts
                 (map string->bytes/utf-8 (list "--" boundary "--" +crlf+ +crlf+)))))

(define (http-get url #:headers [headers '()])
  (call-response/input-url url (get-impure-port _ headers)))

(define (http-put url body #:headers [headers '()])
  (call-response/input-url url (put-impure-port _ body headers)))

(define (http-post url body #:headers [headers '()] #:files [files #hasheq()])
  (define do-request (lambda (b h)
                       (call-response/input-url url (post-impure-port _ b h))))
  (if (hash-empty? files)
      (do-request body headers)
      (let* [(boundary (make-boundary))
             (body* (make-multipart-form-data-body files boundary))]
        (append headers (list (format "Content-Type: multipart/form-data, boundary=~a"
                                      boundary)
                              (format "Content-Length: ~a" (bytes-length body*))))
        (do-request body* headers))))

(define (http-delete url #:headers [headers '()])
  (call-response/input-url url (delete-impure-port _ headers)))

(define http-requester
  (requester http-get
             http-put
             http-post
             http-delete))
