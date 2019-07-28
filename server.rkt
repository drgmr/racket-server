#lang racket

(require xml net/url)

(define (serve port-number)
  (define custodian (make-custodian))
  (parameterize ([current-custodian custodian])
    (define listener (tcp-listen port-number 5 #t))
    (define (loop)
      (accept-and-handle listener)
      (loop))
    (thread loop))
  (lambda ()
    (custodian-shutdown-all custodian)))

(define (accept-and-handle listener)
  (define custodian (make-custodian))
  (parameterize ([current-custodian custodian])
    (define-values (in out) (tcp-accept listener))
    (thread
        (lambda ()
          (handle in out)
          (close-input-port in)
          (close-output-port out))))
  (thread
    (lambda ()
      (sleep 10)
      (custodian-shutdown-all custodian))))

(define (handle in out)
  (define request
    ; Extract the request type from the first line
    (regexp-match #rx"^GET (.+) HTTP/[0-9]+\\.[0-9]+"
                  (read-line in)))
  (when request
    ; Discard the headers
    (regexp-match #rx"(\r\n|^)\r\n" in)
    ; Dispatch
    (let ([xexpr (dispatch (list-ref request 1))])
      ; Send a reply
      (display "HTTP/1.0 200 Okay\r\n" out)
      (display "Server: Racket\r\nContent-Type: text/html\r\n\r\n" out)
      (display (xexpr->string xexpr) out))))

(define (dispatch str-path)
  ; Parse the request as a URL
  (define url (string->url str-path))
  ; Extract the path
  (define path (map path/param-path (url-path url)))
  ; Find a handler based on the path's first element
  (define handler (hash-ref dispatch-table (car path) #f))
  (if handler
      ; Call the handler
      (handler (url-query url))
      ; No handler found
      `(html (head (title "Error"))
             (body
              (font ((color "red"))
                    "Unknown page: "
                    ,str-path)))))

(define dispatch-table (make-hash))
(hash-set! dispatch-table "hello"
           (lambda (query)
             (display query)
             `(html (body "Hello, world!"))))
