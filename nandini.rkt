#lang racket

(require racket/tcp
         racket/port
         net/url
         net/uri-codec
         racket/string
         racket/date)

;; ========== 🔧 Utility Functions ==========

;; Calculate difference in days between two dates
(define (date-difference d1 d2)
  (define seconds1 (date->seconds d1))
  (define seconds2 (date->seconds d2))
  (quotient (- seconds1 seconds2) 86400)) ; 86400 seconds = 1 day

;; Check if today is the user's birthday
(define (check-birthday today-month today-day birth-month birth-day)
  (and (= today-month birth-month) (= today-day birth-day)))

;; Calculate days until the next birthday
(define (days-until-birthday today birth-month birth-day)
  (define current-year (date-year today))
  (define birthday-this-year
    (seconds->date (find-seconds 0 0 0 birth-day birth-month current-year)))
  (define birthday-next-year
    (seconds->date (find-seconds 0 0 0 birth-day birth-month (+ current-year 1))))
  (if (>= (date->seconds birthday-this-year) (date->seconds today))
      (date-difference birthday-this-year today)
      (date-difference birthday-next-year today)))

;; ========== 🎨 CSS Styling ==========

(define css-style "
  <style>
    body {
      font-family: 'Segoe UI', sans-serif;
      background: #fffbe6;
      text-align: center;
      padding: 2rem;
      color: #333;
    }
    h1 {
      color: #ff4081;
    }
    form {
      background: #fff;
      padding: 2rem;
      margin: auto;
      border-radius: 10px;
      width: 300px;
      box-shadow: 0 0 10px rgba(0,0,0,0.1);
    }
    input {
      padding: 0.5rem;
      font-size: 1rem;
      margin-top: 1rem;
      width: 90%;
      border-radius: 5px;
      border: 1px solid #ccc;
    }
    input[type='submit'] {
      background: #ff4081;
      color: white;
      border: none;
      cursor: pointer;
    }
    input[type='submit']:hover {
      background: #d81b60;
    }
    .wish {
      margin-top: 2rem;
      font-size: 1.3rem;
      color: #2e7d32;
    }
    a {
      display: block;
      margin-top: 1rem;
      color: #3f51b5;
    }
  </style>
")

;; ========== 🌐 Web Routing ==========

(define (parse-request-line line)
  (define parts (string-split line " "))
  (if (>= (length parts) 2)
      (let* ([raw-url (second parts)]
             [url-obj (string->url raw-url)]
             [path (map path/param-path (url-path url-obj))]
             [query (url-query url-obj)])
        (values path query))
      (values '() '())))

(define (route path query)
  (cond
    [(equal? path '("birthday"))
     ;; Show birthday input form
     (string-append
      "<h1>🎂 Birthday Wisher 🎉</h1>"
      "<form action='/wish' method='get'>
         <input type='text' name='name' placeholder='Your name' required><br>
         <input type='text' name='month' placeholder='Birth month (1–12)' required><br>
         <input type='text' name='day' placeholder='Birth day (1–31)' required><br>
         <input type='submit' value='Get Your Wish'>
       </form>")]

    [(equal? path '("wish"))
     ;; Get input values
     (define name (cdr (assoc 'name query)))
     (define month (string->number (cdr (assoc 'month query))))
     (define day (string->number (cdr (assoc 'day query))))
     (define today (current-date))
     (define today-month (date-month today))
     (define today-day (date-day today))
     (define message
       (if (check-birthday today-month today-day month day)
           (format "🎉 Happy Birthday, ~a! 🎂 Wishing you a joyful day!" name)
           (let ([days (days-until-birthday today month day)])
             (format "Hi ~a! 🎈 Your birthday is in ~a day~a 🎁"
                     name days (if (= days 1) "" "s")))))
     ;; Display result
     (string-append
      "<h1>🎉 Birthday Wish</h1>"
      (format "<div class='wish'>~a</div>" message)
      "<a href='/birthday'>🎂 Check Another</a>")]

    [(equal? path '("favicon.ico"))
     ""] ; prevent browser from sending favicon request into error

    [else
     ;; Unknown route
     "<h1>404 Not Found</h1><a href='/birthday'>Go to Birthday Wisher</a>"]))

;; ========== 🌍 Server Setup ==========

(define (handle-client in out)
  (define request-line (read-line in 'any))
  (printf "Request: ~a\n" request-line)
  ;; Skip headers
  (let loop ()
    (define line (read-line in 'any))
    (unless (equal? line "")
      (loop)))
  ;; Process path and query
  (define-values (path query) (parse-request-line request-line))
  (define body (route path query))
  ;; Respond with full HTML
  (display
   (string-append
    "HTTP/1.1 200 OK\r\n"
    "Content-Type: text/html\r\n\r\n"
    "<html><head><meta charset='UTF-8'>" css-style "</head><body>"
    body
    "</body></html>")
   out)
  (flush-output out))

;; Start the TCP server
(define listener (tcp-listen 8080))
(printf "🎂 Birthday Wish App running at: http://localhost:8080/birthday\n")

(define (serve-forever)
  (let loop ()
    (define-values (in out) (tcp-accept listener))
    (thread (lambda ()
              (handle-client in out)
              (close-input-port in)
              (close-output-port out)))
    (loop)))

(serve-forever)
