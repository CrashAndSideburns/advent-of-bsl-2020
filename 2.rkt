;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)
(require 2htdp/batch-io)

;; THIS IS INPUT PROCESSING, AND IS NOT PART OF THE CODE FOR MY SOLUTION

(define-struct pw (min max char str))

;; KINDLY IGNORE THE FACT THAT I JUST CASUALLY CREATED A STRUCT WITH NO HTDD

(define (delimit p c)
  (cond [(string=? (string-ith p 0) c) ""]
        [else
         (string-append (string-ith p 0)
                        (delimit (substring p 1) c))]))

(define (parse-min p)
  (string->number (delimit p "-")))

(define (parse-max p)
  (string->number (substring (delimit p " ")
                             (+ (string-length (delimit p "-")) 1))))

(define (parse-char p)
  (substring (delimit p ":")
             (+ (string-length (delimit p " ")) 1)))

(define (parse-str p)
  (substring p
             (+ (string-length (delimit p ":")) 2)))

(define (parse-p p)
  (make-pw (parse-min p)
           (parse-max p)
           (parse-char p)
           (parse-str p)))

(define (parse-lop lop)
  (cond [(empty? lop) empty]
        [else
         (cons (parse-p (first lop)) (parse-lop (rest lop)))]))

(define INPUT (parse-lop (read-lines "input/2.txt")))



;; PART 1 SOLUTION

(@htdf solve1)
(@signature (listof Password) -> Natural)
;; gives the number of passwords in a list which fulfill the rules of part 1

;(define (solve1 lop) 0) ;stub

(define (solve1 lop)
  (cond [(empty? lop) 0]
        [else
         (+ (helper1-1 (first lop)) (solve1 (rest lop)))]))


(@htdf helper1-1)
(@signature Password -> Natural)
;; return 1 if a password is valid, 0 if it is not

;(define (helper1-1 p) 0) ;stub

(define (helper1-1 p)
  (if (and (>= (helper1-2 (pw-str p) (pw-char p)) (pw-min p))
           (<= (helper1-2 (pw-str p) (pw-char p)) (pw-max p)))
      1
      0))


(@htdf helper1-2)
(@signature String 1-String -> Natural)
;; return the number of occurrences of the char c in the string

;(define (helper1-2 p) 0) ;stub

(define (helper1-2 str c)
  (cond [(string=? str "") 0]
        [else
         (if (string=? (string-ith str 0) c)
             (+ 1 (helper1-2 (substring str 1) c))
             (helper1-2 (substring str 1) c))]))



;; PART 2 SOLUTION

(@htdf solve2)
(@signature (listof Password) -> Natural)
;; gives the number of passwords in a list which fulfill the rules of part 2

;(define (solve2 lop) 0) ;stub

(define (solve2 lop)
  (cond [(empty? lop) 0]
        [else
         (+ (helper2-1 (first lop)) (solve2 (rest lop)))]))



(@htdf helper2-1)
(@signature Password -> Natural)
;; return 1 if a password is valid, 0 if it is not

;(define (helper2-1 p) 0) ;stub

(define (helper2-1 p)
  (if (xor (string=? (string-ith (pw-str p) (- (pw-min p) 1)) (pw-char p))
           (string=? (string-ith (pw-str p) (- (pw-max p) 1)) (pw-char p)))
      1
      0))



(@htdf xor)
(@signature Boolean Boolean -> Boolean)
;; it's just xor. i don't know why this isn't in BSL

;(define (xor b1 b2) false) ;stub

(define (xor b1 b2)
  (and (or b1 b2)
       (not (and b1 b2))))



;; PRINT SOLUTIONS TO CONSOLE

(solve1 INPUT)
(solve2 INPUT)