;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)
(require 2htdp/batch-io)

;; THIS IS INPUT PROCESSING, AND IS NOT PART OF THE CODE FOR MY SOLUTION

(define (parse-input lo1s)
  (cond [(empty? lo1s) empty]
        [(string=? (first lo1s) ".") (cons false (parse-input (rest lo1s)))]
        [(string=? (first lo1s) "#") (cons true (parse-input (rest lo1s)))]
        [else (parse-input (rest lo1s))]))

(define INPUT (parse-input (read-1strings "input/3.txt")))

;; DEFINE SOME CONSTANTS FOR USE LATER

(define WIDTH (string-length (first (read-lines "input/3.txt"))))

(define HEIGHT (length (read-lines "input/3.txt")))

;; HELPER FUNCTION TO MAKE WORKING WITH INPUT EASIER

(define (get-tree x y lob)
  (list-ref lob (+ (* WIDTH y) x)))



;; HELPER USED IN BOTH PARTS

(@htdf bool->num)
(@signature Boolean -> Natural)
;; return 1 if bool is true, 0 if false

;(define (bool->num b) 0) ;stub

(define (bool->num b)
  (if b
      1
      0))



;; PART 1 SOLUTION

(@htdf solve1)
(@signature (listof Boolean) -> Natural)
;; return the number of trees encountered tobogganing down lob

;(define (solve1 lob) 0) ;stub

(define (solve1 lob)
  (helper1-1 lob 0 0))



(@htdf helper1-1)
(@signature (listof Boolean) Natural Natural -> Natural)
;; return number of trees encountered tobogganing lob from 0, 0

;(define (helper1-1 lob x y) 0) ;stub

(define (helper1-1 lob x y)
  (cond [(>= y HEIGHT) 0]
        [else
         (+ (bool->num (get-tree x y lob))
            (helper1-1 lob (modulo (+ x 3) WIDTH) (+ y 1)))]))



;; PART 2 SOLUTION

(@htdf solve2)
(@signature (listof Boolean) -> Natural)
;; return prod. of number of trees encountered descending lob at certain slopes

;(define (solve2 lob) 0) ;stub

(define (solve2 lob)
  (* (helper2-1 lob 0 0 1 1)
     (helper2-1 lob 0 0 3 1)
     (helper2-1 lob 0 0 5 1)
     (helper2-1 lob 0 0 7 1)
     (helper2-1 lob 0 0 1 2)))



(@htdf helper2-1)
(@signature (listof Boolean) Natural Natural Natural Natural -> Natural)
;; return number of trees encountered tobogganing lob from 0, 0 with slope dx dy

;(define (helper2-1 lob x y dx dy) 0) ;stub

(define (helper2-1 lob x y dx dy)
  (cond [(>= y HEIGHT) 0]
        [else
         (+ (bool->num (get-tree x y lob))
            (helper2-1 lob (modulo (+ x dx) WIDTH) (+ y dy) dx dy))]))



;; PRINT SOLUTIONS TO CONSOLE

(solve1 INPUT)
(solve2 INPUT)