;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require 2htdp/batch-io)

;; THIS IS INPUT PROCESSING, AND IS NOT PART OF THE CODE FOR MY SOLUTION

(define (los->lon los)
  (cond [(empty? los) empty]
        [else
         (cons (string->number (first los)) (los->lon (rest los)))]))

(define INPUT (los->lon (read-lines "input/1.txt")))



;; PART 1 SOLUTION

(define (solve1 lon)
  (cond [(empty? lon) false]
        [(number? (helper1-1 (first lon) (rest lon)))
         (helper1-1 (first lon) (rest lon))]
        [else (solve1 (rest lon))]))

(define (helper1-1 n lon)
  (cond [(empty? lon) false]
        [(= (+ n (first lon)) 2020)
         (* n (first lon))]
        [else
         (helper1-1 n (rest lon))]))



;; PART 2 SOLUTION

(define (solve2 lon)
  (cond [(empty? lon) false]
        [(number? (helper2-1 (first lon) (rest lon)))
         (helper2-1 (first lon) (rest lon))]
        [else
         (solve2 (rest lon))]))

(define (helper2-1 n lon)
  (cond [(empty? lon) false]
        [(number? (helper2-2 n (first lon) (rest lon)))
         (helper2-2 n (first lon) (rest lon))]
        [else
         (helper2-1 n (rest lon))]))

(define (helper2-2 n1 n2 lon)
  (cond [(empty? lon) false]
        [(= (+ n1 n2 (first lon)) 2020)
         (* n1 n2 (first lon))]
        [else
         (helper2-2 n1 n2 (rest lon))]))



;; PRINT SOLUTIONS TO CONSOLE

(solve1 INPUT)
(solve2 INPUT)