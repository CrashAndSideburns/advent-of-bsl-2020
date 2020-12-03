;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require spd/tags)
(require 2htdp/batch-io)

;; THIS IS INPUT PROCESSING, AND IS NOT PART OF THE CODE FOR MY SOLUTION

(define (los->lon los)
  (cond [(empty? los) empty]
        [else
         (cons (string->number (first los)) (los->lon (rest los)))]))

(define INPUT (los->lon (read-lines "input/1.txt")))



;; PART 1 SOLUTION

(@htdf solve1)
(@signature (listof Natural) -> Natural or false)
;; produce the product of the 2 numbers in a list that sum to 2020, or false
(check-expect (solve1 (cons 1721
                            (cons 979
                                  (cons 366
                                        (cons 299
                                              (cons 675
                                                    (cons 1456 empty)))))))
              514579)

;(define (solve1 lon) false) ;stub

(define (solve1 lon)
  (cond [(empty? lon) false]
        [(number? (helper1-1 (first lon) (rest lon)))
         (helper1-1 (first lon) (rest lon))]
        [else (solve1 (rest lon))]))



(@htdf helper1-1)
(@signature Natural (listof Natural) -> Natural or false)
;; produce product of n with number in lon with which n sums to 2020, or false

;(define (helper1-1 n lon) false) ;stub

(define (helper1-1 n lon)
  (cond [(empty? lon) false]
        [(= (+ n (first lon)) 2020)
         (* n (first lon))]
        [else
         (helper1-1 n (rest lon))]))



;; PART 2 SOLUTION

(@htdf solve2)
(@signature (listof Natural) -> Natural or false)
;; produces the product of the 3 numbers in a list that sum to 2020, or false
(check-expect (solve2 (cons 1721
                            (cons 979
                                  (cons 366
                                        (cons 299
                                              (cons 675
                                                    (cons 1456 empty)))))))
              241861950)

;(define (solve2 lon) false) ;stub

(define (solve2 lon)
  (cond [(empty? lon) false]
        [(number? (helper2-1 (first lon) (rest lon)))
         (helper2-1 (first lon) (rest lon))]
        [else
         (solve2 (rest lon))]))



(@htdf helper2-1)
(@signature Natural (listof Natural) -> Natural or false)
;; produce product of n with numbers in lon with which n sums to 2020, or false

;(define (helper2-1 n lon) false) ;stub

(define (helper2-1 n lon)
  (cond [(empty? lon) false]
        [(number? (helper2-2 n (first lon) (rest lon)))
         (helper2-2 n (first lon) (rest lon))]
        [else
         (helper2-1 n (rest lon))]))



(@htdf helper2-2)
(@signature Natural Natural (listof Natural) -> Natural or false)
;; produce product of n1 n2 with n in lon with which they sum to 2020, or false

;(define (helper2-2 n1 n2 lon) false) ;stub

(define (helper2-2 n1 n2 lon)
  (cond [(empty? lon) false]
        [(= (+ n1 n2 (first lon)) 2020)
         (* n1 n2 (first lon))]
        [else
         (helper2-2 n1 n2 (rest lon))]))



;; PRINT SOLUTIONS TO CONSOLE

(solve1 INPUT)
(solve2 INPUT)
