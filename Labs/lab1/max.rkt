#lang racket
(define (f n)
  (if (zero? n)
      null
      (append (f (- n 1)) (list n))))