
(define myexpr (/
                 (+ 5 4 
                    (- 2 (- 3 (+ 6 (/ 1 3)))))
                 (* 3 (- 6 2) (- 2 7))))

(define (mysquare x) (* x x))

(define (sum_of_squares x y) (+ (mysquare x) (mysquare y)))

(define (ex14 x y z)
  (cond ((and (>= x y) (>= y z)) (sum_of_squares x y))
        ((and (>= x y) (>= z y)) (sum_of_squares x z))
        ((and (>= y z) (>= z x)) (sum_of_squares y z))
        ((and (>= y z) (>= x z)) (sum_of_squares y x))
        ((and (>= z x) (>= x y)) (sum_of_squares z x))
        ((and (>= z x) (>= y x)) (sum_of_squares z y))
        (else "error")))

(define (a-plus-abs-b a b) ((if (> b 0) + -) a b))

;;;
;;; Newton's method for computing square roots
;;;
(define (sqrt x) 
  (define (good-enough? guess) 
    (< (abs (- (mysquare guess) x)) 0.01))
  (define (improve-guess guess) 
    (average (/ x guess) guess))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve-guess guess))))
  (sqrt-iter 1.0))

;;; new-if stuff
;;; (define (new-if predicate then-clause else-clause)
;;;   (cond (predicate then-clause)
;;;         (else else-clause)))

;;; (define (sqrt-iter guess x)
;;;   (new-if (good-enough? guess x)
;;;           guess
;;;           (sqrt-iter (improve-guess guess x) x)))

