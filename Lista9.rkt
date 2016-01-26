#lang slideshow

;; Task 2
(define (count-change num vals)
  (define (count-change-aux v)
    (if (<= num v)
        (if (= num v) 1 0)
        (memoized-count-change (- num v))))
  (define memoized-count-change
    (memoized
     (lambda (val)
       (foldl + 0 (map count-change-aux vals)))))
  (memoized-count-change num))

(define (memoized fun)
  (let ([cache (make-hash)])
    (lambda (val)
      (if (hash-has-key? cache val)
          (hash-ref cache val)
          (let ([result (fun val)])
            (hash-set! cache val result)
            result)))))
;; Task 3
(define (count-atoms l)
  (count
   (lambda (elem)
     (not (pair? elem)))
   l))

;; Task 4
(define (mk-mobile left right) (list left right))
(define (mk-branch len str) (list len str))

(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (cadr mobile))

(define (branch-length branch) (car branch))
(define (branch-struct branch) (cadr branch))
(define (is-terminal? branch) (not (pair? (branch-struct branch))))

(define (weight mobile)
  (define (weight-struct str)
    (let ([child-str (branch-struct str)])
      (if (is-terminal? str)
          child-str
          (weight child-str))))
  (+ (weight-struct (left-branch mobile)) (weight-struct (right-branch mobile))))

(define (balanced? mobile)
  (define (torque br)
    (* (branch-length br)
       (let ([child-str (branch-struct br)])
         (if (is-terminal? br)
             child-str
             (weight child-str)))))
  (define (balanced-branch? br)
    (if (is-terminal? br)
        true
        (balanced? (branch-struct br))))
  (let ([left-br (left-branch mobile)]
        [right-br (right-branch mobile)])
    (and (balanced-branch? left-br)
         (balanced-branch? right-br)
         (= (torque left-br)
            (torque right-br)))))


(define (print-mobile mobile)
  (define (print-branch br)
    (let ([child-br (branch-struct br)])
      (if (is-terminal? br)
          (colorize (cc-superimpose (text (~a child-br)) (circle 30)) "green")
          (print-mobile child-br))))
  (htl-append
   (drop-below-ascent (print-branch (left-branch mobile)) 20)
   (colorize (circle 20) "red")
   (drop-below-ascent (print-branch (right-branch mobile)) 20)))

;; Task 5
(define (deriv form var)
  (simplify
   (match (simplify form)
     [`(,'* ,p1 ,p2) `(,'+ (,'* ,(deriv p1 var) ,p2) (,'* ,p1 ,(deriv p2 var)))]
     [`(,'+ ,p1 ,p2) `(,'+ ,(deriv p1 var) ,(deriv p2 var))]
     [(? (lambda (x) (eq? var x))) 1]
     [_ 0])))

(define (simplify form)
  (match form
    [`(,'+ ,p1 ,p2) (simplify-sum (simplify p1) (simplify p2))]
    [`(,'* ,p1 ,p2) (simplify-mul (simplify p1) (simplify p2))]
    [y y]))

(define (simplify-sum p1 p2)
  (let ([np1 (number? p1)]
        [np2 (number? p2)])
    (let ([zp1 (and np1 (= 0 p1))]
          [zp2 (and np2 (= 0 p2))])
      (if (and np1 np2)
          (+ p1 p2)
          (if (or zp1 zp2)
              (if zp1 p2 p1)
              `(,'+ ,p1 ,p2))))))

(define (simplify-mul p1 p2)
  (let ([np1 (number? p1)]
        [np2 (number? p2)])
    (let ([zp1 (and np1 (= 0 p1))]
          [zp2 (and np2 (= 0 p2))]
          [op1 (and np1 (= 1 p1))]
          [op2 (and np2 (= 1 p2))])
      (if (and np1 np2)
          (* p1 p2)
          (if (or zp1 zp2)
              0
              (if (or op1 op2)
                  (if op1 p2 p1)
                  `(,'* ,p1 ,p2)))))))
