#lang racket

(provide (all-defined-out))

(define satisfying-literal?
  (λ (assignment lit)
    (match lit
      [`(not ,var) (not (hash-ref assignment var))]
      [else (hash-ref assignment lit)])))

(define satisfying-clause?
  (λ (assignment clause)
    (cond
      [(null? clause) #f]
      [else (or (satisfying-literal? assignment (car clause))
                (satisfying-clause? assignment (cdr clause)))])))

(define satisfying-clauses?
  (λ (assignment clauses)
    (cond
      [(null? clauses) #t]
      [else (and (satisfying-clause? assignment (car clauses))
                (satisfying-clauses? assignment (cdr clauses)))])))

(define get-unsat-clauses
  (λ (clauses assignment)
    (foldl (λ (clause energy)
             (if (satisfying-clause? assignment clause)
                 energy
                 (+ energy 1))) 0 clauses)))

(define build-init-assignment
  (λ (var-num)
    (let ([flip-a-coin
           (λ () (if (= (random 2) 0) #f #t))])
      (foldl
       (λ (var-id assignment) (hash-set assignment (+ var-id 1) (flip-a-coin)))
       (hash)
       (range var-num)))))

(define flip-bit
  (λ (bit)
    (if bit #f #t)))

(define get-1-exchange-neighbors
  (λ (sol)
    (for/list ([v (in-range (hash-count sol))])
      (hash-update sol (+ v 1) flip-bit))))

(define argmins
  (λ (proc lst)
    (if (null? lst)
        '()
        (foldl (λ (e r)
                 (let ([ve (proc e)]
                       [vr (proc (car r))])
                   (cond
                     [(< ve vr)  `(,e)]
                     [(= ve vr) (cons e r)]
                     [else r]))) `(,(car lst)) (cdr lst)))))