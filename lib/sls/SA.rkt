#lang racket

(require "dpll.rkt")
(require "parser.rkt")

; based on paper: Uniform Solution Sampling Using a Constraint Solver As an Oracle

; first MCMC
; unnormalized Boltzmann distribution
(define Boltzmann-distribution
  (λ (E T)
    (exp (- 0 (/ E T)))))

(define get-energy
  (λ (clauses)
    (λ (assignment)
      (foldl (λ (clause energy)
               (if (satisfying-clause? assignment clause)
                   energy
                   (+ energy 1))) 0 clauses))))

; randomly flip one bit
(define propose
  (λ (assignment)
    (let ([var (+ 1 (random (hash-count assignment)))])
      (hash-set assignment var (not (hash-ref assignment var))))))

(define Metropolis
  (λ (init-assignment sampling-prob propose steps)
    (letrec ([Metropolis-helper (λ (assignment steps)
                                  (cond
                                    [(= steps 0) assignment]
                                    [else
                                     (let* ([proposal (propose assignment)]
                                            [accept-prob (min 1 (/ (sampling-prob proposal)
                                                                   (sampling-prob assignment)))])
                                       (if (< (random) accept-prob)
                                           (Metropolis-helper proposal (- steps 1))
                                           (Metropolis-helper assignment (- steps 1))))]))])
      (Metropolis-helper init-assignment steps))))

(define init-assignment (build-init-assignment 25))
(define clauses (parse-dimacs-file "../tests/5queens.cnf"))
;(define clauses '((1 2 3 4 5)))

(define result (filter (λ (a) (satisfying-clauses? a clauses))
        (map (λ (x)
               (Metropolis init-assignment (λ (p) (Boltzmann-distribution
                                                   ((get-energy clauses) p) 0.1)) propose 2000))
             (range 100))))

(define solution->string
  (λ (assignment size)
    (letrec ([solution->string^ (λ (idxs)
                                  (cond
                                    [(null? idxs) ""]
                                    [else
                                     (let ([idx (+ 1 (car idxs))])
                                       (string-append (if (hash-ref assignment idx) "X" "O")
                                                      (string-append (if (= (modulo idx size) 0) "\n" "")
                                                                     (solution->string^ (cdr idxs)))))]))])
      (solution->string^ (range (hash-count assignment))))))

;(foldl (λ (a r) (if (hash-ref a 2) (+ r 1) r)) 0 result)

(define str-sols (map (λ (a) (solution->string a 5)) result))

(define count-solutions
  (λ (str-sols ht)
    (cond
      [(null? str-sols) ht]
      [else (let ([str-sol (car str-sols)])
              (if (hash-has-key? ht str-sol)
                  (count-solutions
                   (cdr str-sols)
                   (hash-set ht str-sol
                             (+ 1 (hash-ref ht str-sol))))
                  (count-solutions
                   (cdr str-sols)
                   (hash-set ht str-sol 1))))])))

 (count-solutions str-sols (hash))
