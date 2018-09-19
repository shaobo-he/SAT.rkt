#lang racket

(require "dpll.rkt")

(provide (all-defined-out))

; based on paper: Uniform Solution Sampling Using a Constraint Solver As an Oracle

; first MCMC
; unnormalized Boltzmann distribution
(define Boltzmann-distribution
  (λ (E T)
    (exp (- 0 (/ E T)))))

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
  (λ (init-assignment get-energy T steps)
    (letrec ([Metropolis-helper (λ (assignment steps)
                                  (cond
                                    [(= steps 0) assignment]
                                    [else
                                     (let* ([proposal (propose assignment)]
                                            [accept-prob (min 1 (/ (Boltzmann-distribution (get-energy proposal) T)
                                                                   (Boltzmann-distribution (get-energy assignment) T)))])
                                       (if (< (random) accept-prob)
                                           (Metropolis-helper proposal (- steps 1))
                                           (Metropolis-helper assignment (- steps 1))))]))])
      (Metropolis-helper init-assignment steps))))

(define build-init-assignment
  (λ (var-num)
    (let ([flip-a-coin (λ () (if (= (random 2) 0) #f #t))])
      (foldl (λ (var-id assignment) (hash-set assignment (+ var-id 1) (flip-a-coin))) (hash) (range var-num)))))

(define init-assignment (build-init-assignment 5))
(define clauses '((1) (2) (3) (4) (5)))

(Metropolis init-assignment (get-energy clauses) 0.1 200)