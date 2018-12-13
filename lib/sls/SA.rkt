#lang racket

(require "utils.rkt")
(require "../parser.rkt")

;; Simulated Annealing

; unnormalized Boltzmann distribution
(define Boltzmann-distribution
  (λ (E T)
    (exp (- 0 (/ E T)))))

(define SA-do
  (λ (clauses sol sampling-prob)
    (if (satisfying-clauses? sol clauses)
        sol
        (let* ([proposal (uninformed-random-walk sol)]
               [accept-prob (min 1 (/ (sampling-prob proposal)
                                      (sampling-prob sol)))]
               [new-sol (if (< (random) accept-prob) proposal sol)])
          (SA-do clauses new-sol sampling-prob)))))

(define SA
  (λ (filename T)
    (let* ([clauses (parse-dimacs-file filename)]
           [var-num (get-var-number filename)]
           [sampling-prob
            (λ (s)
              (Boltzmann-distribution
               (get-unsat-clauses clauses s) T))])
      (SA-do clauses (build-init-assignment var-num) sampling-prob))))