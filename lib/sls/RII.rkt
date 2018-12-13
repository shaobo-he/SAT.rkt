#lang racket

(require "utils.rkt")
(require "../parser.rkt")
;; Randomized Iterative Improvement
;; in Hoos & Stützle chapter 2

(define uninformed-random-walk
  (λ (sol)
    (hash-update sol (+ 1 (random (hash-count sol))) flip-bit)))

(define iterative-improve
  (λ (clauses sol)
    (let ([neighbors (get-1-exchange-neighbors sol)])
      (let ([ts (argmins car (map (λ (s) (cons (get-unsat-clauses clauses s) s)) neighbors))])
        (cdr (list-ref ts (random (length ts))))))))

(define RII-do
  (λ (clauses sol wp)
    (if (satisfying-clauses? sol clauses)
        sol
        (let ([new-sol (if (<= (random) wp)
                           (uninformed-random-walk sol)
                           (iterative-improve clauses sol))])
          (RII-do clauses new-sol wp)))))

(define RII
  (λ (filename wp)
    (let ([clauses (parse-dimacs-file filename)]
          [var-num (get-var-number filename)])
      (RII-do clauses (build-init-assignment var-num) wp))))