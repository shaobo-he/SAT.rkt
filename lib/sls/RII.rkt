#lang racket

(require "utils.rkt")
(require "../parser.rkt")
;; Randomized Iterative Improvement
;; in Hoos & Stützle chapter 2

(define iterative-improve
  (λ (clauses sol)
    (let ([neighbors (get-1-exchange-neighbors sol)])
      ;; neighbor selection: best improvement
      ;; problem with this method -- when the number
      ;; of variables is largeg, to get the mimimum value
      ;; is too expensive
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