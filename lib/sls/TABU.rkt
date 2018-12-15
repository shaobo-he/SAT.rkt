#lang racket

(require "utils.rkt")
(require "../parser.rkt")

;; Tabu Search


(define tabu-do
  (λ (var-num clauses sol tt-table tt step)
    (if (satisfying-clauses? sol clauses)
        sol
        (let ([curr-score (get-unsat-clauses clauses sol)]
              [candT (map (λ (v)
                            (let ([c (+ v 1)])
                              (cons c
                                    (get-unsat-clauses
                                     clauses
                                     (hash-update sol c flip-bit)))))
                          (for/list ([i (in-range var-num)]) i))])
          (let* ([aT (filter (λ (t) (< (cdr t) curr-score)) candT)]
                 [candVTs (if (null? aT)
                              (filter
                               (λ (t)
                                 (let ([v (car t)])
                                   (>= (- step (hash-ref tt-table v)) tt)))
                               candT)
                              aT)]
                 [cV (car (list-ref candVTs (random (length candVTs))))])
            (tabu-do
             var-num
             clauses
             (flip-var cV sol)
             (hash-set tt-table cV step)
             tt
             (+ step 1)))))))

(define tabu
  (λ (filename tt)
    (let* ([clauses (parse-dimacs-file filename)]
           [var-num (get-var-number filename)]
           [tabu-table (foldl
                        (λ (var-id assignment)
                          (hash-set assignment (+ var-id 1) (- 0 tt)))
                        (hash)
                        (range var-num))])
      (tabu-do var-num clauses (build-init-assignment var-num) tabu-table tt 0))))