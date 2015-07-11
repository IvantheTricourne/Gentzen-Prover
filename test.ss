(load "gentzen-i.ss")

;; P, P->Q => Q
(define modus-ponens (start '(P (P > Q)) '(Q)))
(define modus-steps
  '((() (apply-left-rule '(P > Q)))
    ;; junk for testing
    (() (apply-right-rule))
    ((1 1 1 1 1 1 1 1) (choose 1))
    ((2 2 2 2 2 2 2 2) (choose 2))
    ;; The thing for ending it
    (() (end))))

;; P ^ Q => P ^ Q
(define left-conj (start '(P ^ Q) '(P ^ Q)))
(define left-conj-steps1
  '((() (apply-left-rule '(P ^ Q)))
    (() (apply-right-rule))
    (() (end))))
(define left-conj-steps2
  '((() (apply-right-rule))
    ((1) (apply-left-rule '(P ^ Q)))
    ((2) (apply-left-rule '(P ^ Q)))
    (() (end))))

;; P, Q => P ^ Q
(define right-conj (start '(P Q) '(P ^ Q)))
(define right-conj-steps
  '((() (apply-right-rule))
    (() (end))))

;; P v Q => P
(define left-disj (start '(P v Q) '(P)))
(define left-disj-steps
  '((() (apply-left-rule '(P v Q)))
    (() (choose 1))))

;; P => P v Q
(define right-disj (start '(P) '(P v Q)))
(define right-disj-steps
  '((() (apply-right-rule))
    (() (choose 1))))


