(load "gentzen-i.ss")

;; P, P->Q => Q
(define modus-ponens (start '(P (P > Q)) '(Q)))

(define modus-steps
  (list
   '(() (apply-left-rule '(P > Q)))
   ;; junk for testing
   '(() (apply-right-rule))
   '(() (choose 1))
   '(() (choose 2))
   ;; The thing for ending it
   '(() (end))))

;; ~P => ~P
(define negation-ex (start '((~ P)) '((~ P))))

;; => P->P
(define right-arrow (start '() '(P > P)))

;; P ^ Q => P ^ Q
(define left-conj (start '(P ^ Q) '(P ^ Q)))
;; P, Q => P ^ Q
(define right-conj (start '(P Q) '(P ^ Q)))

;; P v Q => P
(define left-disj (start '(P v Q) '(P)))

;; P => P v Q
(define right-disj (start '(P) '(P v Q)))


