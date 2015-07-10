;; start is used to create a new proof expression (or goal)
(define start
  (lambda (left right)
    (or (axiom? left right) `(,left . ,right))))

(define left-thin?
  (lambda (left right)
    (and (memq (car right) left)
         (not (reg-op left)))))
(define axiom?
  (lambda (left right)
    (and (singleton? right)
         (or (and (singleton? left)
                  (equal? left right))
             (left-thin? left right)))))

(define left car)
(define right cdr)

(define-syntax show
  (syntax-rules (=>)
    ((_ exp) `(,(left exp) => ,(right exp)))))

(define singleton?
  (lambda (side)
    (and (not (null? side)) (null? (cdr side)))))
(define no-op?
  (lambda (side)
    (null? (cddr side))))
(define reg-op
  (lambda (exp)
    (if (or (singleton? exp) (no-op? exp))
        #f
        (cadr exp))))

;; Rules
(define rules '(apply-right-rule apply-left-rule choose end))
(define rule?
  (lambda (x)
    (cond
     ((memq x rules) #t)
     (else #f))))

;; A tree is one of
;; - (conj theo theo)
;; - (disj theo theo)
(define disj? (lambda (tree) (equal? (car tree) 'disj)))
(define conj? (lambda (tree) (equal? (car tree) 'conj)))
(define tree?
  (lambda (x)
    (or (conj? x) (disj? x))))

;; A Path is a list of 1s and 2s
(define path?
  (lambda (x)
    (if (list? x)
        (andmap (lambda (y) (or (= y 1) (= y 2))) x)
        #f)))

;; A step is one of
;; (list Path Rule-Application)
(define step?
  (lambda (x)
    (and (path? (car x))
         (rule? (cadr x))
         )))
(define get-path (lambda (step) (car step)))
(define get-rule (lambda (step) (cadr step)))

;; A proof is one of:
;; (list Theorem Steps)
(define Prove
  (lambda (theo steps)
    (cond
     ((andmap step? steps)
      (apply-step* theo steps))
     (else theo))))
(define apply-step*
  (lambda (theo steps)
    (cond
     ((null? steps) theo)
     (else (apply-step* (apply-step theo (car steps))
                        (cdr steps))))))
(define apply-step
  (lambda (theo step)
    (let ([path (get-path step)]
          [rule (get-rule step)])
      (cond
       ((null? path) ((get-rule step) theo))
       ((tree? theo)
        (let ([lbl (car theo)]
              [fst (cadr theo)]
              [snd (caddr theo)]
              [step^ (list (cdr path) rule)])
          (cond
           ((= (car path) 1)
            `(,lbl ,(apply-step fst step^) ,snd))
           (else `(,lbl ,fst ,(apply-step snd step^))))))
       (else theo)))))

;; tbu for disj
(define apply-choice
  (lambda (tree choice)
    (cond
     ((= choice 1) (car tree))
     (else (cadr tree)))))
(define choose
  (lambda (choice)
    (lambda (tree)
      (if (disj? tree)
          (apply-choice tree choice)
          tree))))

;; tbu for conj
(define end
  (lambda ()
    (lambda (exp)
      (let ([fst (cadr exp)]
            [snd (cadr exp)])
        (if (and (boolean? fst) (boolean? snd))
            (and fst snd)
            `(conj ,fst ,snd))))))

;;; Right rules are really simple
;;; Simply, create a new proof or a list of goals
(define apply-right-rule
  (lambda ()
    (lambda (proof)
      (let ([l (left proof)] [r (right proof)])
        (case (reg-op r)
          ((>) (start (cons (car r) l) (cddr r)))
          ((^) `(conj ,(start l (list (car r))) ,(start l (cddr r))))
          ;; maybe put an `or` since these should simplify to #t
          ((v) `(disj ,(start l (list (car r))) ,(start l (cddr r))))
          ;; the event right is a singleton
          ((#f) proof))))))

;;; Left is a bit more complicated
;;; We need to find a target expression
;;; Make sure it actually exists, remove it, apply a rule to it,
;;; then add/create/update goals
(define find-left-expr
  (lambda (left exp)
    (if (null? left) #f
        (or (equal? left exp)
            (equal? (car left) exp)
            (find-left-expr (cdr left) exp)))))
(define remove-left-expr
  (lambda (left exp)
    (cond
     ((or (null? left) (equal? left exp))  '())
     ((equal? (car left) exp)
      (remove-left-expr (cdr left) exp))
     (else (cons (car left) (remove-left-expr (cdr left) exp))))))
(define apply-left-rule
  (lambda (exp)
    (lambda (proof)
      (if (find-left-expr (left proof) exp)
        (let ([l (remove-left-expr (left proof) exp)]
              [r (right proof)])
          (case (reg-op exp)
            ((>) `(conj ,(start l (list (car exp)))
                        ,(start (append l (cddr exp)) r)))
            ((^) (start (append l (list (car exp) (caddr exp))) r))
            ((v) `(disj ,(start (append l (list (car exp))) r)
                        ,(start (append l (list (caddr exp))) r)))
            ;; should this be `proof` or `(start left right)`?
            ((#f) proof)))
        proof))))
