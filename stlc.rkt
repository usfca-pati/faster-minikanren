#lang racket

; Simply-Typed λ Calculus typechecker in miniKanren

; τ ∈ Type ::= int | τ -> τ | τ × τ

; e ∈ Expr ::= x ∈ Var | n ∈ Z | e e | cons e e | e + e | fst e | snd e | λx: τ. e

; Γ ∈ Context = Var → Type

(require "mk.rkt")

(define success (== 'a 'a))
(define failure (== 'a 'b))

(defrel (lookup Γ x τ)
  (fresh (t)
         (conde
          ((== (cons (cons x τ) t) Γ))
          ((fresh (h)
                  (== (cons h t) Γ)
                  (lookup t x τ))))))

(define (bind x τ Γ)
  (cons (cons x τ) Γ))

(define (lift f . args)
  (if (apply f args) success failure))

(defrel (type-of depth Γ e τ)
  (match depth
    [(list _ 0) failure]
    [(list lower upper)
     (let ([depth1 (list (sub1 lower) (sub1 upper))])
       (if (<= lower 0)
           ; hit the base cases
           (fresh (x e1 e2 τ1 τ2)
                  (conde
                   [(== e `(var ,x)) (lookup Γ x τ)] ; t-var
                   [(numbero e) (== τ 'int)] ; t-num
                   [(== e `(app ,e1 ,e2)) ; ->-E
                    (type-of depth1 Γ e1 `(-> ,τ1 ,τ))
                    (type-of depth1 Γ e2 τ1)]
                   [(== e `(cons ,e1 ,e2)) ; ×-I
                    (== τ `(* ,τ1 ,τ2))
                    (type-of depth1 Γ e1 τ1)
                    (type-of depth1 Γ e2 τ2)]
                   [(== e `(+ ,e1 ,e2)) ; t-add
                    (== τ 'int)
                    (type-of depth1 Γ e1 'int)
                    (type-of depth1 Γ e2 'int)]
                   [(== e `(fst ,e1)) ; ×-E1
                    (type-of depth1 Γ e1 `(* τ τ1))]
                   [(== e `(snd ,e1)) ; ×-E2
                    (type-of depth1 Γ e1 `(* τ1 τ))]
                   [(== e `(λ ,x ,τ1 ,e1)) ; ->-I
                    (== τ `(-> ,τ1 ,τ2))
                    (type-of depth1 (bind x τ1 Γ) e1 τ2)]))
           ; don't use the base cases
           (fresh (x e1 e2 τ1 τ2)
                  (conde
                   [(== e `(app ,e1 ,e2)) ; ->-E
                    (type-of depth1 Γ e1 `(-> ,τ1 ,τ))
                    (type-of depth1 Γ e2 τ1)]
                   [(== e `(cons ,e1 ,e2)) ; ×-I
                    (== τ `(* ,τ1 ,τ2))
                    (type-of depth1 Γ e1 τ1)
                    (type-of depth1 Γ e2 τ2)]
                   [(== e `(+ ,e1 ,e2)) ; t-add
                    (== τ 'int)
                    (type-of depth1 Γ e1 'int)
                    (type-of depth1 Γ e2 'int)]
                   [(== e `(fst ,e1)) ; ×-E1
                    (type-of depth1 Γ e1 `(* τ τ1))]
                   [(== e `(snd ,e1)) ; ×-E2
                    (type-of depth1 Γ e1 `(* τ1 τ))]
                   [(== e `(λ ,x ,τ1 ,e1)) ; ->-I
                    (== τ `(-> ,τ1 ,τ2))
                    (type-of depth1 (bind x τ1 Γ) e1 τ2)]))))]))

(defrel (has-app e)
  (fresh
   (x t e1 e2)
   (conde
    [(== e `(cons ,e1 ,e2))]
    [(== e `(lambda ,x ,t ,e1)) (has-app e1)]
    [(== e `(fst ,e1)) (has-app e1)]
    [(== e `(snd ,e1)) (has-app e1)]
    [(== e `(app ,e1 ,e2)) (conde ((has-app e1)) ((has-app e2)))]
    [(== e `(+ ,e1 ,e2)) (conde ((has-app e1)) ((has-app e2)))])))
           

; (require "hamt/weak-hamt.rkt") (start-will-executor!)
(time (begin
        (run 1 (e) (fresh (t) (type-of '(6 10) '() e t) (has-app e))))
      #t)

(displayln (/ (current-memory-use) (* 1024.0 1024.0)))

(collect-garbage)
(collect-garbage)

(displayln (/ (current-memory-use) (* 1024.0 1024.0)))
