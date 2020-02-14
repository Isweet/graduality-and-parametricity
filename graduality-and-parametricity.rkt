#lang racket
(require redex)

(define-language PolyGν
  ((A B) ?
         X
         Bool
         (× A A)
         (→ A A)
         (∃ν X A)
         (∀ν X A))
  (G X
     Bool
     (× ? ?)
     (→ ? ?)
     (∃ν X ?)
     (∀ν X ?))
  ((M N) x
         (:: M A)
         (seal X M)
         (unseal X M)
         (is? G M)
         true
         false
         (if M M M)
         (pair M M)
         (let (= (pair x x) M) M)
         (M M)
         (λ (x A) M)
         (packν (≅ X A) M)
         (unpack (= (pair X x) M) M)
         (Λν X M)
         (M (≅ X A))
         (let (= x M) M))
  (F (: x A)
     X
     (≅ X A))
  (Γ (F ...))
  ((x X) variable-not-otherwise-mentioned))

;; TODO(ins): Make these into tests

;; P × U
(redex-match
 PolyGν
 A
 (term (× P U)))

;; unseal X ((Λ X . λ x : X . x :: X){X ≅ B}(seal X true))
(redex-match
 PolyGν
 M
 (term (unseal X (((Λν X (λ (x X) (:: x X))) (≅ X Bool)) (seal X true)))))

(define-judgment-form
  PolyGν
  #:mode (~ I I)
  #:contract (~ A A)

  [--------------------- DynL
   (~ ? A)]

  [--------------------- DynR
   (~ A ?)]

  [---------------------
   (~ Bool Bool)]

  [---------------------
   (~ X X)]

  [ (~ A_0 B_0) (~ A_1 B_1)
    ---------------------
    (~ (→ A_0 A_1) (→ B_0 B_1))]

  [ (~ A_0 B_0) (~ A_1 B_1)
    ---------------------
    (~ (× A_0 A_1) (× B_0 B_1))]

  [ (~ A_0 A_1)
    ---------------------
    (~ (∃ν X A_0) (∃ν X A_1))]

  [ (~ A_0 A_1)
    ---------------------
    (~ (∀ν X A_0) (∀ν x A_1))])

(define-judgment-form
  PolyGν
  #:mode (lookup I O)
  #:contract (lookup (F ...) F)
  [(lookup (_ ... F _ ...) F)])

(define-judgment-form
  PolyGν
  #:mode (⊢ I I O O)
  #:contract (⊢ Γ M A Γ)

  [ (⊢ Γ M A Γ_0)
    (~ A B)
    --------------------- Asc
    (⊢ Γ (:: M B) B Γ_0)]

  [ (lookup Γ (: x A))
   --------------------- Var
   (⊢ Γ x A ())]

  [ (⊢ (F ...) M A (F_1 ...))
    (⊢ (F ... F_1 ... (: x A)) N B (F_2 ...))
    ----------------------
    (⊢ (F ...) (let (= x M) N) B (F_1 ... F_2 ...))]

  [ (⊢ (F ...) M B (F_0 ...))
    (lookup (F ... F_0 ...) (≅ X A))
    (~ B A)
    ----------------------
    (⊢ (F ...) (seal X M) X (F_0 ...))]


  )

(judgment-holds (⊢ ((: y Bool)) (let (= x y) x) A Γ) (A Γ))
(judgment-holds (⊢ ((: y (× ? Bool)) (≅ Y (× Bool Bool))) (seal Y y) A Γ) (A Γ))