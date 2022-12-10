;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1
;; Write a Racket function racketlist->mupllist that takes a Racket list
;; (presumably of mupl values but that will not affect your solution)
;; and produces an analogous mupl list with the same elements in the same order
(define (racketlist->mupllist xs)
  (cond [(null? xs) (aunit)]
        [#t (apair (car xs) (racketlist->mupllist (cdr xs)))]))

;; CHANGE (put your solutions here)

;; Problem 2
;; Write a Racket function mupllist->racketlist that takes a mupl list
;; (presumably of mupl values but that will not affect your solution) and
;; produces an analogous Racket list (of mupl values) with the same elements
;; in the same order.
(define (mupllist->racketlist xs)
  (cond [(aunit? xs) null]
        [#t (cons (apair-e1 xs) (mupllist->racketlist (apair-e2 xs)))]))

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(int? e) e]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1) (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL ifgreater applied to non-number")))]
        [(fun? e)
         (closure env (fun (fun-nameopt e) (fun-formal e) (fun-body e)))]
        [(call? e)
         (let ([v1 (eval-under-env (call-funexp e) env)]
               [v2 (eval-under-env (call-actual e) env)])
           (if (closure? v1)
               (eval-under-env (fun-body (closure-fun v1))
                               (if (fun-nameopt(closure-fun v1))
                                   (append (list (cons (fun-nameopt (closure-fun v1)) v1)
                                                 (cons (fun-formal(closure-fun v1)) v2))
                                           (closure-env v1))
                                   (append (list (cons (fun-formal (closure-fun v1)) v2))
                                           (closure-env v1))))
               (error "MUPL cannot call a function that is not a closure")))]
        [(mlet? e)
         (letrec ([v (eval-under-env (mlet-e e) env)]
                  [env-n (cons (cons (mlet-var e) v) env)])
           (eval-under-env (mlet-body e) env-n))]
        [(apair? e)
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2))]
        [(fst? e)
         (let ([v (eval-under-env (fst-e e) env)])
           (if (apair? v)
               (apair-e1 v)
               (error "MUPL fst on a non-pair")))]
        [(snd? e)
         (let  ([v (eval-under-env (snd-e e) env)])
           (if (apair? v)
               (apair-e2 v)
               (error "MUPL snd on a non-pair")))]
        [(aunit? e) e]
        [(isaunit? e)
         (let ([v (eval-under-env (isaunit-e e) env)])
           (if (aunit? v)
               (int 1)
               (int 0)))]
        [(closure? e) e]
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

;; Write a Racket function ifaunit that takes three mupl expressions e1, e2, and e3.
;; It returns a mupl expression that when run evaluates e1 and if the result is mupl’s
;; aunit then it evaluates e2 and that is the overall result, else it evaluates e3 and
;; that is the overall result.
(define (ifaunit e1 e2 e3)
  (ifgreater (isaunit e1) (int 0) e2 e3))

;; Write a Racket function mlet* that takes a Racket list of Racket pairs
;; ’((s1 . e1) ...(si . ei) ...(sn . en)) and a final mupl expression en+1.
;; In each pair, assume si is a Racket string and ei is a mupl expression.
;; mlet* returns a mupl expression whose value is en+1 evaluated in an environment
;; where each si is a variable bound to the result of evaluating the corresponding
;; ei for 1 ≤ i ≤ n. The bindings are done sequentially, so that each ei is evaluated
;; in an environment where s1 through si−1 have been previously bound to the values e1 through ei−1.
(define (mlet* lstlst e2)
  (cond [(null? lstlst) e2]
        [#t (mlet (car (car lstlst)) (cdr (car lstlst)) (mlet* (cdr lstlst) e2))]))

;; Write a Racket function ifeq that takes four mupl expressions e1, e2, e3, and e4 and returns a
;; mupl expression that acts like ifgreater except e3 is evaluated if and only if e1 and e2 are equal
;; integers. Assume none of the arguments to ifeq use the mupl variables _x or _y. Use this assumption
;; so that when an expression returned from ifeq is evaluated, e1 and e2 are evaluated exactly once each.
(define (ifeq e1 e2 e3 e4)
  (ifgreater e1 e2 (ifgreater e2 e1 e3 e4) (ifgreater e2 e1 e4 e3)))

;; Problem 4
;; Bind to the Racket variable mupl-map a mupl function that acts like map (as we used extensively in ML).
;; Your function should be curried: it should take a mupl function and return a mupl function that takes a
;; mupl list and applies the function to every element of the list returning a new mupl list. Recall a mupl
;; list is aunit or a pair where the second component is a mupl list.
(define mupl-map
  (fun "map-fun" "func"
       (fun "map-xs" "xs"
            (ifaunit (var "xs")
                     (aunit)
                     (apair (call (var "func") (fst (var "xs")))
                            (call (var "map-xs") (snd (var "xs"))))))))

;; Bind to the Racket variable mupl-mapAddN a mupl function that takes an mupl integer i and returns a mupl
;; function that takes a mupl list of mupl integers and returns a new mupl list of mupl integers that adds i
;; to every element of the list. Use mupl-map (a use of mlet is given to you to make this easier).
(define mupl-mapAddN 
  (mlet "map" mupl-map
        (fun "mupl-fun-int" "i"
	     (fun "mupl-fun-list" "ml-int"
		  (call (call (var "map") (fun "add-i" "x" (add (var "x") (var "i"))))
			(var "ml-int"))))))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e)
  (struct res (e fvs)) ; result type of f (could also use a pair)
    (define (f e) 
      (cond [(var? e) (res e (set (var-string e)))]
            [(int? e) (res e (set))]
            [(add? e) (let ([r1 (f (add-e1 e))]
                            [r2 (f (add-e2 e))])
                        (res (add (res-e r1) (res-e r2))
                             (set-union (res-fvs r1) (res-fvs r2))))]
            [(ifgreater? e) (let ([r1 (f (ifgreater-e1 e))]
                                  [r2 (f (ifgreater-e2 e))]
                                  [r3 (f (ifgreater-e3 e))]
                                  [r4 (f (ifgreater-e4 e))])
                              (res (ifgreater (res-e r1) (res-e r2) (res-e r3)    (res-e r4))
                                  (set-union (res-fvs r1) (res-fvs r2) (res-fvs   r3) (res-fvs r4))))]
            [(fun? e) (let* ([r (f (fun-body e))]
                             [fvs (set-remove (res-fvs r) (fun-formal e))]
                             [fvs (if (fun-nameopt e) 
                                      (set-remove fvs (fun-nameopt e)) 
                                      fvs)])
                        (res (fun-challenge (fun-nameopt e) (fun-formal e) 
                                            (res-e r) fvs)
                            fvs))]
            [(call? e) (let ([r1 (f (call-funexp e))]
                             [r2 (f (call-actual e))])
                        (res (call (res-e r1) (res-e r2))
                             (set-union (res-fvs r1) (res-fvs r2))))]
            [(mlet? e) (let* ([r1 (f (mlet-e e))]
                              [r2 (f (mlet-body e))])
                         (res (mlet (mlet-var e) (res-e r1) (res-e r2))
                              (set-union (res-fvs r1) (set-remove (res-fvs r2)   (mlet-var e)))))]
            [(apair? e) (let ([r1 (f (apair-e1 e))]
                              [r2 (f (apair-e2 e))])
                          (res (apair (res-e r1) (res-e r2))
                             (set-union (res-fvs r1) (res-fvs r2))))]
            [(fst? e) (let ([r (f (fst-e e))])
                        (res (fst (res-e r))
                             (res-fvs r)))]
            [(snd? e) (let ([r (f (snd-e e))])
                        (res (snd (res-e r))
                             (res-fvs r)))]
            [(aunit? e) (res e (set))]
            [(isaunit? e) (let ([r (f (isaunit-e e))])
                            (res (isaunit (res-e r))
                                 (res-fvs r)))]))
    (res-e (f e)))

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env)
  (cond 
        [(fun-challenge? e)
         (closure (set-map (fun-challenge-freevars e)
                           (lambda (s) (cons s (envlookup env s))))
                  e)]
         ; call case uses fun-challenge as appropriate
         ; all other cases the same
        ...))

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
