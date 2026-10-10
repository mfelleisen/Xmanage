#lang racket

;; the basic data representation of an account

(define string-amount
  (and/c string? (flat-named-contract 'amount (λ (x) (regexp-match #px"\\d*\\.\\d\\d" x)))))

(provide
 (contract-out

  (struct account
    ([name         string?]
     [recents      (listof dw?)]
     [balance      amount?]
     [balance-date my-date?]
     [history      (listof dw?)]
     [check-no     natural?]
     [special      [listof (list/c string? amount?)]]))

  (struct dw
    ([comment string?]
     [date    my-date?]
     [amount  amount?]))
  
  (transactions->balance
   (-> (listof dw?) amount?))

  [transactions->running-balance
    (->i ([dws (listof dw?)]
          [balance amount?]
          [op (-> amount? amount? amount?)])
         (r (dws) (and (listof (list/c dw? amount?))
                       (λ (r) (equal? dws (reverse (map first r)))))))]

  [amount?
   (-> any/c boolean?)]

  [amount->decimal-string
   (-> rational? string-amount)]

  (align
   (-> (and/c string? shorter-than-COMMENT-MAX)
       (and/c string? (compose (=/c COMMENT-MAX) string-length))))))

(provide
 ;; match expander
 amount>

 argv-amount>)

(module+ examples
  (provide checking0))

;; -------------------------------------------------------------------------------------------------
(require Xmanage/private/date)

(module+ test
  (require (submod ".."))
  (require (submod ".." examples))
  (require rackunit))

;                                                   
;                                                   
;                                        ;          
;                                        ;          
;   ;;;;  ;;;;;;   ;;;   ;   ;  ; ;;   ;;;;;   ;;;  
;       ; ;  ;  ; ;; ;;  ;   ;  ;;  ;    ;    ;   ; 
;       ; ;  ;  ; ;   ;  ;   ;  ;   ;    ;    ;     
;    ;;;; ;  ;  ; ;   ;  ;   ;  ;   ;    ;     ;;;  
;   ;   ; ;  ;  ; ;   ;  ;   ;  ;   ;    ;        ; 
;   ;   ; ;  ;  ; ;; ;;  ;   ;  ;   ;    ;    ;   ; 
;    ;;;; ;  ;  ;  ;;;    ;;;;  ;   ;    ;;;   ;;;  
;                                                   
;                                                   
;                                                   

(define (amount? x)
  (and (rational? x) (exact? x)))

(module+ test
  (check-true (amount? 0)))

(define-match-expander amount>
  (λ (stx)
    (syntax-case stx ()
      [(_ x) #'(app string->number/2 (? amount? x))]))
  (λ (stx)
    (syntax-case stx ()
      [(_ expr) #'(cond
                    [(number? expr) (inexact->exact expr)]
                    [(string->number/2 expr)
                     (λ (x)
                       (if (amount? x)
                           x
                           (error 'amount> "amount expected, given ~s" x)))]
                    [else (error 'amount> "amount expected, given ~s" expr)])])))

(define-match-expander argv-amount>
  (λ (stx)
    (syntax-case stx ()
      [(_ x) #'(app string->number/2 (and (? amount?) (? (lambda (x) (>= x 0)) x)))])))

(module+ test
  (match "159.20"
    [(argv-amount> x) (check-false (inexact? x) "argv match")])
  (match "0."
    [(argv-amount> x) (check-true (= x 0))]))

(define (string->number/2 amount)
  (when (regexp-match #px"\\.\\d\\d\\d" (~a amount))
    (eprintf "bad amount ~a\n" amount))
  (parameterize ([read-decimal-as-inexact #false])
    (string->number amount)))

(module+ test
  (check-equal? (string->number/2 "1.23") #e1.23)
  (check-match (with-output-to-string
                 (λ ()
                   (parameterize ([current-error-port (current-output-port)])
                     (check-equal? (string->number/2 "1.234") 1.234))))
               (pregexp "bad")))

;; ---------------------------------------------------------------------------------------------------
(define (amount->decimal-string a)
  (~r a #:precision '(= 2)))
    
(module+ test
  (check-equal? (amount->decimal-string 3.0) "3.00")
  (check-equal? (amount->decimal-string 43.12) "43.12")
  (check-equal? (amount->decimal-string 43.1) "43.10")
  (check-equal? (amount->decimal-string 43.0) "43.00"))


;                                                          
;                                                          
;                                               ;          
;                                               ;          
;   ;;;;    ;;;    ;;;    ;;;   ;   ;  ; ;;   ;;;;;   ;;;  
;       ;  ;;  ;  ;;  ;  ;; ;;  ;   ;  ;;  ;    ;    ;   ; 
;       ;  ;      ;      ;   ;  ;   ;  ;   ;    ;    ;     
;    ;;;;  ;      ;      ;   ;  ;   ;  ;   ;    ;     ;;;  
;   ;   ;  ;      ;      ;   ;  ;   ;  ;   ;    ;        ; 
;   ;   ;  ;;     ;;     ;; ;;  ;   ;  ;   ;    ;    ;   ; 
;    ;;;;   ;;;;   ;;;;   ;;;    ;;;;  ;   ;    ;;;   ;;;  
;                                                          
;                                                          
;                                                          
  
(struct account (name recents balance balance-date history check-no special) #:transparent #:mutable)
(struct dw (comment date amount) #:transparent #:mutable)
#; {type Account = (account String [Listof T] Rational Date [Listof T] Natural [Listof [List String Amount]])}
#; {type DW      = (dw String Date PositiveRational)}

(module+ examples
  (define [checking0 (first-check-no 0)]
    (account "checking" '() 0 (today) '() first-check-no '())))

#; {String String -> Void}
;; EFFECT create a new account file 
(define (new-account name #:first-check# (last 0))
  (account name '() 0 (today) '() last '[]))

#; {[List DW] -> Number}
(define (transactions->balance dw*)
  (for/sum ([h dw*])
    (dw-amount h)))

#; {[Listof DW] Amount [Amount Amount -> Amount] -> [Listof {List DW Amount}]}
(define (transactions->running-balance history current op)
  (for/fold ([balance current][r '()] #:result r) ([h history])
    (define balance+ (op balance (dw-amount h)))
    (define dw+      (list h balance+))
    (values balance+ (cons dw+ r))))

;                                                   
;                                                   
;                                                   
;                                                   
;   ;;;;   ;   ;   ;;;;  ;;;;    ;;;    ;;;    ;;;  
;   ;; ;;  ;   ;   ;;  ; ;; ;;  ;; ;;  ;   ;  ;;  ; 
;   ;   ;  ;   ;   ;     ;   ;  ;   ;  ;      ;   ;;
;   ;   ;  ;   ;   ;     ;   ;  ;   ;   ;;;   ;;;;;;
;   ;   ;  ;   ;   ;     ;   ;  ;   ;      ;  ;     
;   ;; ;;  ;   ;   ;     ;; ;;  ;; ;;  ;   ;  ;     
;   ;;;;    ;;;;   ;     ;;;;    ;;;    ;;;    ;;;; 
;   ;                    ;                          
;   ;                    ;                          
;   ;                    ;                          

(define COMMENT-MAX 30)

#; {String -> Boolean}
(define shorter-than-COMMENT-MAX
  (flat-named-contract "short purpose comment" (λ (s) (<= (string-length s) COMMENT-MAX))))

#; {String CheckNoString -> String}
(define (align purpose (x ""))
  (~a " " x " " purpose #:max-width COMMENT-MAX #:min-width COMMENT-MAX #:left-pad-string " "))

(module+ test ;; testing basic comment property
  (define LARGE (make-string COMMENT-MAX #\space))
  (check-equal? (string-length (align LARGE)) COMMENT-MAX "t")

  (check-equal? (string-length (align " Claudia")) COMMENT-MAX)
  (check-equal? (string-length (align " Claudia")) COMMENT-MAX)
  (check-equal? (string-length (align " C")) COMMENT-MAX))

;                                     
;                                     
;     ;                    ;          
;     ;                    ;          
;   ;;;;;   ;;;    ;;;   ;;;;;   ;;;  
;     ;    ;;  ;  ;   ;    ;    ;   ; 
;     ;    ;   ;; ;        ;    ;     
;     ;    ;;;;;;  ;;;     ;     ;;;  
;     ;    ;          ;    ;        ; 
;     ;    ;      ;   ;    ;    ;   ; 
;     ;;;   ;;;;   ;;;     ;;;   ;;;  
;                                     
;                                     
;                                     

(module+ test
  (check-equal? (checking0) (new-account "checking"))

  (define dw0 (dw "test" (today) #e10))
  (check-equal? (transactions->running-balance (list dw0) #e10. -) (list (list dw0 0))
                "running balance: an account w/o initial deposit")
  
  (check-equal? (transactions->balance (list dw0)) 10
                "balance: an account w/o initial deposit"))
