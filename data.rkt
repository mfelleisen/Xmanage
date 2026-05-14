#lang racket

;; the basic data representation of an account

(provide
 (contract-out

  (struct account
    ([name         string?]
     [recents      (listof dw?)]
     [balance      amount?]
     [balance-date my-date?]
     [history      (listof dw?)]
     [check-no     natural?]))

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
   (-> any/c boolean?)]))

(provide
 ;; match expander
 amount>

 argv-amount>)

(module+ examples
  (provide checking0))

;; -------------------------------------------------------------------------------------------------
(require "../Xmanaged/date.rkt")

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

(define amount? rational?)

(define-match-expander amount>
  (λ (stx)
    (syntax-case stx ()
      [(_ x) #'(app string->number (and amount? x))])))

(define-match-expander argv-amount>
  (λ (stx)
    (syntax-case stx ()
      [(_ x) #'(app string->number (and amount? positive? x))])))

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
  
(struct account (name recents balance balance-date history check-no) #:transparent #:mutable)
(struct dw (comment date amount) #:transparent #:mutable)
#; {type Account = (account String [Listof T] Rational Date [Listof T] Natural)}
#; {type DW      = (dw String Date PositiveRational)}

(module+ examples
  (define [checking0 (first-check-no 0)]
    (account "checking" '() 0 (today) '() first-check-no)))

#; {String String -> Void}
;; EFFECT create a new account file 
(define (new-account name #:first-check# (last 0))
  (account name '() 0 (today) '() last))

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
;                     ; 
;          ;;;        ; 
;            ;        ; 
;    ;;;     ;     ;;;; 
;   ;; ;;    ;    ;; ;; 
;   ;   ;    ;    ;   ; 
;   ;   ;    ;    ;   ; 
;   ;   ;    ;    ;   ; 
;   ;; ;;    ;    ;; ;; 
;    ;;;      ;;   ;;;; 
;                       
;                       
;                       

(define SPECIAL 5) ;; the 5th of every month is special charges day
(define DEDUCTS 
  '(
    #;
    (" heloc                       " 3188.36)
    #;
    (" condo fee                   "  500.00)))

(define DEDUCTS-chase
  '((" mortgage                    " 2209.74)
    (" INVEST VAN                  "  200.00)))

(define DEDUCTS-condo
  '(
    (" insurance (barry/mcchough)   "  622.17))) #; [415.33 423.25 480.64 509.71 540.62]