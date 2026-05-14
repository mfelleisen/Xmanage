#lang racket

;; the actions that can be performed on an account

(provide
 (contract-out
  [name->path
   (->* (string?) (#:kind string?) path?)]

  [new-account
   (->* (string?) (#:first-check# natural?) account?)]
  [deposit
   ;; consumes command-line arguments to deposit amount 
   (->* (account? my-date?) () #:rest (listof string?) account?)]
  [withdraw
   ;; consumes commad-line arguments to withdraw amount
   (->* (account? my-date?) () #:rest (listof string?) account?)]
  [write-check
   ;; consumes commad-line arguments to record a check at amount
   (->* (account? my-date? string? string?) () account?)]
  [show-balance
   (->* (account? my-date?) () #:rest any/c amount?)])
  
 show-statement
 to-html)

;; ---------------------------------------------------------------------------------------------------
(require "../Xmanaged/data.rkt")
(require "../Xmanaged/date.rkt")
(require (lib "decimals.ss" "utils"))
(require (only-in xml xexpr->xml display-xml/content))

(module+ test
  (require (submod ".."))
  (require (submod "../Xmanaged/file-io.rkt" examples))
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
(define COMMENT-MAX 30)

;                                                   
;                                                   
;            ;                                      
;            ;                                      
;    ;;;   ;;;;;   ;;;    ;;;;  ;;;;    ;;;;   ;;;  
;   ;   ;    ;    ;; ;;   ;;  ;     ;  ;;  ;  ;;  ; 
;   ;        ;    ;   ;   ;         ;  ;   ;  ;   ;;
;    ;;;     ;    ;   ;   ;      ;;;;  ;   ;  ;;;;;;
;       ;    ;    ;   ;   ;     ;   ;  ;   ;  ;     
;   ;   ;    ;    ;; ;;   ;     ;   ;  ;; ;;  ;     
;    ;;;     ;;;   ;;;    ;      ;;;;   ;;;;   ;;;; 
;                                          ;        
;                                       ;  ;        
;                                        ;;         

(define HOME (find-system-path 'home-dir))

(define (name->path name #:kind (kind #false))
  (build-path HOME "Private" "Accounts" "Xmanaged" (if kind name (~a "." name ".act"))))

;                       
;                       
;                       
;                       
;   ; ;;    ;;;  ;     ;
;   ;;  ;  ;;  ; ;     ;
;   ;   ;  ;   ;; ; ; ; 
;   ;   ;  ;;;;;; ; ; ; 
;   ;   ;  ;      ;; ;; 
;   ;   ;  ;      ;; ;; 
;   ;   ;   ;;;;   ; ;  
;                       
;                       
;                       

#; {String String -> Void}
;; EFFECT create a new account file 
(define (new-account name #:first-check# (last 0))
  (account name '() 0 (today) '() last))

;                                                                                                    
;              ;      ;                                     ;                                        
;              ;      ;           ;;;                       ;        ;                           ;   
;              ;      ;          ;                          ;        ;                           ;   
;   ;;;;    ;;;;   ;;;;          ;             ;;;   ;   ;  ;;;;   ;;;;;   ;;;;  ;;;;    ;;;   ;;;;; 
;       ;  ;; ;;  ;; ;;          ;;           ;   ;  ;   ;  ;; ;;    ;     ;;  ;     ;  ;;  ;    ;   
;       ;  ;   ;  ;   ;          ;;           ;      ;   ;  ;   ;    ;     ;         ;  ;        ;   
;    ;;;;  ;   ;  ;   ;         ;  ; ;         ;;;   ;   ;  ;   ;    ;     ;      ;;;;  ;        ;   
;   ;   ;  ;   ;  ;   ;         ;  ;;;            ;  ;   ;  ;   ;    ;     ;     ;   ;  ;        ;   
;   ;   ;  ;; ;;  ;; ;;         ;;  ;         ;   ;  ;   ;  ;; ;;    ;     ;     ;   ;  ;;       ;   
;    ;;;;   ;;;;   ;;;;          ;;; ;         ;;;    ;;;;  ;;;;     ;;;   ;      ;;;;   ;;;;    ;;; 
;                                                                                                    
;                                                                                                    
;                                                                                                    

#; {{U + -} -> (Account Date Amount Any ... -> Account)}
(define ((make-d/w how) account date  . other)
  (define-values (amount msg)
    (match other
      [(list (argv-amount> a) (? string? m))
       (values a m)]
      [_
       (error 'make-d/w "(amount and purpose) expected, found ~a" other)]))
  (define history (account-recents account))
  (define delta   (how 0 amount))
  (define action  (dw msg date delta))
  (set-account-recents! account (cons action history))
  account)

#; (Account Date Amount String -> Void)
;; effect: record a deposit
(define deposit (make-d/w +))

#; (Account Date Amount String -> Void)
;; effect: record a non-check withdrawl
(define withdraw (make-d/w -))

#; (Account Date Amount Comment -> Void)
;; EFFECT confirm check writing action with a print to console 
(define (write-check account date amount purpose)
  (begin0
    (withdraw account date amount (create-check account purpose))
    ;; confirmation 
    (printf "Check No. ~a Today: ~a~n" (account-check-no account) (today))))

#; [Account String -> String]
;; create entry for a check in `acc` 
;; EFFECT increase check number in account 
(define (create-check acc comment)
  (define x (+ (account-check-no acc) 1))
  (set-account-check-no! acc x)
  (~a " " x " " comment #:max-width COMMENT-MAX #:min-width COMMENT-MAX #:left-pad-string " "))

;                                                   
;   ;                                               
;   ;             ;;;                               
;   ;               ;                               
;   ;;;;   ;;;;     ;    ;;;;   ; ;;    ;;;    ;;;  
;   ;; ;;      ;    ;        ;  ;;  ;  ;;  ;  ;;  ; 
;   ;   ;      ;    ;        ;  ;   ;  ;      ;   ;;
;   ;   ;   ;;;;    ;     ;;;;  ;   ;  ;      ;;;;;;
;   ;   ;  ;   ;    ;    ;   ;  ;   ;  ;      ;     
;   ;; ;;  ;   ;    ;    ;   ;  ;   ;  ;;     ;     
;   ;;;;    ;;;;     ;;   ;;;;  ;   ;   ;;;;   ;;;; 
;                                                   
;                                                   
;                                                   

(define (show-balance a _date . _others)
  (define b (+ (transactions->balance (account-recents a)) (account-balance a)))
  b
  #;
  (values
    b 
    (λ () (printf "~a\n" (number->decimal-string b)))))

;                                            
;                            ;               
;                            ;               
;                            ;               
;    ;;;;   ;;;   ; ;;    ;;;;   ;;;    ;;;; 
;    ;;  ; ;;  ;  ;;  ;  ;; ;;  ;;  ;   ;;  ;
;    ;     ;   ;; ;   ;  ;   ;  ;   ;;  ;    
;    ;     ;;;;;; ;   ;  ;   ;  ;;;;;;  ;    
;    ;     ;      ;   ;  ;   ;  ;       ;    
;    ;     ;      ;   ;  ;; ;;  ;       ;    
;    ;      ;;;;  ;   ;   ;;;;   ;;;;   ;    
;                                            
;                                            
;                                            

(define show-statement void)

#; {Account -> Void}
(define (to-html a)
  (define page:xml (xexpr->xml (account-to-html a)))
  (define file     (name->path (account-name a) #:kind '.html))
  (with-output-to-file file #:exists 'replace (lambda () (display-xml/content page:xml)))
  (system (format "open ~a" file)))

(define COMPLETE "Complete transactions for ")

#; {Account -> Xexpr}
(define (account-to-html a)

  (define title (~a COMPLETE (account-name a)))
  (define date  (date-to-html (account-balance-date a)))

  (define b (account-balance a))
  (define history+ [transactions->running-balance (identity (account-history a)) b -])
  (define recents+ [transactions->running-balance (reverse (account-recents a)) b +])
   
  (define recent:xexpr  (transactions-to-html recents+))
  (define history:xexpr (transactions-to-html history+))
  
  (assemble-page title date (number->decimal-string b) recent:xexpr history:xexpr))

#; {String String String Xexpr Xexpr -> Xexpr}
(define (assemble-page title date b recent:xexpr history:xexpr)
  `(html
    (head
     (title ,title))
    (body
     (blockquote 
      (h3 ,title)
      (table ([border "0"] [cellpadding "0"] [cellspacing "0"])
             ,(headers)
             ,@recent:xexpr
             ,@(assemble-separators date b)
             ,@(reverse history:xexpr))))))

#; {String String -> [List TR TR]}
(define (assemble-separators date b)
  (define blk '(th " "))
  `((tr ((bgcolor "pink")) ,blk ,@(2-aligned-cells "History:" date) ,blk ,blk ,blk) 
    (tr ((bgcolor "pink")) ,blk ,@(2-aligned-cells "Balance:" b)    ,blk ,blk ,blk)))

#; {-> TR}
(define (headers)
  (html-row '() 'th
            #:date       (td-center "Date"          100)
            #:check      (td-right  "Check #"        66)
            #:purpose    (td-center "Purpose"       200)
            #:withdrawal (td-right  "Withdrawal"     88) 
            #:deposit    (td-right  "Deposit"        88) 
            #:total      (td-right  "Running Total" 111)
            #:yes        (td-center  ""              44)))

#; {Striing String -> [List TH TH TH]}
(define (2-aligned-cells word other)
  `[(th ((align "left")) ,word)
    (th ((align "left")) "   ")
    (th ((align "left")) ,other)])

#; {[Listof [List DW Amount]] -> [Listof TR]}
;; render the given list of tranasctions as HTML table rows with alternating color
(define (transactions-to-html transactions)
  (for/list ((t (in-list transactions)) (i (in-naturals)))
    (define dw      (first t))
    (define comment (dw-comment dw))
    (define amount  (dw-amount dw))
    (define x       (~r (abs amount) #:precision '(= 2)))
    
    (define check? (or (regexp-match #px"^ (\\d+) " comment) (regexp-match #px"^\\*(\\d+) " comment)))
    
    (html-row `([style ,(string-append "background-color:" (if (odd? i) "lightblue" "white"))]) 'td
              #:date       (td-left (date-to-html (dw-date dw)))
              #:check      (auto-or-check-to-html check? amount)
              #:purpose    (td-left (purpose-to-html check? comment))
              #:withdrawal (td-right (if (>= amount 0) "" x))
              #:deposit    (td-right (if (> amount 0)  x ""))
              #:total      (td-right (~r (second t) #:precision '(= 2)))
              #:yes        (td-center (if (regexp-match "^\\*" comment) check-mark "")))))

#; {(U False [List String String] Amount -> [List [List TD-Attribute String]])}
(define (auto-or-check-to-html check? amount)
  (td-right (if check? (second check?) (if (< amount 0) "auto" " "))))

#; {(U False [List String String]) String -> String}
(define (purpose-to-html check? comment)
  (define proper (substring comment 1))
  (cond
    [(not check?) proper]
    [else 
     (define end-of-check# (string-length (second check?)))
     (substring proper end-of-check#)]))

;; Attributes = [Listof [List Symbol String]] 
;; SA = [List Attributes String]

#; {TR-Attributes CellTag #:date SA #:purpose SA #:withdrawal SA #:deposit SA #:total SA #:yes SA
                  -> 
                  TR}
(define (html-row a tag #:date d #:purpose p #:withdrawal - #:deposit + #:total t #:yes y #:check c)
  ;; CONTRACT only `-` or `+` can come with a non-empty string 
  `(tr ,a ,@(map (lambda (x) (cons tag x)) (list d c y p - + t))))

(define (date-to-html d)
  (match-define `(,year ,month ,day) (map (lambda (x) (~r x #:min-width 2 #:pad-string "0")) d))
  (string-append month " " day " " year))

(define check-mark '(img ((src "check-mark.png") (width "22") (alt "yes"))))

(define ((make-td where) content (w #false))
  (if (boolean? w)
      `(((align ,(~a where))) ,content)
      `(((align ,(~a where)) (width ,(~a w))) ,content)))

(define td-left   (make-td 'left))
(define td-center (make-td 'center))
(define td-right  (make-td 'right))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (define recents
    (list
     (dw " 101 a check"    '(2026 6 6)  -50)
     (dw " one withdrawal" '(2026 6 6)  -50)
     (dw "*one deposit"    '(2026 6 6) +100)))
  (define with-balances (map list recents (list 0 50 100)))

  (check-equal? (transactions-to-html with-balances)
                `((tr
                   ((style "background-color:white"))
                   (td ,@(td-left "06 06 2026"))
                   (td ,@(td-right "101"))
                   (td ,@(td-center ""))
                   (td ,@(td-left " a check"))
                   (td ,@(td-right "50.00"))
                   (td ,@(td-right ""))
                   (td ,@(td-right "0.00")))
                  (tr
                   ((style "background-color:lightblue"))
                   (td ,@(td-left "06 06 2026"))
                   (td ,@(td-right "auto"))
                   (td ,@(td-center ""))
                   (td ,@(td-left "one withdrawal"))
                   (td ,@(td-right "50.00"))
                   (td ,@(td-right ""))
                   (td ,@(td-right "50.00")))
                  (tr
                   ((style "background-color:white"))
                   (td ,@(td-left  "06 06 2026"))
                   (td ,@(td-right " "))
                   (td ,@(td-center `(img ((src "check-mark.png") (width "22") (alt "yes")))))
                   (td ,@(td-left "one deposit"))
                   (td ,@(td-right ""))
                   (td ,@(td-right "100.00"))
                   (td ,@(td-right "100.00")))))
  
  (check-equal? (assemble-page "title" "date" "b" '[] '[])
                `(html
                  (head (title "title"))
                  (body
                   (blockquote
                    (h3 "title")
                    (table
                     ((border "0") (cellpadding "0") (cellspacing "0"))
                     ,(headers)
                     ,@(assemble-separators "date" "b"))))))
  
  (check-equal? (account-to-html checking)
                `(html
                  (head (title ,(~a COMPLETE (account-name checking))))
                  (body
                   (blockquote
                    (h3 ,(~a COMPLETE (account-name checking)))
                    (table
                     ((border "0") (cellpadding "0") (cellspacing "0"))
                     ,(headers)
                     ,@(assemble-separators (date-to-html (today)) "0")))))))

(module+ test
  #;
  (to-html A+100+25chk)
  #;
  (to-html chase-chk))

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

(module+ test ;; testing deposits, withdrawals, and check writing
  (define 2day '(2026 6 6))

  (check-equal? (deposit (struct-copy account Achk) 2day "100" 100purpose) A+100chk)
  (check-equal? (withdraw (struct-copy account A+100chk) 2day "50" 50purpose) A+100-50chk)
 
  (check-match
   (with-output-to-string
     (λ ()
       (check-equal? (write-check (struct-copy account A+100chk) 2day "25" 25purpose) A+100+25chk)))
   (pregexp #px"Check No.")))

(module+ test ;; error checking
  (check-exn #px"\\(amount" (λ () (deposit (struct-copy account Achk) 2day "100" "666" 100purpose))))

(module+ test ;; testing basic comment property
  (check-equal? (string-length (create-check Achk (make-string COMMENT-MAX #\space))) COMMENT-MAX "t")

  (check-equal? (string-length (create-check Achk " Caludia")) COMMENT-MAX)
  (check-equal? (string-length (create-check A+100chk " Caludia")) COMMENT-MAX)
  (check-equal? (string-length (create-check A+100-50chk " Caludia")) COMMENT-MAX))

(module+ test ;; tests for balance
  (check-equal? (show-balance Achk 2day) 0)
  (check-equal? (show-balance A+100chk 2day) 100)
  (check-equal? (show-balance A+100+25chk 2day) 75))
