#lang racket

;; the actions that can be performed on an account

;; the result plus action setup makes unit testing convenient and effective
;; but is useless for the actual script. 
(define (action/c x/c)
  (list/c x/c (-> any)))

(provide
 (rename-out [to-html to-html/t])

 (contract-out
  [name->path
   (->* (string?) (#:kind string?) path?)]

  [write 
   (-> account? (-> void?))]

  [update
   (-> account? my-date? account?)]

  [new-account/2
   (-> procedure? string? (action/c account?))]
  [deposit
   ;; consumes command-line arguments to deposit amount 
   (->* (account? my-date? string?) () #:rest (listof string?) (action/c account?))]
  [withdraw
   ;; consumes commad-line arguments to withdraw amount
   (->* (account? my-date? string?) () #:rest (listof string?) (action/c account?))]
  [write-check
   ;; consumes commad-line arguments to record a check at amount
   (->* (account? my-date? string? string?) () #:rest (listof string?) (action/c account?))]
  [show-balance
   (->* (account? my-date?) () #:rest any/c (action/c amount?))]
  [to-html
   (->* (account? my-date?) () #:rest any/c (action/c any/c))]))

;; ---------------------------------------------------------------------------------------------------
(require Xmanage/private/date)
(require Xmanage/private/data)
(require Xmanage/private/file-io)
(require racket/control)
(require (only-in xml xexpr->xml display-xml/content))

(module+ test
  (require (submod ".."))
  (require (submod Xmanage/private/file-io examples))
  (require rackunit))

;; ---------------------------------------------------------------------------------------------------
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

(define (name->path name #:kind (proper-or-html #false))
  (define file-name (if proper-or-html (~a name "." proper-or-html) (~a "." name ".act")))
  (build-path HOME "Private" "Accounts" "Xmanage" file-name))

(define [(write account)]
  (define file-name (name->path (account-name account)))
  (with-output-to-file file-name #:exists 'replace (λ () (account-writer account))))

(module+ test
  (check-equal? (name->path "name") (build-path HOME "Private" "Accounts" "Xmanage" ".name.act"))
  (check-equal? (name->path "n" #:kind "h") (build-path HOME "Private" "Accounts" "Xmanage" "n.h"))
  
  (let ([file-name (name->path (account-name checking))])
    (dynamic-wind
     void
     (λ () (check-true (void? [(write checking)]) "dumb check"))
     (λ () (delete-file file-name)))))


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

(define ALREADY-EXISTS "An account with this name already exists.\n")

#; {(Natural -> α)  String -> Void}
;; the α is Tim Griffin's continuation trick (argh)
(define (new-account/2 exit-for-testing name #:last-check (last 0))
  (define a (account name '() 0 (today) '() last))
  (define (effect)
    (define file-name (name->path name))
    (when (file-exists? file-name)
      (printf ALREADY-EXISTS)
      (exit-for-testing 1))
    (with-output-to-file file-name (λ () (account-writer a))))
  (list a effect))

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
(define ((make-d/w how) account date  a . msg)
  (define amount
    (match a
      [(argv-amount> b) b]
      [_
       (error 'make-d/w "amount expected, found ~a" a)]))
  (define history (account-recents account))
  (define delta   (how 0 amount))
  (define action  (dw (align (string-join msg)) date delta))
  (set-account-recents! account (cons action history))
  (list account (write account)))

#; (Account Date Amount String -> Void)
;; effect: record a deposit
(define deposit (make-d/w +))

#; (Account Date Amount String -> Void)
;; effect: record a non-check withdrawl
(define withdraw (make-d/w -))

#; (Account Date Amount String ... -> Void)
;; EFFECT confirm check writing action with a print to console 
(define (write-check account date amount . purpose)
  (define the-check (create-check account (string-join purpose)))
  (match-define (list account+ do) (withdraw account date amount the-check))
  (define (ackn) (printf "Check No. ~a Today: ~a~n" (account-check-no account) (today)))
  (list account+ (λ () (do) (ackn))))

#; [Account String -> String]
;; create entry for a check in `acc` 
;; EFFECT increase check number in account 
(define (create-check acc purpose)
  (define x (+ (account-check-no acc) 1))
  (set-account-check-no! acc x)
  (~a x " " purpose))

;; ---------------------------------------------------------------------------------------------------

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
  (list b (λ () (printf "Current balance: ~a\n" (amount->decimal-string b)))))

;                                            
;                     ;                      
;                     ;           ;          
;                     ;           ;          
;   ;   ;  ;;;;    ;;;;  ;;;;   ;;;;;   ;;;  
;   ;   ;  ;; ;;  ;; ;;      ;    ;    ;;  ; 
;   ;   ;  ;   ;  ;   ;      ;    ;    ;   ;;
;   ;   ;  ;   ;  ;   ;   ;;;;    ;    ;;;;;;
;   ;   ;  ;   ;  ;   ;  ;   ;    ;    ;     
;   ;   ;  ;; ;;  ;; ;;  ;   ;    ;    ;     
;    ;;;;  ;;;;    ;;;;   ;;;;    ;;;   ;;;; 
;          ;                                 
;          ;                                 
;          ;                                 

(define (update account action-date)
  (define last-date (account-balance-date account))
  (when (next-month last-date action-date)
    (define-values (balance history)
      (for/fold ([balance (account-balance account)]
                 [history (account-history account)])
                ([r (account-recents account)])
        (values (+ (dw-amount r) balance)
                (cons r history))))
    (set-account-recents! account '())
    (set-account-history! account history)
    (set-account-balance-date! account action-date)
    (set-account-balance! account balance))
  account)

(module+ test
  (let () ;; test update 
    (define Acopied  (struct-copy account A+100chk))
    (define nextdate '(2026 8 20))
    (define exp (account "checking" '() 100  nextdate (account-recents A+100chk) 0))
    (check-equal? (update Acopied nextdate) exp "update: check return")
    (check-equal? Acopied exp "update: check effect")))


  
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

#; {Account Date -> Void}
(define (to-html a my-dste #:open (open "open") . _other)
  (define page:xml (xexpr->xml (account-to-html a)))
  (define file     (name->path (account-name a) #:kind 'html))
  (list '_
        (λ ()
          (with-output-to-file file #:exists 'replace (lambda () (display-xml/content page:xml)))
          (system (~a open " " (path->string file)))
          (sleep 1))))

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
  
  (assemble-page title date (amount->decimal-string b) recent:xexpr history:xexpr))

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
    (define x       (amount->decimal-string (abs amount)))
    
    (define check? (or (regexp-match #px"^ (\\d+) " comment) (regexp-match #px"^\\*(\\d+) " comment)))
    
    (html-row `([style ,(string-append "background-color:" (if (odd? i) "lightblue" "white"))]) 'td
              #:date       (td-left (date-to-html (dw-date dw)))
              #:check      (auto-or-check-to-html check? amount)
              #:purpose    (td-left (purpose-to-html check? comment))
              #:withdrawal (td-right (if (>= amount 0) "" x))
              #:deposit    (td-right (if (> amount 0)  x ""))
              #:total      (td-right (amount->decimal-string (second t)))
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
                     ,@(assemble-separators (date-to-html (today)) "0.00")))))))

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

(module+ test 
  #; {String -> Account}
  ;; EFFECT run the effect of making a new account 
  (define (run-new-account/2 name)
    (prompt 
     (match-define (list a do) (new-account/2 (λ (x) (control k x)) name))
     (do)
     a))

  #; {Procedure Any ... -> Any}
  (define (run f . others)
    (match-define (list r _) (apply f others))
    r)

  #; {(Account #:file-to-be-created Any ... -> [List Any (-> Void)])}
  (define (run-for-effect f  a #:file-to-be-created (fn (name->path (account-name a))) . others)
    (match-define (list _ do) (apply f a others))
    (dynamic-wind
     (λ ()
       (delete-directory/files fn #:must-exist? #false))
     (λ ()
       (begin0
         (with-output-to-string do)
         (check-true (file-exists? fn))))
     (λ ()
       (sleep .1)
       (delete-file fn)))))

;; ---------------------------------------------------------------------------------------------------
(module+ test ;; new account
  (define TTT "ttt")

  (dynamic-wind
   (λ ()
     (define a (run-new-account/2 TTT))
     (check-equal? (run show-balance a (today)) 0)
     (check-equal? (with-output-to-string (λ () [(second (show-balance a (today)))]))
                   "Current balance: 0.00\n"))

   (λ ()
     ;; re-create to trigger failure
     (check-match (with-output-to-string (λ () (check-equal? (run-new-account/2 TTT) 1)))
                  (pregexp ALREADY-EXISTS)))
   
   (λ ()
     (delete-file (name->path TTT)))))
;; ---------------------------------------------------------------------------------------------------
(module+ test ;; testing deposits, withdrawals, and check writing
  (define 2day '(2026 6 6))

  (check-equal? (run deposit (struct-copy account Achk) 2day "100" 100purpose) A+100chk)
  (check-equal? (run withdraw (struct-copy account A+100chk) 2day "50" 50purpose) A+100-50chk)
  (check-equal? (run write-check (struct-copy account A+100chk) 2day "25" 25purpose) A+100+25chk))

;; ---------------------------------------------------------------------------------------------------
(module+ test ;; check that it creates the account file and prints confirmation message to STDOUT 
  (check-match (run-for-effect write-check (struct-copy account A+100chk) 2day "25" 25purpose)
               (pregexp "Check No.")))
;; ---------------------------------------------------------------------------------------------------
(module+ test ;; check that it creates the HTML file 
  (define a (struct-copy account A+100chk))
  (define f (name->path (account-name a) #:kind "html"))
  (check-equal? (run-for-effect (λ x (apply to-html/t #:open "ls" x)) a 2day #:file-to-be-created f)
                (~a (path->string f) "\n")))

;; ---------------------------------------------------------------------------------------------------
(module+ test ;; error checking
  (check-exn #px"amount expected"
             (λ () (run deposit (struct-copy account Achk) 2day "1h" "6" 100purpose))))

;; ---------------------------------------------------------------------------------------------------
(module+ test ;; tests for balance
  (check-equal? (run show-balance Achk 2day) 0)
  (check-equal? (run show-balance A+100chk 2day) 100)
  (check-equal? (run show-balance A+100+25chk 2day) 75))
