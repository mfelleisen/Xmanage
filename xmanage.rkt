#! /bin/sh
#|
exec racket -tm "$0" -- ${1+"$@"}
|#
#lang racket

;; main for managing checking accounts 

(provide main)

(require Xmanage/private/date)
(require Xmanage/private/file-io)
(require Xmanage/private/actions)

(module+ test
  (require racket/control)
  (require rackunit))

;; TODO:
;; -- `-new` should be able to consume the first check number 

;                                     
;                                     
;   ;    ;                            
;   ;    ;                            
;   ;    ;  ;;;   ;;;;    ;;;;   ;;;  
;   ;    ; ;   ;      ;  ;;  ;  ;;  ; 
;   ;    ; ;          ;  ;   ;  ;   ;;
;   ;    ;  ;;;    ;;;;  ;   ;  ;;;;;;
;   ;    ;     ;  ;   ;  ;   ;  ;     
;   ;    ; ;   ;  ;   ;  ;; ;;  ;     
;    ;;;;   ;;;    ;;;;   ;;;;   ;;;; 
;                            ;        
;                         ;  ;        
;                          ;;         

(define USAGE "manage")

(define START
  (list #; [List String String ((Natural -> α) Any ...) -> Any]
        (list "-help" "to see this message"
              (λ _ (list '_ (λ () (help-msg #:header USAGE ALL)))))
        (list "-new"  "<name> for a new account named <name>" new-account/2)))

(define MAN
  (list #; [List String String (Account -> Any)]
        (list "-h" "to see this account-specific help message" (λ _ (list '_ (λ _ (help-msg MAN)))))
        (list "-d" "<num> <coment> for deposits"               deposit)
        (list "-w" "<num> <comment> for withdrawals"           withdraw)
        (list "-c" "<num> <comment> for checks"                write-check)
        (list "-b" "for balance"                               show-balance)
        (list "-a" "for an HTML rendering, like a spreadsheet" to-html)))

(define ALL (append START MAN))

(define PAD (make-string (string-length USAGE) #\space))

(define (help-msg los #:header (h #false))
  (when h
    (printf "~a manage ~a ~a\n" USAGE (first (first los)) (second (first los))))
  (for ([l (if (not h) los (rest los))])
    (printf "~a manage <name> ~a ~a\n" PAD (first l) (second l))))

(define NOT-EXISTS "manage: The account does not exist.")

(define ALREADY-EXISTS "An account with this name already exists.\n")

;                              
;                              
;                    ;         
;                              
;  ;;;;;;  ;;;;    ;;;   ; ;;  
;  ;  ;  ;     ;     ;   ;;  ; 
;  ;  ;  ;     ;     ;   ;   ; 
;  ;  ;  ;  ;;;;     ;   ;   ; 
;  ;  ;  ; ;   ;     ;   ;   ; 
;  ;  ;  ; ;   ;     ;   ;   ; 
;  ;  ;  ;  ;;;;   ;;;;; ;   ; 
;                              
;                              
;                              

(define (main #:exit (exit-for-testing exit) . argv)
  (match argv
    [(list* (app (action> START) (and (? procedure?) action)) other)
     (generic-command exit-for-testing action other)]
    [(list* name (app (action> MAN) (and (? procedure?) action)) other)
     (account-specific-command exit-for-testing action name other)]
    [(list* name any other)
     (ill-formed-command exit-for-testing name any other)]
    [_
     (help-msg ALL)])
  (exit-for-testing 0))

#; {∀α.[Listof [List String String α]] -> String -> α}
(define ((action> cmd-list) a)
  (define r (assoc a cmd-list))
  (and r (third r)))

;; ---------------------------------------------------------------------------------------------------
#; {(Natural -> α) [(Natural -> α) #:rest ANY -> ANY] [Listof Any] -> Any}
(define (generic-command exit-for-testing action other)
  (with-handlers ([exn? (general void exit-for-testing)])
    (match-define (list _ do) (apply action exit-for-testing other))
    (do)))

#; {(Natural -> α) [Account Date #:rest ANY -> ANY] String [Listof Any] -> Any}
(define (account-specific-command exit-for-testing action name other)
  (define account #false)
  (with-handlers ([exn? (general (λ () (when account [(write account)])) exit-for-testing)])
    (define file-name (does-account-already-exist exit-for-testing name))
    (set! account     (with-input-from-file file-name account-reader))
    (define 2day      (today))
    (set! account     (update account 2day))
    (define do        (second (apply action account 2day other)))
    (with-handlers ([exn:fail:filesystem?
		     (λ (xn) (eprintf "writing modified account failed\n ~a\n" (exn-message xn)) 1)])
      (do))))

#; {(-> Account) (Natural -> α) -> Exn:Fail -> α}
(define ((general restore! exit) xn)
  (printf "~a\n" (exn-message xn))
  (with-handlers ([exn:fail:filesystem? ;; I do not know how to test this failure 
                   (λ (xn) (eprintf "restoring account failed: ~a\n" (exn-message xn)))])
    (restore!))
  (exit 1))

#; {(Natural -> α) String Any [Listof Any] -> α}
(define (ill-formed-command exit-for-testing name any other)
  (does-account-already-exist exit-for-testing name)
  (help-msg MAN)
  (exit-for-testing 1))

#; {(Natural -> α) String -> PathString}
(define (does-account-already-exist exit-for-testing name)
  (define file-name (name->path name))
  (unless (file-exists? file-name)
    (println NOT-EXISTS)
    (exit-for-testing 1))
  file-name)

;; ---------------------------------------------------------------------------------------------------
(module+ test ;; testing handlers

  #; {Any ... -> Void}
  ;; raise generic fail exn
  (define (bad-action . _)
    (raise (make-exn:fail "ouch" (current-continuation-marks))))

  #; {Any ... -> Void}
  ;; raise filesystem exn 
  (define (bad-file-action . _)
    (list '_
          (λ ()
            (raise (make-exn:fail:filesystem "ouch" (current-continuation-marks))))))

  #; {[(Natural -> α) Any ... -> α] Any ... -> Void}
  ;; check that command fails and returns 1, plus prints a string containing "ouch"
  (define (check-handler command . other)
    (check-match 
     (with-output-to-string
       (λ ()
         (parameterize ([current-error-port (current-output-port)])
           (define rest-args (append other '((others))))
           (check-equal? (prompt (apply command (λ (x) (control k x)) rest-args)) 1))))
     (pregexp "ouch")))

  (check-handler account-specific-command bad-action "check")
  (check-handler account-specific-command bad-file-action "check")
  (check-handler generic-command bad-action))

;                                                   
;                                                   
;     ;                    ;       ;                
;     ;                    ;                        
;   ;;;;;   ;;;    ;;;   ;;;;;   ;;;   ; ;;    ;;;; 
;     ;    ;;  ;  ;   ;    ;       ;   ;;  ;  ;;  ; 
;     ;    ;   ;; ;        ;       ;   ;   ;  ;   ; 
;     ;    ;;;;;;  ;;;     ;       ;   ;   ;  ;   ; 
;     ;    ;          ;    ;       ;   ;   ;  ;   ; 
;     ;    ;      ;   ;    ;       ;   ;   ;  ;; ;; 
;     ;;;   ;;;;   ;;;     ;;;   ;;;;; ;   ;   ;;;; 
;                                                 ; 
;                                              ;  ; 
;                                               ;;  

(module+ test
  #; {String Any String Any ... -> Void}
  (define (check-main purpose expected expected-msg . others)
    ; (eprintf "testing ~a\n" purpose)
    (check-match
     (with-output-to-string
       (λ ()
         (check-equal? (prompt (apply main others  #:exit (λ (x) (control k x)))) expected purpose)))
     (pregexp expected-msg)))

  (define TTT "ttt")

  ;; -------------------------------------------------------------------------------------------------
  (dynamic-wind ;; an integrated unit test: create same account twice
   (λ ()
     ;; create new account, check that it exists and works
     (check-main "make TTT" 0 "" "-new" TTT)
     (check-main "TTT has balance 0" 0 "0.00" TTT "-b"))

   (λ ()
     ;; re-create to trigger failure
     (check-main "don't create account again" 1  "already exists"  "-new" TTT))
   (λ ()
     (delete-file (~a "." TTT ".act"))))

  ;; -------------------------------------------------------------------------------------------------
  (dynamic-wind ;; an integrated unit test: write a check for 0.00 
   (λ ()
     ;; create new account, check that it exists and works
     (check-main "make TTT" 0 "" "-new" TTT))
   (λ ()
     ;; re-create to trigger failure
     (check-main "bad check/withdrawal amount" 1 "amount expected" TTT "-c" "0.00" "void"))
   (λ ()
     (delete-file (~a "." TTT ".act"))))

  ;; -------------------------------------------------------------------------------------------------
  (dynamic-wind ;; an integrated unit test: write a check without purpose statement
   (λ ()
     ;; create new account, check that it exists and works
     (check-main "make TTT" 0 "" "-new" TTT))
   (λ ()
     ;; re-create to trigger failure
     (check-main "bad check/withdrawal amount" 1 "amount expected" TTT "-c" "0.00"))
   (λ ()
     (delete-file (~a "." TTT ".act"))))
  
  ;; -------------------------------------------------------------------------------------------------
  ;; at this point ".ttt.act" does not exist

  (check-main "generic help" 0 USAGE "-help")
  (check-main "balance of non existing account" 1 NOT-EXISTS "non-existing" "-b")
  (check-main "bad command for existing account" 1 USAGE "check" "-notcom")
  (check-main "bad command for non-existing account" 1 NOT-EXISTS "ttt" "-notcom")
  (check-main "ill-formed command line" 0 USAGE "")
  (check-main "account-specific help" 0 USAGE "check" "-h"))
