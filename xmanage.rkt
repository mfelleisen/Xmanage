#! /bin/sh
#|
exec racket -tm "$0" -- ${1+"$@"}
|#
#lang racket

(provide main)

(require "date.rkt")
(require "data.rkt")
(require "file-io.rkt")
(require "actions.rkt")
(require (lib "decimals.ss" "utils"))

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

(define USAGE "Usage:")

(define START
  (list #; [List String String ((Natural -> α) Any ...) -> Any]
        (list "-help" "to see this message"
              (λ _ (list '_ (λ () (help-msg #:header USAGE ALL)))))
        (list "-new"  "<name> for a new account named <name>" new-account/2)))

(define MAN
  (list #; [List String String (Account -> Any)]
        (list "-h" "to see this account-specific help message" (λ _ (help-msg MAN)))
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
     (help-msg ALL)
     (exit-for-testing 0)]))

#; {∀α.[Listof [List String String α]] -> String -> α}
(define ((action> cmd-list) a)
  (define r (assoc a cmd-list))
  (and r (third r)))

#; {(Natural -> α) [(Natural -> α) #:rest ANY -> ANY] [Listof Any] -> Any}
(define (generic-command exit-for-testing action other)
  (match-define (list _ do) (apply action exit-for-testing other))
  (do))

#; {(Natural -> α) [Account Date #:rest ANY -> ANY] String [Listof Any] -> Any}
(define (account-specific-command exit-for-testing action name other)
  (with-handlers ([exn? (λ (xn) xn)])
    (define file-name (does-account-already-exist exit-for-testing name))
    (define account (with-input-from-file file-name account-reader))
    (match-define (list r optional-action) (apply action account (today) other))
    (optional-action)
    r))

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
  (dynamic-wind ;; an integrated unit test
   (λ ()
     ;; create new account, check that it exists and works
     (main "-new" "ttt")
     (check-equal? (main "ttt" "-b") 0))

   (λ ()
     ;; re-create to trigger failure
     (check-match (with-output-to-string
                    (λ ()
                      (check-equal? (prompt (main "-new" "ttt" #:exit (λ (x) (control k x)))) 1)))
                  (pregexp "already exists")))
   (λ ()
     (delete-file ".ttt.act")))
  
  (check-match (with-output-to-string (λ () (main "-help")))
               (pregexp USAGE))

  (check-match (with-output-to-string ;; non-existing account, valid command 
                 (λ ()
                   (check-equal? (prompt (main "non-existing" "-b" #:exit (λ (x) (control k x)))) 1)))
               (pregexp "does not exist"))

  (check-match (with-output-to-string ;; existing account, invalid command 
                 (λ ()
                   (check-equal? (prompt (main "check" "-notcom" #:exit (λ (x) (control k x)))) 1)))
               (pregexp "manage"))

  (check-match (with-output-to-string ;; non-existing account, invalid comand 
                 (λ ()
                   (check-equal? (prompt (main "ttt" "-notcom" #:exit (λ (x) (control k x)))) 1)))
               (pregexp "does not exist"))


  (check-match (with-output-to-string ;; ill-formed cmd line 
                 (λ ()
                   (check-equal? (prompt (main #:exit (λ (x) (control k x)))) 0)))
               (pregexp "manage"))
  
  (check-match (with-output-to-string ;; account-specific help 
                 (λ ()
                   (main "check" "-h")))
               (pregexp "manage")))

(require SwDev/Debugging/spy)