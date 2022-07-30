;Julie Kye
;Project: N-bit Adder 
;Description: Scheme program to simulate an N-bit Adder using recursive 1-bit Adder
;             procedures. The 1-bit Adder procedures are constructed by logic gate functions
;             to mimick how real N-bit Adders perform addition on binary numbers.

;Functions defining logic gates
(define OR-GATE (lambda (a b)
; if a or b is 1, output it 1                 
                  (if (= a b 0)
                      0
                      1)
                  ))

(define AND-GATE (lambda (a b)
; only if a and b are 1, output is 1                   
                   (if (= a b 1)
                       1
                       0)
                   ))    

(define XOR-GATE (lambda (a b)
;  if a and b are the same, output is 0
                     (if (= a b)
                         0
                         1)
                   ))                     
                 
;TEST LOGIC GATE FUNCTIONS
(display "AND-GATE TRUTH TABLE: ")(newline)
(display "0 0 ")
(AND-GATE 0 0) ;0
(display "0 1 ")
(AND-GATE 0 1) ;0
(display "1 0 ")
(AND-GATE 1 0) ;0
(display "1 1 ")
(AND-GATE 1 1) ;1

(newline)
(display "OR-GATE TRUTH TABLE: ")(newline)
(display "0 0 ")
(OR-GATE 0 0) ;0
(display "0 1 ")
(OR-GATE 0 1) ;1
(display "1 0 ")
(OR-GATE 1 0) ;1
(display "1 1 ")
(OR-GATE 1 1) ;1

(newline)
(display "XOR-GATE TRUTH TABLE: ")(newline)
(display "0 0 ")
(XOR-GATE 0 0) ;0
(display "0 1 ")
(XOR-GATE 0 1) ;1
(display "1 0 ")
(XOR-GATE 1 0) ;1
(display "1 1 ")
(XOR-GATE 1 1) ;0


;1-Bit Adder procedure takes in arguments a, b, and a carry-in
;returns a pair with sum and carry-out '(s . c)
(define bitAdder (lambda (Cin a b) 
                   ; generate pair of s and c
                   (cons (sum-bits Cin a b)(carry-out Cin a b))                   
                   ))
;Function to generate sum-bits for the 1-Bit Adder                 
(define sum-bits (lambda (Cin a b)
                   (XOR-GATE Cin (XOR-GATE a b))
                   ))
;Function to generate the carry-out value for 1-Bit Adder procedure
(define carry-out (lambda (Cin a b)
                   (OR-GATE (AND-GATE Cin (XOR-GATE a b))(AND-GATE a b))
                    ))

;TEST 1-BIT ADDER 
(newline)
(display "FULL ADDER TRUTH TABLE: ")(newline)
(display "INPUTS    OUTPUTS") (newline)
(display "A B C-in  Sum C-out") (newline)
(display "0 0 0     ")(bitAdder 0 0 0)
(display "0 0 1     ")(bitAdder 0 0 1)
(display "0 1 0     ")(bitAdder 0 1 0) 	
(display "0 1 1     ")(bitAdder 0 1 1)	
(display "1 0 0     ")(bitAdder 1 0 0) 	
(display "1 0 1     ")(bitAdder 1 0 1)	
(display "1 1 0     ")(bitAdder 1 1 0) 	
(display "1 1 1     ")(bitAdder 1 1 1) 

;list functions used for binary addition 
;returns last element of lst
(define tail (lambda (lst)
               (cond ((null? (cdr lst))
                   (car lst))
                   (else (tail (cdr lst))))
               ))
               

;returns list without last element
(define rmtail (lambda (lst)
                 (cond ((null? (cdr lst))
                     '())
                    (else (cons (car lst)(rmtail (cdr lst)))))
                 ))


;N-Bit Adder function computes result for parameters list1 and list2 of N-bits
;Calls recursive add with initial carry-in value of 0
(define n-bit-adder (lambda (L1 L2 n)
                  (cond ((or (not (= (length L1) (length L2)))) (display "Error: The numbers entered must have the same number of bits"))
                        (else (newline)(display "ADDITION OF BINARY NUMBERS ")(newline)(display L1)(newline)(display L2)(newline)
                         (display "WITH A ")(display n)(display "-BIT ADDER: ") (newline)
                         (recursiveAdd L1 L2 n 0)
                              )
                        )
                
                      ))
;function for adding each last bit of lists until lists are empty
(define recursiveAdd (lambda (L1 L2 n carry)
                       (cond ((null? L1) 
                              ;print final carryout when lists are empty
                              (list (list carry)))
                             (else
                              (updateSum (cons (car(bitAdder (tail L1) (tail L2) carry))
                                         (recursiveAdd (rmtail L1) ; recursively call on remainder of the lists
                                                       (rmtail L2) n
                                                       ;use the carry-out(cdr)as the next carry-in for the recursive call
                                                       (cdr(bitAdder (tail L1) (tail L2) carry)))))
                              )
                             )
                       ))     

;This function appends the new sum-bit to front of list of previously calculated sum-bits                                
(define updateSum (lambda (lst)
                                (cond ((null? lst)
                                    '())
                                    ;append new sum-bit to rest of bits
                                    (else
                                     (append (cdr lst)(list(car lst))))
                                 )
                                ))


;DEFINE BINARY NUMBERS (AS A LIST) FOR ADDITION HERE:
(define A1 '(0 1 0 ) ) ;5 (decimal value)
(define A2 '(1 0 1 1) ) ;11
(define B1 '(0 1 0 1 1 0 1 1) ) ;91
(define B2 '(1 0 1 1 0 0 1 0) ) ;178
(define C1 '(1 1 0 1 1 0 1 1 1 0 1 1 0 0 1 0) ) ;56242
(define C2 '(1 1 1 0 1 1 0 1 0 0 1 1 1 1 1 0) ) ;60734	
(define D1 '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1) )	;4294967295
(define D2 '(1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0) ) ;2863311530

                                
;N-BIT ADDER TEST CASES USING NUMBERS DEFINED ABOVE
(newline)
(n-bit-adder  A1 A2 4) ;(1)0000 (binary output) ;16 (decimal value)
(n-bit-adder  B1 B2 8) ;(1)00001101 ;269
(n-bit-adder  C1 C2 16) ;(1)1100100011110000 ;116976
(n-bit-adder  D1 D2 32) ;(1)10101010101010101010101010101001 ;7158278825




