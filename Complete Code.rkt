#lang racket
;; Name:Mansoor Saeed
;; Student Id:001396070
;; Date of birth (day/month/year): 18/02/2006

;; Data format: Name, Mother, Father, Date of birth, Date of death.
;; An empty list means Unknown.

;; Safe list access helper
(define (safe-ref lst idx default)
(if (and (list? lst) (> (length lst) idx))
    (list-ref lst idx)
    default))
  (define family-tree
  '(("Alice" "Mary" "John")
    ("Bob" "Alice" "Mark")
    ("Charlie" "Alice" "Mark")
    ("David" "Jane" "Tom")
    ("Eve" "Mary" "James")))

;; Maternal branch
(define Mb
'(((Mary Blake) ((Ana Ali) (Theo Blake)) ((17 9 2022) ()))
  ((Ana Ali) ((Ada West) (Md Ali)) ((4 10 1995) ()))
  ((Theo Blake) ((Mary Jones) (Tom Blake)) ((9 5 1997) ()))
  ((Greta Blake) ((Mary Jones) (Tom Blake)) ((16 3 1999)))
  ((Ned Bloom) (() ()) ((23 4 2001) ()))
  ((John Bloom) ((Greta Blake) (Ned Bloom)) ((5 12 2023) ()))))

;; Paternal branch
(define Pb
'(((John Smith) ((Jane Doe) (Fred Smith)) ((1 12 1956) (3 3 2021)))
  ((Ana Smith) ((Jane Doe) (Fred Smith)) ((6 10 1958) ()))
  ((Jane Doe) ((Eve Talis) (John Doe)) ((2 6 1930) (4 12 1992)))
  ((Fred Smith) ((Lisa Brown) (Tom Smith)) ((17 2 1928) (13 9 2016)))
  ((Eve Talis) (() ()) ((15 5 1900) (19 7 1978)))
  ((John Doe) (() ()) ((18 2 1899) (7 7 1970)))
  ((Lisa Brown) (() ()) ((31 6 1904) (6 3 1980)))
  ((Tom Smith) (() ()) ((2 8 1897) (26 11 1987)))
  ((Alan Doe) ((Eve Talis) (John Doe)) ((8 9 1932) (23 12 2000)))
  ((Mary Doe) (() (Alan Doe)) ((14 1964) ()))))



;; Define lst-mb, lst-pb, lst-all
(define (lst-mb mb) mb)
(define (lst-pb pb) pb)
(define (append-lst list1 list2)
(if (null? list1) list2
    (cons (car list1) (append-lst (cdr list1) list2))))
(define (lst-all mb pb)
(append-lst mb pb))
(lst-all Mb Pb)
(define (normalize-date date)
(cond
  [(null? date) '(1 1 1900)]  ;; Default date if missing
  [(= (length date) 2) (list (car date) 1 (cadr date))] ;; Assume missing month = 1 (January)
  [else
   (let* ([day (car date)]
          [month (cadr date)]
          [year (caddr date)]
          [valid-month (if (or (< month 1) (> month 12)) 1 month)]
          [max-day (case valid-month
                     [(1 3 5 7 8 10 12) 31]
                     [(4 6 9 11) 30]
                     [(2) (if (leap-year? year) 29 28)]
                     [else 30])]
          [valid-day (if (> day max-day) max-day day)])
     (list valid-day valid-month year))]))

(define (leap-year? year)
(or (and (zero? (modulo year 4)) (not (zero? (modulo year 100))))
    (zero? (modulo year 400))))



;; A1: Extract parents
(define (parents lst)
(map (lambda (x) (safe-ref x 1 '())) lst))
(displayln "Parents:") 
(displayln (parents (lst-all Mb Pb)))

;; A2: Living members (only from maternal branch)
(define (living-members lst)
  (filter (lambda (ind)
            (let ((dates (safe-ref ind 2 '())))
              (or (null? dates) (null? (safe-ref dates 1 '()))))) ;; Ensures only living members (no death date)
          Mb)) ;; Ensures filtering only living members from Mb

;; (displayln "Living Members:") 
;; (displayln (living-members Mb)) ;; Adjusted to only consider Mb

;; A3: Compute current age
(define (current-age lst)
(map (lambda (ind)
       (let ((birth (safe-ref (safe-ref ind 2 '()) 0 '())))
         (if (and (list? birth) (>= (length birth) 3))
             (- 2025 (safe-ref birth 2 0))
             'Unknown)))
     lst))

    ;;(displayln "Current Age:") 
    ;;(displayln (current-age (lst-all Mb Pb)))


;; A4: Filter individuals born in a specific month
(define (same-birthday-month lst month)
(filter (lambda (x)
          (let ((birth (safe-ref (safe-ref x 2 '()) 0 '())))
            (and (list? birth) (>= (length birth) 2) (= (safe-ref birth 1 0) month))))
        lst))

;; A5: Sort by last name
;; Function to extract last name from a person's record
(define (get-last-name person)
(if (and (pair? person) (pair? (car person)))
    (cadr (car person))  ;; Extract last name
    'zzz))  ;; Default value to avoid errors

;; Sort the list of people by last name
(define (sort-by-last people)
(sort people 
      (lambda (p1 p2)
        (string<? (symbol->string (get-last-name p1))
                  (symbol->string (get-last-name p2))))))

;; Return a list of last names sorted alphabetically
(define (sorted-last-names people)
(map (lambda (person) (symbol->string (get-last-name person))) 
     (sort-by-last people)))


;; A6 Function to change a person's name
(define (change-name family-tree old-name new-name)
(map (lambda (entry)
       (let ([name (car entry)]
             [parents (cadr entry)]
             [dates (caddr entry)])
         (list (if (equal? name old-name) new-name name)  ;; Change name if it matches
               (list (if (equal? (car parents) old-name) new-name (car parents))  ;; Change mother if it matches
                     (if (equal? (cadr parents) old-name) new-name (cadr parents)))  ;; Change father if it matches
               dates)))
     family-tree))





;; B1 - Function to get all children from both branches when called
(define (find-children lst parent)
  (cond
    ((null? lst) '()) 
    ((equal? (cadr (car lst)) parent) (cons (car (car lst)) (find-children (cdr lst) parent)))
    (else (find-children (cdr lst) parent))))

(define (all-children lst all-members)
  (if (null? all-members)
      '()
      (cons (cons (car all-members) (find-children lst (list (car all-members))))
            (all-children lst (cdr all-members)))))

(define (children)
  (append (all-children Mb (map car Mb)) (all-children Pb (map car Pb))))


;; B2 - Function to Find the Oldest Living Member from Both Branches
(define (oldest-living-member)
(letrec ([older? 
          (lambda (date1 date2)
            (if (or (null? date1) (null? date2))
                #f  ;; If either date is missing, do not compare
                (let ([year1 (if (and (pair? date1) (>= (length date1) 3)) (list-ref date1 2) 9999)]
                      [year2 (if (and (pair? date2) (>= (length date2) 3)) (list-ref date2 2) 9999)]
                      [month1 (if (and (pair? date1) (>= (length date1) 2)) (list-ref date1 1) 12)]
                      [month2 (if (and (pair? date2) (>= (length date2) 2)) (list-ref date2 1) 12)]
                      [day1 (if (and (pair? date1) (>= (length date1) 1)) (list-ref date1 0) 31)]
                      [day2 (if (and (pair? date2) (>= (length date2) 1)) (list-ref date2 0) 31)])
                  (or (< year1 year2)
                      (and (= year1 year2) (< month1 month2))
                      (and (= year1 year2) (= month1 month2) (< day1 day2))))))]
         
         [oldest-helper 
          (lambda (lst oldest-person oldest-birthdate)
            (if (null? lst)
                oldest-person
                (let* ([person (car lst)]
                       [name (car person)]
                       [raw-birthdate (if (and (pair? person) (>= (length person) 3)) (list-ref person 2) '())]
                       [birthdate (if (pair? raw-birthdate) (car raw-birthdate) raw-birthdate)]  ;; Extract first element if nested
                       [raw-deathdate (if (and (pair? person) (>= (length person) 4)) (list-ref person 3) '())]
                       [deathdate (if (pair? raw-deathdate) (car raw-deathdate) raw-deathdate)]) ;; Extract first element if nested

          

                  (if (and (not (null? birthdate)) (null? deathdate)) ;; Only consider living people with valid birthdate
                      (if (or (null? oldest-birthdate) (older? birthdate oldest-birthdate))
                          (oldest-helper (cdr lst) name birthdate)
                          (oldest-helper (cdr lst) oldest-person oldest-birthdate))
                      (oldest-helper (cdr lst) oldest-person oldest-birthdate)))))])
  
  ;; Start recursion with an empty oldest-person and birthdate
  (oldest-helper (append Mb Pb) '() '())))






;; B3 - Average age of death
(define (extract-birth-death member)
(let* ((dates (caddr member))
       (birthdate (if (and (list? dates) (>= (length dates) 1)) (car dates) #f))
       (deathdate (if (and (list? dates) (>= (length dates) 2)) (cadr dates) #f)))
  (list birthdate deathdate)))

(define (calculate-age birth death)
(if (and (list? birth) (list? death)
         (>= (length birth) 3) (>= (length death) 3))
    (- (caddr death) (caddr birth))  ; Extract year and compute age
    #f))

(define (get-ages family-tree)
(filter number?
  (map (lambda (member)
         (let* ((dates (extract-birth-death member))
                (birthdate (car dates))
                (deathdate (cadr dates)))
           (calculate-age birthdate deathdate)))
       family-tree)))

(define (average-age-on-death)
(let* ((ages (append (get-ages Mb) (get-ages Pb)))
       (valid-ages (filter number? ages)))
  (if (null? valid-ages)
      'No-data
      (/ (apply + valid-ages) (* 1.0 (length valid-ages))))))  ; Ensure floating-point division


;; B4 - Birthdays in February
(define (birthday-month-same family-tree)
  (map car  ; Extract only the name of the person
    (filter (lambda (member)
              (let* ((dates (caddr member)) ; Extract birth and possibly death date
                     (birthdate (if (list? dates) (car dates) '()))) ; Get only birthdate
                (and (list? birthdate)  ; Ensure birthdate is a list
                     (>= (length birthdate) 2) ; Ensure birthdate has at least (day month)
                     (= (cadr birthdate) 2)))) ; Check if the month is February (2)
            family-tree)))

;; B5 - Function to get all members by first names
;; Function to get all members by first names
(define (first-person member)
  (symbol->string (car (car member))))  ;; Convert symbol to string

;; Function to Sort Members by First Name (Only Paternal Branch)
(define (sort-by-first)
  (map first-person
    (sort Pb
      (lambda (p1 p2)
        (string-ci<? (first-person p1) (first-person p2))))))

;; B6 - Change name "Mary" to "Maria"
;; Function to retrieve maternal and paternal lineage separately
(define (find-lineage name branch)
  (filter (lambda (entry) (equal? (car (car entry)) name)) branch))

;; Function to change "Mary Doe" to "Maria Doe" in the family tree
(define (change-name-to-Maria branch)
  (map (lambda (entry)
         (if (equal? (car entry) '(Mary Doe))  ; Ensure it's a single symbol
             (cons '(Maria Doe) (cdr entry))
             entry))
       branch))

(define (display-lineage name)
  (let ((maternal (find-lineage name (change-name-to-Maria Mb)))
        (paternal (find-lineage name (change-name-to-Maria Pb))))
    (displayln "Maternal Lineage:")
    (for-each (lambda (entry)
                (displayln (string-append "Child: " (symbol->string (caar entry))))
                (displayln (string-append "Mother: " (if (null? (caadr entry)) "Unknown" (symbol->string (caaar (caadr entry))))))
                (displayln (string-append "Father: " (if (null? (cadadr entry)) "Unknown" (symbol->string (caadr (caadr entry))))))
                (displayln "---"))
              maternal)
    (displayln "\nPaternal Lineage:")
    (for-each (lambda (entry)
                (displayln (string-append "Child: " (symbol->string (caar entry))))
                (displayln (string-append "Mother: " (if (null? (caadr entry)) "Unknown" (symbol->string (caaar (caadr entry))))))
                (displayln (string-append "Father: " (if (null? (cadadr entry)) "Unknown" (symbol->string (caadr (caadr entry))))))
                (displayln "---"))
              paternal)))

;; Example usage
(display-lineage "B6")
;; C1 - Function to extract the maternal branch
(define (maternal-branch family-tree mother-name)
(filter (lambda (x) 
          (and (pair? (cadr x))  ; Ensure parents list exists
               (pair? (car (cadr x)))  ; Ensure mother list exists
               (equal? (caadr x) mother-name))) ; Compare with mother name
        family-tree))
;; C2 - Function to extract the paternal branch
(define (paternal-branch family-tree father-name)
(filter (lambda (x) 
          (and (pair? (cadr x))  ; Ensure parents list exists
               (pair? (cdr (cadr x)))  ; Ensure father list exists
               (equal? (cadadr x) father-name))) ; Compare with father name
        family-tree))
;; C3 -  Function to extract both branches
(define (both-branches family-tree mother-name)
(append (maternal-branch family-tree mother-name)
        (paternal-branch family-tree mother-name)))
      
  
  (displayln "Maternal Branch:")
  (displayln (maternal-branch (append Mb Pb) '(Mary Jones)))  ; ✅ Correct
  
  (displayln "Paternal Branch:")
  (displayln (paternal-branch (append Mb Pb) '(Fred Smith)))  ; 
  
  (displayln "Both Branches:")
  (displayln (both-branches (append Mb Pb) '(Greta Blake)))  ; ✅ Correct

;;
;;You should include code to execute each of your functions below.
  
;; Function to display the menu
;; FUNTION TO HELP SHOW MENU FOR EACH PART


;; Function to display the menu
(define (display-menu)
(displayln "\nSelect an option:")
(displayln "A1: parents")
(displayln "A2: living-members")
(displayln "A3: current-age")
(displayln "A4: same-birthday-month")
(displayln "A5: sort-by-last")
(displayln "A6: change-name-to-Juan")
(displayln "B1: children")
(displayln "B2: oldest-living-member")
(displayln "B3: average-age-on-death")
(displayln "B4: birthday-month-same")
(displayln "B5: sort-by-first")
(displayln "B6: change-name-to-Maria")
(displayln "Press 'esc' or 'q' to exit."))


;; Function to process user input and execute the corresponding option
(define (process-option option)
(cond
  [(equal? option "A1") 
   (displayln "Parents:") 
   (displayln (parents (lst-all Mb Pb)))]
  
  [(equal? option "A2") 
   (displayln "Living Members:") 
   (displayln (living-members (lst-all Mb Pb)))]
  
  [(equal? option "A3") 
   (displayln "Current Age:") 
   (displayln (current-age (lst-all Mb Pb)))]
  
  [(equal? option "A4") 
   (displayln "Enter the birth month (1-12):")
   (let ([month (string->number (string-trim (read-line)))])
     (if (and month (<= 1 month 12))
         (displayln (same-birthday-month (lst-all Mb Pb) month))
         (displayln "Invalid month! Please enter a number between 1 and 12.")))]
  
  [(equal? option "A5") 
   (displayln "Sorted by Last Name:") 
   (display "Sorted Maternal Last Names: ")
  (display (sorted-last-names Mb))
  (newline)
  (display "Sorted Paternal Last Names: ")
  (display (sorted-last-names Pb))]
  
  [(equal? option "A6") 
  (displayln "Enter old Full name:")
  (let ([old-name (map string->symbol (string-split (string-trim (read-line))))]) ; Convert full name to list of symbols
    (displayln "Enter new Full name:")
    (let ([new-name (map string->symbol (string-split (string-trim (read-line))))]) ; Convert full name correctly
      (displayln (change-name (lst-all Mb Pb) old-name new-name))))] 

  
  [(equal? option "B1") 
   (displayln "All Children:") 
   (displayln (children))]
 

  
  [(equal? option "B2") 
   (displayln "Oldest Living Member:") 
   (displayln (oldest-living-member))]
  
  [(equal? option "B3") 
   (displayln "Average Age at Death:") 
   (displayln (average-age-on-death))]
  
  [(equal? option "B4") 
   (displayln "People born in February:") 
   (displayln (birthday-month-same (lst-all Mb Pb)))]
  
  [(equal? option "B5") 
   (displayln "Sorted by First Name:") 
   (displayln (sort-by-first))]
  
[(equal? option "B6")
 (displayln "Updated names:")
 (displayln (change-name-to-Maria Mb)) ; Change names in maternal branch
 (displayln (change-name-to-Maria Pb))] ; Change names in paternal branch


  
  ;; Exit condition
  [(member option '("esc" "q")) 
   (displayln "Exiting program...") 
   (exit)]
  
  ;; Invalid input
  [else (displayln "Invalid option! Please try again.")]))

;; Main loop to keep displaying the menu and processing user input
(define (main-loop)
(display-menu)
(process-option (string-trim (read-line))) ;; Read and process input
(main-loop)) ;; Recursively call main-loop to keep the program running

;; Start the program
(main-loop)
