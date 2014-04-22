; CS 161 Spring 2014: HW1 test cases from assignment
; hw-1-examples.lsp

(load "hw-1-solution.lsp")

; C1: "Teenager Charlotte Newton feels excited."
(setq C1 '(STATE TYPE (EMOTION SENTIM (POS)
                               SCALE (>NORM)) 
                 AGENT (HUMAN F-NAME (CHARLOTTE)
                              L-NAME (NEWTON)
                              GENDER (FEMALE)
                              AGE (RANGE FROM (13)
                                         TO (19)
                                         UNIT (YEAR)))))

; C2: "Charles gives Charlotte an expensive emerald ring."
(setq C2 '(GIVE AGENT (HUMAN F-NAME (CHARLES)
                             L-NAME ( ) 
                             GENDER (MALE))
                TO (HUMAN F-NAME (CHARLOTTE) 
                          L-NAME ( )
                          GENDER (FEMALE)) 
                OBJECT (RING CONTAINS (EMERALD)
                             COST (>NORM))))

; C3: [Gapped] "At Charlotte's home Charles secretly throws a newspaper 
; story in the trash can but Charlotte sees him."
(setq C3 '(THROW AGENT AG001
                 OBJECT OBJ001 
                 INTO (CONTAINER SHAPE (CYLINDRICAL)
                                 FOR (TRASH)
                                 REF (DEF))
                 MANNER (SECRETLY)
                 EXP-VIOL EXPV001
                 LOC (HOME OWNER (HUMAN F-NAME (CHARLOTTE) 
                                        L-NAME ( )
                                        GENDER (FEMALE)))))


(setq F-NAME001 '(FRANK))

(setq AG001 '(HUMAN F-NAME (CHARLES)
                    L-NAME ( )
                    GENDER (MALE)))

(setq OBJ001 '(PRINTED-MATTER TYPE (NEWSPAPER MATERIAL (PAPER))
                              INFO (STORY)
                              REF (INDEF)))

(setq EXPV001 'CON004)

(setq CON004 '(SEE AGENT AG002 OBJECT OBJ002))

(setq AG002 'CON005)

(setq CON005 '(HUMAN F-NAME (CHARLOTTE)
                     L-NAME ( )
                     GENDER (FEMALE)))

(setq OBJ002 '(HUMAN F-NAME (CHARLES)
                     L-NAME ( )
                     GENDER (MALE)))


; Helper functions to make the main functions more readable
; (cond statement is just to not redefine if solution was included,
; so we can avoid some annoying warning messages)

(cond ((boundp 'HW-1-SOLN-INCLUDED) nil)
(t

    ; FUNCTION: front-slot
    ; PURPOSE:  Return the name of the first SLOT in FRAME
    ; INPUT:    FRAME
    ; OUTPUT:   atom: name of first SLOT
    (defun front-slot (frame)
        (second frame)
    )
 
    ; FUNCTION: front-filler
    ; PURPOSE:  Return the FILLER of the first SLOT in FRAME
    ; INPUT:    FRAME
    ; OUTPUT:   FRAME/GAP: filler of first SLOT in FRAME
    (defun front-filler (frame)
        (third frame)
    )
 
    ; FUNCTION: pop-slot
    ; PURPOSE:  Return a copy of FRAME with its first slot removed
    ; INPUT:    FRAME
    ; OUTPUT:   FRAME (with first slot removed)
    (defun pop-slot (frame)
        (cons (first frame ) (nthcdr 3 frame))
    )
 
    ; Function to retrieve predicate of a FILLER (or just the symbol if it's a symbol)
    (defun f-pred (frame)
        (cond
            ((listp frame) (first frame))
            (t frame)
        )
    )
 
    ; Function to safely check length of FILLER in case it isn't a list
    (defun f-length (frame)
        (cond
            ((listp frame) (length frame))
            (t 1)
        )
    )
)) ; end if solution included

; -----------------------------------------------------------------------------
; Re-including solution functions so that fr-equal can operate as intended,
; independent of your solution

(defun f-FILLER (slot frame)
    (cond
        ; Base case: predicate with no slots (or empty frame)
        ((<= (length frame) 1) nil)
        ; If first slot matches, return its filler. (first (second ... gets the
        ; slot name, while (second (second ... gets its filler
        ((equal slot (front-slot frame)) (front-filler frame))
        ; Else, first slot does not match, so test the rest of the slots
        (t (f-FILLER slot (pop-slot frame)))
    )
)

(defun f-GAPSLOTS (sf)
    (cond
        ; Base case: got through them all
        ((null sf) nil)
        ; Recursive case: dispatch UNGAP on our first filler
        (t (append (append (list (first sf))            ; rebuild our first slot-filler pair
                           (list (f-UNGAP (second sf))))  ; dispatch UNGAP on the filler
                           (f-GAPSLOTS (nthcdr 2 sf))))   ; recurse on rest of sf
    )
)

(defun f-UNGAP (frame)
    (cond
        ; Base case: we got a non-NIL atom, so evaluate it if bound
        ((not (listp frame)) (if (boundp frame) (f-UNGAP (eval frame)) frame))
        ; Base case: empty, or single pred frame
        ((<= (length frame) 1) frame)
        ; Main case: dispatch GAPSLOTS on our slot-filler list
        (t (cons (first frame) (f-GAPSLOTS (rest frame))))
    )
)

(defun FR-EQUAL-COMP (frame1 frame2)
    (cond
        ; Base case: empty frames match
        ((and (null frame1) (null frame2)) t)
        ; Base case: frames with different preds do not match
        ((not (equal (f-pred frame1) (f-pred frame2))) NIL)
        ; Base case: frames of different lengths do not match
        ((not (= (f-length frame1) (f-length frame2))) NIL)
        ; Base case: bare predicates (or matching symbols) match
        ((<= (f-length frame1) 1) t)
        ; Base case: variables match iff they have the same name
        ; [!] Not required for HW1
        ((equal (first frame1) 'V) (equal frame1 frame2))
        
        ; Recursive case: check frame1's front slot, then remove and recurse to
        ; check the rest of the slots.
        (t (let ((front (front-slot frame1))) 
            (and (FR-EQUAL-COMP (f-FILLER front frame1) (f-FILLER front frame2))
                 (FR-EQUAL-COMP (pop-slot frame1) (rm-slot front frame2)))
            )
        )
    )
)

; This is just my version of SAME-SF used for testing so you can see if your
; SAME-SF agrees without having to rename it to avoid a redefinition
(defun FR-EQUAL (frame1 frame2)
    (let ((UG-frame1 (f-UNGAP frame1)) (UG-frame2 (f-UNGAP frame2)))
        (FR-EQUAL-COMP UG-frame1 UG-frame2)
    )
)

; Test function; make sure result is equal to expected, and if not, print expected value
(defun test-case (actual expected case-name)
    (cond
        ((equal actual expected) (format t "~A: success~%" case-name))
        (t (format t "~A: failed~%Expected ~A~%Got ~A~%---------------------------~%" case-name expected actual))
    )
)

; Test function, with equality replaced with frame-equality
(defun test-case-fr (actual expected case-name)
    (cond
        ((not (and (listp actual) (listp expected))) t (format t "~A: failed~%Expected ~A~%Got ~A~%---------------------------~%" case-name expected actual))
        ((fr-equal actual expected) (format t "~A: success~%" case-name))
        (t (format t "~A: failed~%Expected ~A~%Got ~A~%---------------------------~%" case-name expected actual))
    )
)

; -----------------------------------------------------------------------------

; Test cases: Function 1 (FILLER)
(format t "Testing FILLER...~%")

(test-case-fr (FILLER 'TYPE C1) '(EMOTION SENTIM (POS)
                                          SCALE (>NORM)) "filler_ex_1")
  
(test-case (FILLER 'SRC C1) NIL "filler_ex_2")
(test-case (FILLER 'L-NAME '(HUMAN F-NAME (CHARLES)
                                   L-NAME ( ) 
                                   GENDER (MALE))) NIL "filler_ex_3")
(test-case (FILLER 'AGENT C3) 'AG001 "filler_ex_4")
(test-case (FILLER 'SHAPE (FILLER 'INTO C3)) '(CYLINDRICAL) "filler_ex_5")

(format t "===========================~%")

; -----------------------------------------------------------------------------

; Test cases: Function 2 (PATH-SL)
(format t "Testing PATH-SL...~%")

(test-case-fr (PATH-SL '(TYPE) C1) 
  '(EMOTION SENTIM (POS)
            SCALE (>NORM)) "path-sl_ex_1")

(test-case-fr (PATH-SL '(AGENT AGE UNIT) C1) 
  '(YEAR) "path-sl_ex_2")

(test-case (PATH-SL '(AGENT AGE LEWL LEWL LEWL) C1) 
  nil "path-sl_ex_3")

(test-case-fr (PATH-SL '(OBJECT INFO) C3) 
  '(STORY) "path-sl_ex_4")

(test-case (PATH-SL '(OBJECT) C3) 'OBJ001 "path-sl_ex_5")

(test-case-fr (PATH-SL nil C3) C3 "path-sl_ex_6")

(format t "===========================~%")

; -----------------------------------------------------------------------------

; Test cases: Function 3 (UNGAP)
(format t "Testing UNGAP...~%")

(test-case (UNGAP 'LOL-TEST) 'LOL-TEST "ungap_ex_1")
(test-case-fr (UNGAP 'F-NAME001) '(FRANK) "ungap_ex_2")
(test-case-fr (UNGAP '(HUMAN F-NAME F-NAME001 GENDER (MALE))) '(HUMAN F-NAME (FRANK) GENDER (MALE)) "ungap_ex_3")
(test-case-fr (UNGAP '(HUMAN F-NAME F-NAME002 GENDER (MALE))) '(HUMAN F-NAME F-NAME002 GENDER (MALE)) "ungap_ex_4")

(test-case-fr (UNGAP C3) 
    '(THROW AGENT (HUMAN F-NAME (CHARLES)
                         L-NAME ( )
                         GENDER (MALE))
                   OBJECT (PRINTED-MATTER TYPE (NEWSPAPER MATERIAL (PAPER))
                                          INFO (STORY)
                                          REF (INDEF)) 
                   INTO (CONTAINER SHAPE (CYLINDRICAL)
                                   FOR (TRASH)
                                   REF (DEF))
                   MANNER (SECRETLY)
                   EXP-VIOL (SEE AGENT (HUMAN F-NAME (CHARLOTTE)
                                              L-NAME ( )
                                              GENDER (FEMALE)) 
                                 OBJECT (HUMAN F-NAME (CHARLES)
                                               L-NAME ( )
                                               GENDER (MALE)))
                   LOC (HOME OWNER (HUMAN F-NAME (CHARLOTTE) 
                                          L-NAME ( )
                                          GENDER (FEMALE)))) "ungap_ex_5")
							
(test-case (UNGAP nil) nil "ungap_ex_6")

(format t "===========================~%")

; -----------------------------------------------------------------------------

; Test cases: Function 4 (ADD-SF)
(format t "Testing ADD-SF...~%")
(test-case-fr (add-sf 'AGENT '(DOG F-NAME (SPOT)) '(PET)) 
    '(PET AGENT (DOG F-NAME (SPOT))) "add-sf_ex_1")

(test-case-fr (add-sf 'AGENT '(DOG F-NAME (SPOT)) C2) 
    '(GIVE AGENT (DOG F-NAME (SPOT))
           TO (HUMAN F-NAME (CHARLOTTE) 
                     L-NAME ( )
                     GENDER (FEMALE)) 
           OBJECT (RING CONTAINS (EMERALD)
                        COST (>NORM))) "add-sf_ex_2")

(test-case-fr (add-sf 'EMOTION '(SENTIM (POS) SCALE (>NORM)) C2)
    '(GIVE EMOTION (SENTIM (POS) 
                    SCALE (>NORM))
           OBJECT (RING CONTAINS (EMERALD)
                        COST (>NORM))
           TO (HUMAN F-NAME (CHARLOTTE) 
                     L-NAME ( )
                     GENDER (FEMALE)) 
           AGENT (HUMAN F-NAME (CHARLES)
                        L-NAME ( ) 
                        GENDER (MALE))) "add-sf_ex_3")

(test-case-fr (add-sf 'L-NAME '(CHARLESTON) 'AG001)
    '(HUMAN F-NAME (CHARLES)
            L-NAME (CHARLESTON)
            GENDER (MALE)) "add-sf_ex_4")

(test-case-fr (add-sf 'SISTER 'CON005 'AG001)
    '(HUMAN F-NAME (CHARLES)
            L-NAME ( )
            GENDER (MALE)
            SISTER CON005) "add-sf_ex_5")

(test-case-fr (add-sf 'SISTER nil 'AG001)
    '(HUMAN F-NAME (CHARLES)
            L-NAME ( )
            GENDER (MALE)
            SISTER ( )) "add-sf_ex_6")

(test-case-fr (add-sf 'SISTER nil (add-sf 'SISTER 'CON005 'AG001))
    '(HUMAN F-NAME (CHARLES)
            L-NAME ( )
            GENDER (MALE)
            SISTER ( )) "add-sf_ex_7")

(format t "===========================~%")

; -----------------------------------------------------------------------------

; Test cases: Function 5 (SAME-SF)
(format t "Testing SAME-SF...~%")

(test-case (SAME-SF C1 C1) t "same-sf_ex_1")
(test-case (SAME-SF C1 C2) nil "same-sf_ex_2")
(test-case (SAME-SF C3 C3) t "same-sf_ex_3")
(test-case (SAME-SF '(EMOTION SENTIM (POS) SCALE (>NORM))
                    '(EMOTION SCALE (>NORM) SENTIM (POS))) t "same-sf_ex_4")
(test-case (SAME-SF 'AG001 'CON005) nil "same-sf_ex_5")
(test-case (SAME-SF 'EXPV001 'CON004) t "same-sf_ex_6")
(test-case (SAME-SF 'EXPV001 'LEWL) nil "same-sf_ex_7")
(test-case (SAME-SF 'LEWL 'LEWL) t "same-sf_ex_8")
(test-case (SAME-SF '(EMOTION SENTIM (POS) SCALE (>NORM)) '(FEELING SENTIM (POS) SCALE (>NORM))) nil "same-sf_ex_9")
(test-case (SAME-SF C3 (UNGAP C3)) t "same-sf_ex_10")
(test-case (SAME-SF nil nil) t "same-sf_ex_11")

(format t "===========================~%")