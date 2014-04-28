; CS 161 Spring 2014: HW2 skeleton

; Use some utility functions from HW1 solution set
; [!] NOTE: Set to the path of the HW1 solution, or your own
(load "hw-1-solution.lsp")

; File structure:
; Some convenience functions have been provided for you. They will be
; labeled GIVEN, to indicate you don't need to modify them as part of the
; assignment.
;
; Section 1: Utility functions:
;   -- CLEAR-GLOBALS (GIVEN): clears the global variables for the project
;   -- RELOAD (GIVEN): clears global variables and reloads your source file
;
;   -- NEXT-PH (PROBLEM 2): splits input phrase into first-rest using lexicon
;   -- NEWATM (PROBLEM 3): generates a unique atom from the input one
;   -- UNIQUE-GAPS (PROBLEM 4): replaces GAPs with unique atoms
;   -- SRCH (PROBLEM 7): searches a list of atoms for a predicate in a direction
;   -- BIND (PROBLEM 8): binds a GAP to a frame and updates global USEDMEM list
;
; Section 2: Main functions:
;   -- ADD-LEX (PROBLEM 1): adds information to the conceptual lexicon
;   -- INSTAN-CON (PROBLEM 5): adds an instantiated frame to working memory
;   -- SPAWN (PROBLEM 6): instantiates a list of demons and adds to ACTV-DEMONS
;   -- POLL-DEMS (PROBLEM 10): repeatedly calls demons in DEMEM until
;                              either they all succeed or an entire round
;                              passes with no demon succeeding
;   -- TOP-CON (PROBLEM 11): Looks through input CON list and returns those that
;                            have yet to be used
;   -- C-ANALYZER (PROBLEM 12): top-level function to analyze a sentence
;
; Section 3: Demons:
;   -- DM-EXP (PROBLEM 9)


; ****** BEGIN SECTION 1: UTILITY FUNCTIONS ******

; Resets global variables, THEN reloads your code. This means you can initialize the
; globals in your source file for testing purposes.
(defun RELOAD ()
    (clear-globals)
    (load "hw-2-skeleton.lsp") ; Replace with the name of this file
    ; Feel free to load additional files for testing etc here
    (load "hw-2-units.lsp")
)

; Resets the global variables used in the assignment.
(defun CLEAR-GLOBALS ()
    (setq LEXMEM NIL)
    (setq WKMEM NIL)
    (setq DEMEM NIL)
    (setq USEDMEM NIL)
)
; Call once to get the ball rolling!
(CLEAR-GLOBALS)

; ****** END GIVEN UTILITY FUNCTIONS ******

; ****** BEGIN MY UTILITY FUNCTIONS ******

; lex-has-phrase takes a copy of the current lex to recurse on and a phrase to
; search for. It returns t if the phrase appears in the current lex, and nil
; otherwise 
(defun lex-has-phrase (lex phrase)
    (cond
        ; if there are no phrase in the lex, it can't have phrase
        ((eq (length lex) 0) NIL)
        ; first item of the first item in the first phrase's phrase name
        ((equal (caar lex) phrase) t)
        ; not empty or the phrase in question, recurse
        (t (lex-has-phrase (cdr lex) phrase))
    )
)

; lex-remove phrase takes a copy of the current lex, and phrase to remove
; it does so by reordering the lex 
(defun lex-remove-phrase (lex phrase)
    (cond
        ; empty lex already has already phrases removed
        ((eq (length lex) 0) NIL)
        ; if the front of the lex is the one we want, return the rest
        ((equal (caar lex) phrase) (cdr lex))
        ; else return this case, plus whatever else passes this function
        (t (cons (car lex) (lex-remove-phrase (cdr lex) phrase)))
    )
)

(defun lex-replace-phrase (phrase frame demons)
    ; remove current phrase from LEXMEM
    (setq LEXMEM (lex-remove-phrase LEXMEM phrase))
    ; add the new phrase as usual
    (lex-add-phrase phrase frame demons)
)

(defun lex-add-phrase (phrase frame demons)
    ; save the value of lexmem, in case of weird stuff
    (let ((oldlex LEXMEM))
        ; add the new phrase list onto LEXMEM
        (setq LEXMEM (cons (list phrase frame demons) oldlex))
    )
)

(defun lex-get-phrase (phrase lexic)
    (cond
        ((eq (length lexic) 0) NIL)
        ((equal (caar lexic) phrase) (car lexic))
        (t (lex-get-phrase phrase (cdr lexic)))
    )
)

(defun ph-lcs (wrdlst phrase result)
    (cond 
        ((eq (length phrase) 0) (reverse result))
        ((equal (car wrdlst) (car phrase)) (ph-lcs (cdr wrdlst) (cdr phrase) (cons (car wrdlst) result)))
        (t (ph-lcs (cdr wrdlst) (cdr phrase) result))
    )
)

(defun ph-greatest-match (wrdlst lexic match)
    (let ((this-match (ph-lcs wrdlst (caar lexic) NIL)))
        (cond
            ((eq (length lexic) 0) match)
            ((>  (length this-match) (length match)) (ph-greatest-match wrdlst (cdr lexic) this-match))
            (t (ph-greatest-match wrdlst (cdr lexic) match))
        )
    )
)

(defun ph-rest (wrdlst match rest)
    (cond
        ((eq (length match) 0) (append wrdlst rest))
        ((equal (car wrdlst) (car match)) (ph-rest (cdr wrdlst) (cdr match) rest))
        (t 'BADMATCH)
    )
)

(defun sf-unique-gaps (sfpairs done)
    (cond
        ((eq (length sfpairs) 0) (reverse done))
        (t (sf-unique-gaps (nthcdr 2 sfpairs) (append (cons (UNIQUE-GAPS (car (cdr sfpairs))) (list (car sfpairs))) done)))
    )
)

(defun spawn-int (partial-dems mycon done)
    (cond
        ((eq (length partial-dems) 0) done)
        (t (spawn-int (cdr partial-dems) mycon (cons (append (cons (caar partial-dems) (list mycon)) (cdr (car partial-dems))) done)))
    )
)

(defun srch-int (srchlst pred)
    (cond 
        ((eq (length srchlst) 0) NIL)
        ((equal (first (symbol-value (car srchlst))) pred) (car srchlst))
        (t (srch-int (cdr srchlst) pred))
    )
)

(defun srch-dir-sort (atmlst myatm dir)
    ;prepare search list cleverly to get around most problems
    (cond 
        ((equal dir 'AFT) (srch-sort atmlst myatm NIL))
        ((equal dir 'IM-AFT) (list (car (srch-sort atmlst myatm NIL))))
        ((equal dir 'BEF) (srch-sort (reverse atmlst) myatm NIL))
        ((equal dir 'IM-BEF) (list (car (srch-sort (reverse atmlst) myatm NIL))))
    )
)

(defun srch-sort (atmlst myatm done)
    (cond
        ((equal (car atmlst) myatm) (cdr (append atmlst done)))
        (t (srch-sort (cdr atmlst) myatm done))  
    )
)

(defun pd-int (demlst proclst)
    (cond 
        ((eq (length demlst) 0) proclst)
        (t (progn
                (let ((dm-result (apply (caar demlst) (cdr (car demlst))) ))
                    (cond 
                        ((equal dm-result NIL) (pd-int (cdr demlst) (cons (car demlst) proclst)))
                        ((equal dm-result '(DIE)) (pd-int (append (cdr demlst) proclst) NIL))
                        (t 'WEIRDSTUFF)
                    )
                )
            )
        )
    )
)

(defun c-make-ungapped-atoms (atoms done)
    (cond
        ((eq (length atoms) 0) done)
        (t (c-make-ungapped-atoms (cdr atoms) (cons (UNGAP (car atoms)) done)))
    )
)

(defun c-remove-front (oldsent phrase)
    (cond
        ((eq (length phrase) 0) oldsent)
        ((equal (car oldsent) (car phrase)) (c-remove-front (cdr oldsent) (cdr phrase)))
        (t 'TRIED-BAD-SENT-DELETE)
    )
)

; ****** END MY UTILITY FUNCTIONS ******

; ****** BEGIN PROBLEM SKELETONS ******

; -----------------------------------------------------------------------------

; PROBLEM 2: NEXT-PH

; NEXT-PH takes a list of words WRDLST and a lexicon LEXIC (the same structure
; as our global variable LEXMEM) and returns a list with the structure 
; (phrase rest)
; ...where phrase is a list of the words that appear at the very
; front of WRDLST and that matches the LONGEST phrase found in LEXIC.
; ...and where rest is the rest of WRDLST with phrase removed
; INPUTS: WRDLST - a phrase to dissect into (phrase rest)
;         LEXIC  - a lexicon with formatting identical to the global LEXMEM
; OUTPUT: List - (phrase rest) as described above

(defun NEXT-PH (WRDLST LEXIC)
    (let ((longest-phrase (ph-greatest-match WRDLST LEXIC NIL)))
        (let ((phrase-clause (lex-get-phrase longest-phrase LEXIC))
        (rest-wrdlst (ph-rest WRDLST longest-phrase NIL)))
            (if (equal longest-phrase NIL)
                (append (list (car WRDLST) (list 'UNKNOWN 'WORD (list (car WRDLST)) '())) (cdr wrdlst)) 
                (append (list phrase-clause) rest-wrdlst)
            )
        )
    )
)

; -----------------------------------------------------------------------------

; PROBLEM 3: NEWATM

; NEWATM takes the name of a symbol symName and generates a fresh, 
; unbound symbol with a unique name based upon symName. The numbering will
; begin with 1 and be incremented, using gentemp, with each newatm created

; param symName - a symbol name to prefix the name of the symbol returned
; returns       - a fresh, unbound symbol with a unique name consisting of:
;                 symName#, where symName was the input symbol turned string
;                           with a unique integer appended where # is
;
; Examples:
; > (NEWATM 'AGENT)
; AGENT1
;
; > (boundp (NEWATM 'AGENT))
; NIL
;
; Now, with another call, (NEWATM 'AGENT) should return:
;  AGENT2

; Begin by setting the gensym-counter to start at 1; this will start all
; numbering using gentemp with 1, as intended
(setq *gensym-counter* 1)
(defun NEWATM (symName)
    ; Replace 'UNIMPLEMENTED with your symbol generation
    (let* ( (new-sym (gensym (string symName))) )
        ; This line included to make your life easy without diving into
        ; what it means! Leave it alone :)
        (intern (string new-sym))
    )
)

; -----------------------------------------------------------------------------

; PROBLEM 4: UNIQUE-GAPS

; UNIQUE-GAPS should recursively go through a frame, replacing each gap it finds
; with a NEWATM version of that gap. If called on a gap, it should replace
; the gap with a NEWATM version.
; INPUTS: frame (concept or gap)
; OUTPUT: frame instance (all gaps unique), or unique gap

(defun UNIQUE-GAPS (frame)
    (cond
        ; if frame is an atom, it's a gap so bind it
        ((atom frame) (NEWATM frame))
        ; if the first slot isn't a gap, ungap the sfpairs
        (t (cons (car frame) (sf-unique-gaps (cdr frame) NIL)))

    )
)

; -----------------------------------------------------------------------------

; PROBLEM 7: SRCH

; SRCH searches through a list of atoms, which is structured as a list of CON
; atoms that evaluate to frames. It should start its search at the atom ATMLST
; that matches MYATM, moving either immediately or iteratively forward or backward
; in ATMLST looking for a frame whose top-level predicate matches pred.
; INPUTS: atmlst - list of atoms that eval to frames
;         myatm  - atom to start at
;         dir    - direction to search which can be:
;                  AFT -> forward
;                  IM-AFT -> immediately following
;                  BEF -> backward
;                  IM-BEF -> immediately preceding
;         pred: pred to search for
; OUTPUT: frame-reference atom if successful, NIL otherwise

(defun SRCH (atmlst myatm dir pred)
    (let ((srch-lst (srch-dir-sort atmlst myatm dir)))
        (srch-int srch-lst pred)
    )
)

; -----------------------------------------------------------------------------

; PROBLEM 8: BIND

; BIND takes a gap and a con-atom found, and uses SET to set gap = found. It 
; should also update the global variable USEDMEM, by adding the found atom to 
; its end.
; (USEDMEM is merely a list of CON-atoms on which BIND has been called)
; INPUT: gap (atom)   - gap to be bound
;        found (atom) - concept atom to be bound
; OUTPUT: found
; SIDE-EFFECT: binds gap to found, and adds found to the global variable USEDMEM

(defun BIND (gap found)
    (setf USEDMEM (cons found USEDMEM))
    (set gap found)
    found
)

; -----------------------------------------------------------------------------

; ****** END SECTION 1 ******


; ****** BEGIN SECTION 2: MAIN FUNCTIONS ******

; PROBLEM 1: ADD-LEX

; ADD-LEX adds an entry to the global variable LEXMEM, which is a representation
; of a conceptual lexicon. A conceptual lexicon is a list of lists, where each
; internal list has three parts: a list containing one or more words (phrase),
; a frame, and a list of zero or more demons.
; INPUT: phrase (list) - list of one or more words (as atoms), which 
;                        sentences will be matched against
;        frame (frame) - frame to store in lexicon
;        demons (list) - list of demons (function calls stored as data)
; OUTPUT: (whatever you want, irrelevant to functionality)
; SIDE-EFFECT: Updates global LEXMEM by appending the lexicon entry defined  
;              by the input arguments.
(defun ADD-LEX (phrase frame demons)
    (cond
        ((lex-has-phrase LEXMEM phrase) (lex-replace-phrase phrase frame demons))
        (t (lex-add-phrase phrase frame demons))
    )
)

; -----------------------------------------------------------------------------

; PROBLEM 5: INSTAN-CON

; INSTAN-CON instantiates an input frame by making any gaps in it unique (using
; UNIQUE-GAPS), creating a new unique CON atom with the instantiated frame as
; its value, and then adding that new CON atom to the end of working memory
; input WKM. As a side effect, WKMEM = WKM at the end of the function.
; INPUT: frame - frame to uniquely gap and then bind the new CON atom to
;        wkm   - working memory input (list of CON atoms)
; OUTPUT: wkm with new CON atom added
; SIDE-EFFECT: Adds generated CON atom to the end of the global list WKMEM

(defun INSTAN-CON (frame wkm)
    (if (equal frame NIL) 'CALLED-ON-N)
    (let ((ungapped-frame (UNIQUE-GAPS frame))
        (new-con-atm (NEWATM 'CON)))
        (set new-con-atm ungapped-frame)
        (setf wkm (reverse (cons new-con-atm (reverse wkm))))
        (setf WKMEM wkm)
        wkm    
    )
)

; -----------------------------------------------------------------------------

; PROBLEM 6: SPAWN

; SPAWN takes in a list of partial demon instances PARTIAL-DEMS and completes
; each partial demon instance by adding the atom MYCON as a first argument to
; each instance. Spawn returns a completed version of PARTIAL-DEMS, and as a
; side effect, adds these completed demon-instances to the front of the global
; variable DEMEM.
; INPUT: partial-dems (list) - list of demons (partial function calls) from 
;                              lexicon
;        mycon               - atom to add as first argument to each demon
;                              partial function call in partial-dems
; OUTPUT: list of completed demon instances
; SIDE-EFFECT: adds each demon instance created as above to the global list
;              DEMEM.

(defun SPAWN (partial-dems mycon)
    (let ((completed-dems (spawn-int partial-dems mycon NIL)))
        (setf DEMEM (append completed-dems DEMEM))
        completed-dems
    )
)

; -----------------------------------------------------------------------------

; PROBLEM 10: POLL-DEMS

; POLL-DEMS goes through a list of demon instances in DEMLST and polls them 
; (i.e. executes each one). When a demon is successful, it will return (DIE) 
; to POLL-DEMS, which will cause POLL-DEMS to remove that demon instance from 
; DEMLST. If any demon returns (DIE) then POLL-DEMS will re-poll the remaining 
; demons. It does this until no demon returns (DIE). That is, POLL-DEMS polls 
; demons repeatedly until they are all quiescent (they all return NIL). 
; POLL-DEMS returns DEMLIST.
; INPUT: demlist (list): list of demon instances
;        wkm (list): list of CON-atoms (which evaluate to frames)
; OUTPUT: list of demons still remaining after quiescence is reached

(defun POLL-DEMS (demlist)
    (let ((new-demem (pd-int demlist NIL)))
        (setf DEMEM new-demem)
        DEMEM
    )
)

; -----------------------------------------------------------------------------

; PROBLEM 11: TOP-CON

; TOP-CON goes through the atoms in WKM and returns a list of all atoms that do
; NOT appear in USED.
; INPUT: wkm (list)  - working memory atoms
;        used (list) - used atoms
; OUTPUT: List of atoms in wkm that do not appear in USED

(defun TOP-CON (wkm used)
    (set-difference wkm used)
)

; -----------------------------------------------------------------------------

; PROBLEM 12: C-ANALYZER

; C-ANALYZE is the top-most level function, that will return a frame representation
; of a sentence represented as a sequence of words. It loops through attempting
; to match the words of SENT to words or phrases in the lexicon, LEXIC, using
; NEXT-PH. For each recognized phrase, it INSTAN-CONs the associated frame (also
; adding the CON atom of that frame to working memory within INSTAN-CON), SPAWNs 
; the associated demon list, and invokes POLL-DEMS on the global list DEMEM.
; This process repeats until SENT is empty.
; C-ANALYZE then returns the UNGAP of the TOP-CON of working memory as it stands
; after loading in the complete sentence.
; INPUT: sent (list)  - list of words (as atoms) representing a sentence
;        lexic (list) - a conceptual lexicon (see problem 1)

(defun C-ANALYZE (sent lexic)
    (if (equal sent NIL) (return-from C-ANALYZE (c-make-ungapped-atoms (TOP-CON WKMEM USEDMEM) ())))
    (let ((next-ph-result (next-ph sent lexic)))
        (if (equal (second (car next-ph-result)) NIL) 'EXTRACON)
        (let ((new-con (INSTAN-CON (second (car next-ph-result)) WKMEM)))
            (let ((new-demons (SPAWN (third (car next-ph-result)) (last WKMEM))))
                (POLL-DEMS DEMEM)
                (C-ANALYZE (cdr next-ph-result) lexic)
            )
        )
    )
    (POLL-DEMS DEMEM)
)


; -----------------------------------------------------------------------------

; ****** END SECTION 2 ******


; ****** BEGIN SECTION 3: DEMONS ******

; Demons are short functions to BIND gaps in different frames of working memory
; to each other. Any given invocation of a demon may be successful (it found
; the things it was looking for and BIND'd them), or if it failed to find what
; it was looking for, it does nothing, waiting for later invocations. (It might
; succeed later when more of the sentence has been loaded into working memory.)
;
; Every demon gets as its first argument the CON it is working for (a particular
; element of working memory), which anchors searches for other frames, and will
; typically have gaps that get bound by the demon. Other arguments vary depending
; on the demon's function.
;
; The return value of a demon indicates whether it successfully
; did its operation: (DIE) value on success, or NIL on failure.

; -----------------------------------------------------------------------------

; PROBLEM 9: DM-EXP

; DM-EXP looks for a CON atom in the global variable WKMEM with top predicate 
; PRED. If it finds this PRED, then let's call that CON atom found. If it finds
; found then this DM-EXP instance binds found to the gap associated with MYSLOT
; in MYCON (using the BIND function you developed in problem #8). It returns 
; (DIE) and (as a side-effect of having used the BIND function from problem #8) 
; now found has been added to the global variable USEDMEM. When looking for 
; found, DM-EXP starts searching at MYCON and looks in the global variable WKMEM
; in direction DIR. If DM-EXP is unsuccessful it returns NIL.
; INPUT: mycon (atom)  - demon's frame that it works for in working memory
;        pred (atom)   - top-level predicate to find in WKMEM
;        dir (atom)    - direction of search to perform
;        myslot (atom) - slot in mycon to bind
; OUTPUT: (DIE) if successfully executed and bound, nil otherwise

(defun DM-EXP (mycon pred dir myslot)
    (let ((srch-result (srch WKMEM mycon dir pred)))
        (if (equal srch-result NIL)
            NIL
            (progn 
                (BIND (FILLER myslot (symbol-value mycon)) srch-result)
                '(DIE)
            )
        )
    )
)

; ****** END SECTION 3 ******
