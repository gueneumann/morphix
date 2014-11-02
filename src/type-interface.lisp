;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: COMMON-LISP-USER -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;        file:
;;;      module:
;;;     version:
;;;  written by:
;;; last update:
;;;  updated by:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; author            | date        | modification
;;; ------------------|-------------|------------------------------------------
;;;                   |             |
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "MORPHIX")

(DEFVAR *NOUN-TYPES* NIL)
(DEFVAR *VERB-TYPES* NIL)
(DEFVAR *ADJ-TYPES* NIL)
(DEFVAR *PARTICPLE-PERFECT-ATTR-TYPES* NIL)
(DEFVAR *PARTICPLE-PRAESENS-ATTR-TYPES* NIL)
(DEFVAR *PARTICPLE-PRAESENS-MIT-ZU-TYPES* NIL)
(DEFVAR *ORDINAL-TYPES* NIL)
(DEFVAR *POSSESSIV-TYPES* NIL)
(DEFVAR *DEF-DET-TYPES* NIL) ;; neu kn
(DEFVAR *INDEF-DET-TYPES* NIL) ;; neu kn
(DEFVAR *SPECIAL-TYPES* NIL)

(setq *SPECIAL-TYPES* '(("PX-ALL" . "PX-ALL")
                        ("INTERPUNKTION" . "INTERPUNKTION")
                        ("CARDINAL" . "CARDINAL")
                        ("PX-ADV" ."PX-ADV" )
                        ("PX-P-PRFX" . "PX-P-PRFX")))





(SETQ *NOUN-TYPES* 
  '(
    ;;; THE NOUN TYPES
    ((((FEM ((SG (NOM DAT AKK)))))) . "NX-FEM-NDA-SG")   ; neu 29/8/94 GN

    ((((FEM ((SG (GEN)))))) . "NX-FEM-G-SG")             ; neu 29/8/94 GN
    ((((FEM ((SG (NOM GEN DAT AKK)))))) . "NX-FEM-SG")
    ((((FEM ((PL (NOM GEN DAT AKK)))))) . "NX-FEM-PL")
    ((((NTR ((SG (NOM DAT AKK)))))) . "NX-NEU-NDA-SG")
    ((((NTR ((SG (GEN)))))) . "NX-NEU-G-SG")
    ((((NTR ((PL (NOM GEN AKK)))))) . "NX-NEU-NGA-PL")
    ((((NTR ((PL (DAT)))))) . "NX-NEU-D-PL")
    ((((NTR ((PL (NOM GEN AKK)) (SG (NOM DAT AKK)))))) . 
     "NX-NEU-NDA-SG-NGA-PL")
    ((((MAS ((SG (NOM DAT AKK)))))) . "NX-MAS-NDA-SG")
    ((((MAS ((SG (GEN)))))) . "NX-MAS-G-SG")
    ((((MAS ((PL (NOM GEN AKK)) (SG (DAT)))))) . "NX-MAS-D-SG-NGA-PL")
    ((((MAS ((PL (DAT)))))) . "NX-MAS-D-PL")
    ((((MAS ((PL (NOM GEN AKK)))))) . "NX-MAS-NGA-PL")
    ((((NTR ((PL (NOM GEN AKK)) (SG (DAT)))))) . "NX-NEU-D-SG-NGA-PL")
    ((((NTR ((PL (NOM GEN DAT AKK)) (SG (NOM DAT AKK)))))) . 
     "NX-NEU-NDA-SG-PL")
    ((((MAS ((PL (NOM GEN AKK)) (SG (NOM DAT AKK)))))) . 
     "NX-MAS-NDA-SG-NGA-PL")
    ((((NTR ((PL (NOM GEN DAT AKK)) (SG (GEN)))))) . "NX-NEU-G-SG-PL")
    ((((NTR ((PL (NOM GEN DAT AKK)))))) . "NX-NEU-PL")
    ((((MAS
        ((PL (NOM GEN DAT AKK)) (SG (NOM DAT AKK)))))) . "NX-MAS-NDA-SG-PL")
    ((((MAS ((SG (NOM)))))) . "NX-MAS-N-SG")
    ((((MAS
        ((PL (NOM GEN DAT AKK))
         (SG (GEN DAT AKK)))))) . "NX-MAS-GDA-SG-PL")
    ((((FEM ((PL (NOM GEN AKK)))))) . "NX-FEM-NGA-PL")
    ((((FEM ((PL (DAT)))))) . "NX-FEM-D-PL")
    ((((MAS ((PL (NOM GEN DAT AKK)))))) . "NX-MAS-PL")
    ((((MAS ((SG (DAT)))))) . "NX-MAS-D-SG")
    ((((NTR ((SG (DAT)))))) . "NX-NEU-D-SG")
    ((((MAS ((PL (NOM GEN DAT AKK)) (SG (GEN)))))) . "NX-MAS-G-SG-PL")
    ((((MAS ((SG (GEN DAT AKK)))))) . "NX-MAS-GDA-SG")
    ((((NIL ((PL (NOM GEN DAT AKK)))))) . :wrong)
    ((((FEM
         ((PL (NOM GEN AKK))
          (SG (NOM GEN DAT AKK)))))) . :wrong)
    ((((FEM
         ((PL (NOM GEN DAT AKK))
          (SG (NOM GEN DAT AKK)))))) . "NX-FEM-SG-PL")
    ((((NTR
         ((PL (NOM GEN DAT AKK)) (SG (NOM GEN DAT AKK)))))) . "NX-NEU-SG-PL")
    ((((NTR ((SG (NOM GEN DAT AKK)))))) . "NX-NEU-SG")
    ((((MAS ((PL (NOM GEN AKK)) (SG (NOM GEN DAT AKK)))))) . 
     ;;     "NX-MAS-SG-NGA-PL"
     :wrong)
    ((((MAS ((SG (NOM GEN DAT AKK)))))) . "NX-MAS-SG")
    ((((MAS ((SG (DAT AKK)))))) . "NX-MAS-DA-SG")
    ((((FEM ((SG (NOM)))))) . :wrong)
    ((((FEM
         ((PL (NOM GEN DAT AKK))
          (SG (GEN DAT AKK)))))) . :NX-ERROR)
    ((((NTR ((SG (GEN DAT AKK)))))) . "NX-NEU-GDA-SG")
    ((((NIL ((PL (NOM GEN AKK)))))) . :wrong)
    ((((NIL ((PL (DAT)))))) . :wrong)
    ((((MAS ((PL (NOM GEN DAT AKK)) (SG (DAT AKK)))))) . "NX-MAS-DA-SG-PL")
    ((((NTR ((SG (NOM AKK)))))) . "NX-NEU-NA-SG")
    ((((NTR ((PL (NOM GEN DAT AKK)) (SG (DAT)))))) . "NX-NEU-D-SG-PL")
    ((((MAS ((PL (NOM GEN DAT AKK)) (SG (NOM GEN DAT AKK)))))) . 
     "NX-MAS-SG-PL")
    ((((NTR ((SG (NOM)))))) . "NX-NEU-N-SG")
    ((((NTR
         ((PL (NOM GEN DAT AKK))
          (SG (GEN DAT AKK)))))) . 
     :wrong)
    ))


(SETQ *VERB-TYPES* 
  '(
    ;;; THE VERB TYPES
   
    ((((PRAESENS ((SG (ANREDE)) (PL (1 3 ANREDE))))
       (KONJUNKTIV-1 ((SG (ANREDE)) (PL (1 3 ANREDE)))) (INFINITIV)
       (IMPERATIV (ANREDE)))) . 
     "VX-SUP-BARE-PRES-PL-1-3") ; 8
    ((((PARTIZIP-PERFEKT) (PRAESENS ((SG (3)) (PL (2))))
       (IMPERATIV (PL)))) . 
     "VX-SUP-PERF-PRES-IND-SG-3-IND-IMP-PL-2") ; 9
    ((((PRAESENS ((SG (1)))) (KONJUNKTIV-1 ((SG (1 3))))
       (IMPERATIV (SG)))) . 
     "VX-PRES-SG-1-CONJ-3-IMP-2") ; 10
    ((((PRAESENS ((SG (2)))))) . "VX-PRES-IND-SG-2") ; 11
    ((((KONJUNKTIV-1 ((SG (2)))))) . "VX-PRES-CONJ-SG-2") ; 12
    ((((KONJUNKTIV-1 ((PL (2)))))) . "VX-PRES-CONJ-PL-2") ; 13
    ((((IMPERFEKT ((SG (1 3)))) (KONJUNKTIV-2 ((SG (1 3)))))) . 
     "VX-PAST-SG-1-3")            ; 14
    ((((IMPERFEKT ((SG (2)))) (KONJUNKTIV-2 ((SG (2)))))) . 
     "VX-PAST-SG-2")            ;16
    ((((IMPERFEKT ((SG (ANREDE)) (PL (1 3 ANREDE))))
       (KONJUNKTIV-2 ((SG (ANREDE)) (PL (1 3 ANREDE)))))) . 
     "VX-PAST-PL-1-3")            ; 18
    ((((IMPERFEKT ((PL (2)))) (KONJUNKTIV-2 ((PL (2)))))) . 
     "VX-PAST-PL-2")            ;20
    ((((PARTIZIP-PRAESENS))) . 
     "VX-PZP-BARE-NULL")        ; 21
    ((((ERWEITERTER-INFINITIV))) . 
     "VX-SUP-ZU")               ;25
    ((((PRAESENS ((SG (ANREDE)) (PL (1 3 ANREDE))))
       (KONJUNKTIV-1 ((SG (ANREDE)) (PL (1 3 ANREDE))))
       (INFINITIV))) . 
     "VX-SUP-BARE-PRES-PL-1-3") ;26
    ((((PRAESENS ((PL (2)))) (IMPERATIV (PL))
       (KONJUNKTIV-1 ((PL (2)))))) . 
     "VX-PRES-IND-CONJ-IMP-PL-2") ;27
    ((((PRAESENS ((SG (3)))) (IMPERATIV (SG)))) . 
     "VX-PRES-SG-IND-3-IMP-2")  ;28
    ((((PRAESENS ((SG (1)))) (KONJUNKTIV-1 ((SG (1 3)))))) . 
      "VX-PRES-SG-1-CONJ-3")      ;29
    ((((PRAESENS ((SG (3)))))) . 
     "VX-PRES-IND-SG-3")        ;31
    ((((PRAESENS ((PL (2)))) (KONJUNKTIV-1 ((PL (2)))))) . 
     "VX-PRES-PL-2")            ;33
    ((((IMPERFEKT ((SG (1 3)))))) . 
     "VX-PAST-IND-SG-1-3")      ;34
    ((((IMPERFEKT ((SG (2)))))) . 
     "VX-PAST-IND-SG-2")        ; 35
    ((((IMPERFEKT ((SG (ANREDE)) (PL (1 3 ANREDE)))))) . 
     "VX-PAST-IND-PL-1-3")        ;36
    ((((IMPERFEKT ((PL (2)))))) . 
     "VX-PAST-IND-PL-2")        ; 37
    ((((KONJUNKTIV-2 ((SG (1 3)))))) . 
     "VX-PAST-CONJ-SG-1-3")     ;38
    ((((KONJUNKTIV-2 ((SG (2)))))) . 
     "VX-PAST-CONJ-SG-2")       ;39
    ((((KONJUNKTIV-2 ((SG (ANREDE)) (PL (1 3 ANREDE)))))) . 
     "VX-PAST-CONJ-PL-1-3")       ;40
    ((((KONJUNKTIV-2 ((PL (2)))))) . 
     "VX-PAST-CONJ-PL-2")       ; 41
    ((((PARTIZIP-PRAESENS-MIT-ZU))) . :WRONG) ; 42
    ((((PARTIZIP-PERFEKT))) . 
     "VX-SUP-PERF")             ;43
    ((((PRAESENS ((SG (3)) (PL (2)))) (IMPERATIV (PL)))) . 
     "VX-PRES-IND-SG-3-IND-IMP-PL-2") ;52
    ((((PRAESENS ((SG (3)) (PL (2)))))) . 
     "VX-PRES-IND-SG-3-PL-2")   ;53
    ((((PRAESENS ((SG (3)) (PL (2)))) (KONJUNKTIV-1 ((SG (3)) (PL (2))))
       (IMPERATIV (PL)))) . 
     "VX-PRES-SG-3-IND-CONJ-IMP-PL-2") ;54
    ((((PRAESENS ((SG (2)))) (KONJUNKTIV-1 ((SG (2)))))) . 
     "VX-PRES-SG-2")            ;55
    ((((PRAESENS ((PL (2)))) (IMPERATIV (PL)))) . 
     "VX-PRES-IND-IMP-PL-2")    ;56
    ((((PRAESENS ((SG (2 3)))))) . 
     "VX-PRES-IND-SG-2-3")      ;57
    ((((PRAESENS ((PL (2)))))) . 
     "VX-PRES-IND-PL-2")        ;58
    ((((IMPERATIV (SG)))) . 
     "VX-IMP-SG-2")             ;71
    ((((PRAESENS ((SG (3)) (PL (2))))
       (KONJUNKTIV-1 ((SG (3)) (PL (2)))))) . 
     "VX-PRES-SG-3-PL-2")       ;72
    ((((PARTIZIP-PERFEKT) (PRAESENS ((SG (2 3)) (PL (2))))
       (IMPERATIV (PL)))) . 
     "VX-SUP-PERF-PRES-IND-SG-2-3-IND-IMP-PL-2") ;73
    ((((PRAESENS ((SG (2 3)) (PL (2)))) (IMPERATIV (PL)))) . 
     "VX-PRES-IND-SG-2-3-IND-IMP-PL-2") ;74
    ((((PARTIZIP-PERFEKT) (PRAESENS ((SG (3)) (PL (2))))
       (KONJUNKTIV-1 ((SG (3)) (PL (2)))) (IMPERATIV (PL)))) . 
     "VX-SUP-PERF-PRES-SG-3-IND-CONJ-IMP-PL-2")                      ;77
    ((((PARTIZIP-PERFEKT) (PRAESENS ((SG (3)) (PL (2)))) (IMPERATIV (PL))
       (KONJUNKTIV-1 ((PL (2)))))) . 
     "VX-SUP-PERF-PRES-IND-SG-3-IND-CONJ-IMP-PL-2") ;78
    ((((PARTIZIP-PERFEKT) (PRAESENS ((SG (3)) (PL (2)))))) .
     "VX-SUP-PERF-PRES-IND-SG-3-PL-2") ; 79
    ((((PRAESENS ((SG (3)) (PL (2)))) (IMPERATIV (PL))
       (KONJUNKTIV-1 ((PL (2)))))) . 
     "VX-PRES-IND-SG-3-IND-CONJ-IMP-PL-2") ;80
    ((((PRAESENS ((SG (3)) (PL (2))))
       (KONJUNKTIV-1 ((PL (2)))))) . 
     "VX-PRES-PL-2-IND-SG-3")   ;81
    ((((PRAESENS ((SG (2 3)) (PL (2)))))) . 
     "VX-PRES-IND-SG-2-3-PL-2") ; 83
    ((((PARTIZIP-PERFEKT) (PRAESENS ((SG (ANREDE)) (PL (1 3 ANREDE))))
       (KONJUNKTIV-1 ((SG (ANREDE)) (PL (1 3 ANREDE))))
       (INFINITIV))) . 
     "VX-SUP-BARE-PERF-PRES-PL-1-3") ; 92
    
    ((((PARTIZIP-PERFEKT) (PRAESENS ((SG (ANREDE)) (PL (1 3 ANREDE))))
       (KONJUNKTIV-1 ((SG (ANREDE)) (PL (1 3 ANREDE)))) (INFINITIV)
       (IMPERATIV (ANREDE)))) . 
     "VX-SUP-BARE-PERF-PRES-PL-1-3")                          ;93
    
    ((((PRAESENS ((SG (3)) (PL (2)))) (KONJUNKTIV-1 ((PL (2))))
       (IMPERATIV (PL)))) . "VX-PRES-SG-3-IND-CONJ-IMP-PL-2") ; 95
    ((((PARTIZIP-PERFEKT)
       (IMPERFEKT ((SG (ANREDE)) (PL (1 3 ANREDE)))))) . 
     "VX-SUP-PERF-PAST-IND-PL-1-3") ;96
    ((((PRAESENS ((SG (1 3)))))) . 
     "VX-PRES-IND-SG-1-3")      ; 97
    ((((KONJUNKTIV-1 ((SG (3)) (PL (2)))))) . 
     ;; "VX-PRES-CONJ-SG-3-PL-2"  ; 98, Morphix-Fehler, Typ kann am Ende
     ;; eliminiert werden; relevante Beispiele wasche/waschet
        "VX-PRES-CONJ-PL-2")  ; 98
    ((((PARTIZIP-PERFEKT) (PRAESENS ((PL (2))))
       (IMPERATIV (PL)))) . :wrong) ;100
    ((((PARTIZIP-PERFEKT) (IMPERFEKT ((SG (ANREDE)) (PL (1 3 ANREDE))))
       (KONJUNKTIV-2 ((SG (ANREDE)) (PL (1 3 ANREDE)))))) . 
     "VX-SUP-PERF-PAST-PL-1-3")   ;101
    ((((KONJUNKTIV-1 ((SG (1 3)))) (IMPERATIV (SG)))) . 
     "VX-PRES-SG-CONJ-1-3-IMP-2") ; 119
    ((((PARTIZIP-PERFEKT) (PRAESENS ((SG (3)) (PL (2))))
       (KONJUNKTIV-1 ((SG (3)) (PL (2)))))) . 
     "VX-SUP-PERF-PRES-SG-3-PL-2") ;120
    ((((KONJUNKTIV-1 ((SG (1 3)))))) . 
     "VX-PRES-CONJ-SG-1-3")     ; 124
    ((((INFINITIV))) . "VX-SUP-BARE") ; 129
    ((((PARTIZIP-PERFEKT)
       (PRAESENS ((SG (2 3)) (PL (2)))))) . :wrong) ;133
    ((((PARTIZIP-PERFEKT) (PRAESENS ((SG (3)) (PL (2))))
       (KONJUNKTIV-1 ((PL (2)))))) . :wrong) ; 134
    ((((PARTIZIP-PERFEKT) (KONJUNKTIV-1 ((PL (2)))))) . 
     "VX-SUP-PERF-PRES-CONJ-PL-2") ;135
    ((((KONJUNKTIV-1 ((SG (ANREDE)) (PL (1 3 ANREDE))))
       (IMPERATIV (ANREDE)))) . 
     "VX-PRES-CONJ-PL-1-3 ")    ;148
    ((((PRAESENS ((SG (1)))))) . 
     "VX-PRES-IND-SG-1")        ;149
    ((((PRAESENS ((SG (ANREDE)) (PL (1 3 ANREDE)))))) . 
     "VX-PRES-IND-PL-1-3")       ;150
    ((((PARTIZIP-PERFEKT) (PRAESENS ((PL (2)))) (IMPERATIV (PL))
       (KONJUNKTIV-1 ((PL (2)))))) . :wrong) ;151
    ((((PRAESENS ((SG (2 3)))) (IMPERATIV (SG)))) . :wrong) ;152
    ((((KONJUNKTIV-1
        ((SG (ANREDE)) (PL (1 3 ANREDE)))))) . 
     "VX-PRES-CONJ-PL-1-3")     ;160
      
    ))

(SETQ *ADJ-TYPES*
  '(
    ((((POS ((PRAEDIKATIV-GEBRAUCHT))))) . "AX-POS-NULL") ; 48
    ((((POS
        ((OHNE
          ((MAS ((SG (NOM)) (PL (GEN)))) (FEM ((SG (GEN DAT)) (PL (GEN))))
           (NTR ((PL (GEN))))))
         (UNBESTIMMT ((MAS ((SG (NOM)))))))))) . "AX-POS-ER") ; 49
    ((((POS
     ((OHNE
       ((MAS ((PL (NOM AKK)))) (FEM ((SG (NOM AKK)) (PL (NOM AKK))))
        (NTR ((PL (NOM AKK))))))
      (BESTIMMT
       ((MAS ((SG (NOM)))) (FEM ((SG (NOM AKK)))) (NTR ((SG (NOM AKK))))))
      (UNBESTIMMT ((FEM ((SG (NOM AKK)))))))))) . "AX-POS-E")
    ((((POS
     ((OHNE
       ((MAS ((SG (GEN AKK)) (PL (DAT)))) (FEM ((PL (DAT))))
        (NTR ((SG (GEN)) (PL (DAT))))))
      (BESTIMMT
       ((MAS ((SG (GEN DAT AKK)) (PL (NOM GEN DAT AKK))))
        (FEM ((SG (GEN DAT)) (PL (NOM GEN DAT AKK))))
        (NTR ((SG (GEN DAT)) (PL (NOM GEN DAT AKK))))))
      (UNBESTIMMT
       ((MAS ((SG (GEN DAT AKK)) (PL (NOM GEN DAT AKK))))
        (FEM ((SG (GEN DAT)) (PL (NOM GEN DAT AKK))))
        (NTR ((SG (GEN DAT)) (PL (NOM GEN DAT AKK)))))))))) . "AX-POS-EN")
    ((((POS
     ((OHNE ((NTR ((SG (NOM AKK))))))
      (UNBESTIMMT ((NTR ((SG (NOM AKK)))))))))) . "AX-POS-ES")
    ((((POS ((OHNE ((MAS ((SG (DAT)))) (NTR ((SG (DAT)))))))))) . "AX-POS-EM")
    
    ((((KOM ((PRAEDIKATIV-GEBRAUCHT)))
       (POS
        ((OHNE
          ((MAS ((SG (NOM)) (PL (GEN)))) (FEM ((SG (GEN DAT)) (PL (GEN))))
                                         (NTR ((PL (GEN))))))
         (UNBESTIMMT ((MAS ((SG (NOM)))))))))) . "AX-POS-ER-COMP-NULL")
    
    ((((KOM ((PRAEDIKATIV-GEBRAUCHT))))) . "AX-COMP-NULL") ; 91
    ((((KOM
        ((OHNE
          ((MAS ((PL (NOM AKK)))) (FEM ((SG (NOM AKK)) (PL (NOM AKK))))
           (NTR ((PL (NOM AKK))))))
         (BESTIMMT
          ((MAS ((SG (NOM)))) (FEM ((SG (NOM AKK)))) (NTR ((SG (NOM AKK))))))
         (UNBESTIMMT ((FEM ((SG (NOM AKK)))))))))) . "AX-COMP-E") ; 61
    ((((KOM
        ((OHNE
          ((MAS ((SG (GEN AKK)) (PL (DAT)))) (FEM ((PL (DAT))))
           (NTR ((SG (GEN)) (PL (DAT))))))
         (BESTIMMT
          ((MAS ((SG (GEN DAT AKK)) (PL (NOM GEN DAT AKK))))
           (FEM ((SG (GEN DAT)) (PL (NOM GEN DAT AKK))))
           (NTR ((SG (GEN DAT)) (PL (NOM GEN DAT AKK))))))
         (UNBESTIMMT
          ((MAS ((SG (GEN DAT AKK)) (PL (NOM GEN DAT AKK))))
           (FEM ((SG (GEN DAT)) (PL (NOM GEN DAT AKK))))
           (NTR
            ((SG (GEN DAT))
             (PL (NOM GEN DAT AKK)))))))))) . "AX-COMP-EN") ; 62
    ((((KOM
        ((OHNE ((NTR ((SG (NOM AKK))))))
         (UNBESTIMMT ((NTR ((SG (NOM AKK)))))))))) . "AX-COMP-ES") ; 63
    ((((KOM
        ((OHNE
          ((MAS ((SG (NOM)) (PL (GEN)))) (FEM ((SG (GEN DAT)) (PL (GEN))))
           (NTR ((PL (GEN))))))
         (UNBESTIMMT ((MAS ((SG (NOM)))))))))) . "AX-COMP-ER") ; 64
    ((((KOM
        ((OHNE
          ((MAS ((SG (DAT)))) (NTR ((SG (DAT)))))))))) . "AX-COMP-EM") ; 65
    ((((SUP
        ((OHNE
          ((MAS ((PL (NOM AKK)))) (FEM ((SG (NOM AKK)) (PL (NOM AKK))))
           (NTR ((PL (NOM AKK))))))
         (BESTIMMT
          ((MAS ((SG (NOM)))) (FEM ((SG (NOM AKK)))) (NTR ((SG (NOM AKK))))))
         (UNBESTIMMT ((FEM ((SG (NOM AKK)))))))))) . "AX-SUPER-E") ; 66
    ((((SUP
        ((OHNE
          ((MAS ((SG (GEN AKK)) (PL (DAT)))) (FEM ((PL (DAT))))
           (NTR ((SG (GEN)) (PL (DAT))))))
         (BESTIMMT
          ((MAS ((SG (GEN DAT AKK)) (PL (NOM GEN DAT AKK))))
           (FEM ((SG (GEN DAT)) (PL (NOM GEN DAT AKK))))
           (NTR ((SG (GEN DAT)) (PL (NOM GEN DAT AKK))))))
         (UNBESTIMMT
          ((MAS ((SG (GEN DAT AKK)) (PL (NOM GEN DAT AKK))))
           (FEM ((SG (GEN DAT)) (PL (NOM GEN DAT AKK))))
           (NTR
            ((SG (GEN DAT))
             (PL (NOM GEN DAT AKK)))))))))) . "AX-SUPER-EN") ; 67
    ((((SUP
        ((OHNE ((NTR ((SG (NOM AKK))))))
         (UNBESTIMMT ((NTR ((SG (NOM AKK)))))))))) . "AX-SUPER-ES") ; 68
    ((((SUP
        ((OHNE
          ((MAS ((SG (NOM)) (PL (GEN)))) (FEM ((SG (GEN DAT)) (PL (GEN))))
           (NTR ((PL (GEN))))))
         (UNBESTIMMT ((MAS ((SG (NOM)))))))))) . "AX-SUPER-ER") ; 69
    ((((SUP
        ((OHNE
          ((MAS ((SG (DAT)))) (NTR ((SG (DAT)))))))))) . "AX-SUPER-EM") ; 70
    ((((POS ((PRAEDIKATIV-GEBRAUCHT)))
       (POS
        ((OHNE
          ((MAS ((PL (NOM AKK)))) (FEM ((SG (NOM AKK)) (PL (NOM AKK))))
           (NTR ((PL (NOM AKK))))))
         (BESTIMMT
          ((MAS ((SG (NOM)))) (FEM ((SG (NOM AKK)))) (NTR ((SG (NOM AKK))))))
         (UNBESTIMMT ((FEM ((SG (NOM AKK)))))))))) . 
     "AX-POS-E-NULL")            ; "IRRE" 76

    ((((SUP ((PRAEDIKATIV-GEBRAUCHT))))) . :wrong)
    ))

(SETQ *PARTICPLE-PERFECT-ATTR-TYPES*
  '(
    
    #|
    ((((POS
        ((OHNE
          ((MAS ((SG (NOM)) (PL (GEN)))) (FEM ((SG (GEN DAT)) (PL (GEN))))
           (NTR ((PL (GEN))))))
           (UNBESTIMMT ((MAS ((SG (NOM)))))))))) . "VX-PZP-PERF-POS-ER") ; 49
           |#
    ((((POS
     ((OHNE
       ((MAS ((PL (NOM AKK)))) (FEM ((SG (NOM AKK)) (PL (NOM AKK))))
        (NTR ((PL (NOM AKK))))))
      (BESTIMMT
       ((MAS ((SG (NOM)))) (FEM ((SG (NOM AKK)))) (NTR ((SG (NOM AKK))))))
      (UNBESTIMMT ((FEM ((SG (NOM AKK)))))))))) . "VX-PZP-PERF-POS-E")
    ((((POS
     ((OHNE
       ((MAS ((SG (GEN AKK)) (PL (DAT)))) (FEM ((PL (DAT))))
        (NTR ((SG (GEN)) (PL (DAT))))))
      (BESTIMMT
       ((MAS ((SG (GEN DAT AKK)) (PL (NOM GEN DAT AKK))))
        (FEM ((SG (GEN DAT)) (PL (NOM GEN DAT AKK))))
        (NTR ((SG (GEN DAT)) (PL (NOM GEN DAT AKK))))))
      (UNBESTIMMT
       ((MAS ((SG (GEN DAT AKK)) (PL (NOM GEN DAT AKK))))
        (FEM ((SG (GEN DAT)) (PL (NOM GEN DAT AKK))))
        (NTR ((SG (GEN DAT)) (PL (NOM GEN DAT AKK)))))))))) . 
     "VX-PZP-PERF-POS-EN")
    ((((POS
     ((OHNE ((NTR ((SG (NOM AKK))))))
      (UNBESTIMMT ((NTR ((SG (NOM AKK)))))))))) . "VX-PZP-PERF-POS-ES")
    ((((POS ((OHNE ((MAS ((SG (DAT)))) (NTR ((SG (DAT)))))))))) . 
     "VX-PZP-PERF-POS-EM")
    
    ((((KOM ((PRAEDIKATIV-GEBRAUCHT)))
       (POS
        ((OHNE
          ((MAS ((SG (NOM)) (PL (GEN)))) (FEM ((SG (GEN DAT)) (PL (GEN))))
                                         (NTR ((PL (GEN))))))
         (UNBESTIMMT ((MAS ((SG (NOM)))))))))) . 
     "VX-PZP-PERF-POS-ER-COMP-NULL")
    
    ;;((((KOM ((PRAEDIKATIV-GEBRAUCHT))))) . "AX-COMP-NULL") ; 91
    ((((KOM
        ((OHNE
          ((MAS ((PL (NOM AKK)))) (FEM ((SG (NOM AKK)) (PL (NOM AKK))))
           (NTR ((PL (NOM AKK))))))
         (BESTIMMT
          ((MAS ((SG (NOM)))) (FEM ((SG (NOM AKK)))) (NTR ((SG (NOM AKK))))))
         (UNBESTIMMT ((FEM ((SG (NOM AKK)))))))))) . 
     "VX-PZP-PERF-COMP-E")      ; 61
    ((((KOM
        ((OHNE
          ((MAS ((SG (GEN AKK)) (PL (DAT)))) (FEM ((PL (DAT))))
           (NTR ((SG (GEN)) (PL (DAT))))))
         (BESTIMMT
          ((MAS ((SG (GEN DAT AKK)) (PL (NOM GEN DAT AKK))))
           (FEM ((SG (GEN DAT)) (PL (NOM GEN DAT AKK))))
           (NTR ((SG (GEN DAT)) (PL (NOM GEN DAT AKK))))))
         (UNBESTIMMT
          ((MAS ((SG (GEN DAT AKK)) (PL (NOM GEN DAT AKK))))
           (FEM ((SG (GEN DAT)) (PL (NOM GEN DAT AKK))))
           (NTR
            ((SG (GEN DAT))
             (PL (NOM GEN DAT AKK)))))))))) . 
     "VX-PZP-PERF-COMP-EN")     ; 62
    ((((KOM
        ((OHNE ((NTR ((SG (NOM AKK))))))
         (UNBESTIMMT ((NTR ((SG (NOM AKK)))))))))) . 
     "VX-PZP-PERF-COMP-ES")     ; 63
    ((((KOM
        ((OHNE
          ((MAS ((SG (NOM)) (PL (GEN)))) (FEM ((SG (GEN DAT)) (PL (GEN))))
           (NTR ((PL (GEN))))))
         (UNBESTIMMT ((MAS ((SG (NOM)))))))))) . 
     "VX-PZP-PERF-COMP-ER")     ; 64
    ((((KOM
        ((OHNE
          ((MAS ((SG (DAT)))) (NTR ((SG (DAT)))))))))) . 
     "VX-PZP-PERF-COMP-EM")     ; 65
    ((((SUP
        ((OHNE
          ((MAS ((PL (NOM AKK)))) (FEM ((SG (NOM AKK)) (PL (NOM AKK))))
           (NTR ((PL (NOM AKK))))))
         (BESTIMMT
          ((MAS ((SG (NOM)))) (FEM ((SG (NOM AKK)))) (NTR ((SG (NOM AKK))))))
         (UNBESTIMMT ((FEM ((SG (NOM AKK)))))))))) . 
     "VX-PZP-PERF-SUPER-E")     ; 66
    ((((SUP
        ((OHNE
          ((MAS ((SG (GEN AKK)) (PL (DAT)))) (FEM ((PL (DAT))))
           (NTR ((SG (GEN)) (PL (DAT))))))
         (BESTIMMT
          ((MAS ((SG (GEN DAT AKK)) (PL (NOM GEN DAT AKK))))
           (FEM ((SG (GEN DAT)) (PL (NOM GEN DAT AKK))))
           (NTR ((SG (GEN DAT)) (PL (NOM GEN DAT AKK))))))
         (UNBESTIMMT
          ((MAS ((SG (GEN DAT AKK)) (PL (NOM GEN DAT AKK))))
           (FEM ((SG (GEN DAT)) (PL (NOM GEN DAT AKK))))
           (NTR
            ((SG (GEN DAT))
             (PL (NOM GEN DAT AKK)))))))))) . "VX-PZP-PERF-SUPER-EN") ; 67
    ((((SUP
        ((OHNE ((NTR ((SG (NOM AKK))))))
         (UNBESTIMMT ((NTR ((SG (NOM AKK)))))))))) . 
     "VX-PZP-PERF-SUPER-ES")    ; 68
    ((((SUP
        ((OHNE
          ((MAS ((SG (NOM)) (PL (GEN)))) (FEM ((SG (GEN DAT)) (PL (GEN))))
           (NTR ((PL (GEN))))))
         (UNBESTIMMT ((MAS ((SG (NOM)))))))))) . "VX-PZP-PERF-SUPER-ER") ; 69
    ((((SUP
        ((OHNE
          ((MAS ((SG (DAT)))) (NTR ((SG (DAT)))))))))) . 
     "VX-PZP-PERF-SUPER-EM")    ; 70
    
    ((((SUP ((PRAEDIKATIV-GEBRAUCHT))))) . :wrong)
    ))

(SETQ *PARTICPLE-PRAESENS-ATTR-TYPES*
  '(

    ((((POS
        ((OHNE
          ((MAS ((SG (NOM)) (PL (GEN)))) (FEM ((SG (GEN DAT)) (PL (GEN))))
           (NTR ((PL (GEN))))))
         (UNBESTIMMT ((MAS ((SG (NOM)))))))))) . "VX-PZP-BARE-ER") ; 49
    ;; OBIGES IST RICHTIG, WIRD ABER NICT ERZEUGT, DAFUER ABER DAS FALSCHE
    ((((KOM ((PRAEDIKATIV-GEBRAUCHT)))
       (POS
        ((OHNE
          ((MAS ((SG (NOM)) (PL (GEN)))) (FEM ((SG (GEN DAT)) (PL (GEN))))
                                         (NTR ((PL (GEN))))))
         (UNBESTIMMT ((MAS ((SG (NOM)))))))))) . "VX-PZP-BARE-ER")
    
    ((((POS
     ((OHNE
       ((MAS ((PL (NOM AKK)))) (FEM ((SG (NOM AKK)) (PL (NOM AKK))))
        (NTR ((PL (NOM AKK))))))
      (BESTIMMT
       ((MAS ((SG (NOM)))) (FEM ((SG (NOM AKK)))) (NTR ((SG (NOM AKK))))))
      (UNBESTIMMT ((FEM ((SG (NOM AKK)))))))))) . "VX-PZP-BARE-E")
    ((((POS
     ((OHNE
       ((MAS ((SG (GEN AKK)) (PL (DAT)))) (FEM ((PL (DAT))))
        (NTR ((SG (GEN)) (PL (DAT))))))
      (BESTIMMT
       ((MAS ((SG (GEN DAT AKK)) (PL (NOM GEN DAT AKK))))
        (FEM ((SG (GEN DAT)) (PL (NOM GEN DAT AKK))))
        (NTR ((SG (GEN DAT)) (PL (NOM GEN DAT AKK))))))
      (UNBESTIMMT
       ((MAS ((SG (GEN DAT AKK)) (PL (NOM GEN DAT AKK))))
        (FEM ((SG (GEN DAT)) (PL (NOM GEN DAT AKK))))
        (NTR ((SG (GEN DAT)) (PL (NOM GEN DAT AKK)))))))))) . 
     "VX-PZP-BARE-EN")
    ((((POS
     ((OHNE ((NTR ((SG (NOM AKK))))))
      (UNBESTIMMT ((NTR ((SG (NOM AKK)))))))))) . "VX-PZP-BARE-ES")
    ((((POS ((OHNE ((MAS ((SG (DAT)))) (NTR ((SG (DAT)))))))))) . 
     "VX-PZP-BARE-EM")

    ))

(SETQ *PARTICPLE-PRAESENS-MIT-ZU-TYPES*
  '(

    ((((POS
        ((OHNE
          ((MAS ((SG (NOM)) (PL (GEN)))) (FEM ((SG (GEN DAT)) (PL (GEN))))
           (NTR ((PL (GEN))))))
         (UNBESTIMMT ((MAS ((SG (NOM)))))))))) . "VX-PZP-ZU-ER") ; 49
    ;; OBIGES IST RICHTIG, WIRD ABER NICT ERZEUGT, DAFUER ABER DAS FALSCHE
    ((((KOM ((PRAEDIKATIV-GEBRAUCHT)))
       (POS
        ((OHNE
          ((MAS ((SG (NOM)) (PL (GEN)))) (FEM ((SG (GEN DAT)) (PL (GEN))))
                                         (NTR ((PL (GEN))))))
         (UNBESTIMMT ((MAS ((SG (NOM)))))))))) . "VX-PZP-ZU-ER")
    ((((POS
     ((OHNE
       ((MAS ((PL (NOM AKK)))) (FEM ((SG (NOM AKK)) (PL (NOM AKK))))
        (NTR ((PL (NOM AKK))))))
      (BESTIMMT
       ((MAS ((SG (NOM)))) (FEM ((SG (NOM AKK)))) (NTR ((SG (NOM AKK))))))
      (UNBESTIMMT ((FEM ((SG (NOM AKK)))))))))) . "VX-PZP-ZU-E")
    ((((POS
     ((OHNE
       ((MAS ((SG (GEN AKK)) (PL (DAT)))) (FEM ((PL (DAT))))
        (NTR ((SG (GEN)) (PL (DAT))))))
      (BESTIMMT
       ((MAS ((SG (GEN DAT AKK)) (PL (NOM GEN DAT AKK))))
        (FEM ((SG (GEN DAT)) (PL (NOM GEN DAT AKK))))
        (NTR ((SG (GEN DAT)) (PL (NOM GEN DAT AKK))))))
      (UNBESTIMMT
       ((MAS ((SG (GEN DAT AKK)) (PL (NOM GEN DAT AKK))))
        (FEM ((SG (GEN DAT)) (PL (NOM GEN DAT AKK))))
        (NTR ((SG (GEN DAT)) (PL (NOM GEN DAT AKK)))))))))) . 
     "VX-PZP-ZU-EN")
    ((((POS
     ((OHNE ((NTR ((SG (NOM AKK))))))
      (UNBESTIMMT ((NTR ((SG (NOM AKK)))))))))) . "VX-PZP-ZU-ES")
    ((((POS ((OHNE ((MAS ((SG (DAT)))) (NTR ((SG (DAT)))))))))) . 
     "VX-PZP-ZU-EM")

    )) 

(SETQ *ORDINAL-TYPES*
      '(
        ((((PRAEDIKATIV-GEBRAUCHT))) . "DX-INFL-NULL")
        ((((OHNE
            ((MAS ((SG (NOM)) (PL (GEN)))) (FEM ((SG (GEN DAT)) (PL (GEN))))
                                           (NTR ((PL (GEN))))))
           (UNBESTIMMT ((MAS ((SG (NOM)))))))) . "DX-INFL-ER") 
        ((((OHNE
            ((MAS ((PL (NOM AKK)))) (FEM ((SG (NOM AKK)) (PL (NOM AKK))))
                                    (NTR ((PL (NOM AKK))))))
           (BESTIMMT
            ((MAS ((SG (NOM)))) (FEM ((SG (NOM AKK)))) (NTR ((SG (NOM AKK))))))
           (UNBESTIMMT ((FEM ((SG (NOM AKK)))))))) . 
         "DX-INFL-E")
        ((((OHNE
            ((MAS ((SG (GEN AKK)) (PL (DAT)))) (FEM ((PL (DAT))))
                                               (NTR ((SG (GEN)) (PL (DAT))))))
           (BESTIMMT
            ((MAS ((SG (GEN DAT AKK)) (PL (NOM GEN DAT AKK))))
             (FEM ((SG (GEN DAT)) (PL (NOM GEN DAT AKK))))
             (NTR ((SG (GEN DAT)) (PL (NOM GEN DAT AKK))))))
           (UNBESTIMMT
            ((MAS ((SG (GEN DAT AKK)) (PL (NOM GEN DAT AKK))))
             (FEM ((SG (GEN DAT)) (PL (NOM GEN DAT AKK))))
             (NTR ((SG (GEN DAT)) (PL (NOM GEN DAT AKK)))))))) . 
         "DX-INFL-EN")
        ((((OHNE ((NTR ((SG (NOM AKK))))))
           (UNBESTIMMT ((NTR ((SG (NOM AKK)))))))) . 
         "DX-INFL-ES")
        ((((OHNE ((MAS ((SG (DAT)))) (NTR ((SG (DAT)))))))) . 
         "DX-INFL-EM")
        ))

;; neu kn ax-pos -> dx-ein
(SETQ *POSSESSIV-TYPES*
      '(
        ((((ARTIKELWORT ((MAS ((SG (NOM)))) (NTR ((SG (NOM AKK)))))))) .
        "DX-EIN-NULL")
        ((((ARTIKELWORT ((MAS ((SG (GEN)))) (NTR ((SG (GEN))))))
           (SUBSTANTIVWORT ((OHNE ((NTR ((SG (NOM GEN AKK)))))))))) .
         "DX-EIN-ES")
        ((((SUBSTANTIVWORT
            ((OHNE
              ((MAS ((SG (NOM)) (PL (GEN)))) (FEM ((SG (GEN DAT)) (PL (GEN))))
                                             (NTR ((PL (GEN))))))))
           (ARTIKELWORT
            ((MAS ((PL (GEN)))) (FEM ((SG (GEN DAT)) (PL (GEN))))
                                (NTR ((PL (GEN)))))))) . "DX-EIN-ER")
        ((((SUBSTANTIVWORT ((OHNE ((MAS ((SG (DAT)))) (NTR ((SG (DAT))))))))
           (ARTIKELWORT ((MAS ((SG (DAT)))) (NTR ((SG (DAT)))))))) . 
         "DX-EIN-EM")
                                                                       
        ((((SUBSTANTIVWORT
            ((OHNE
              ((MAS ((PL (NOM AKK)))) (FEM ((SG (NOM AKK)) (PL (NOM AKK))))
                                      (NTR ((PL (NOM AKK))))))
             (BESTIMMT
              ((MAS ((SG (NOM)))) (FEM ((SG (NOM AKK)))) 
                                  (NTR ((SG (NOM AKK))))))))
           (ARTIKELWORT
            ((MAS ((PL (NOM AKK)))) (FEM ((SG (NOM AKK)) (PL (NOM AKK))))
                                    (NTR ((PL (NOM AKK)))))))) . 
         "DX-EIN-E")
        ((((SUBSTANTIVWORT
            ((OHNE
              ((MAS ((SG (AKK)) (PL (DAT)))) (FEM ((PL (DAT)))) 
                                             (NTR ((PL (DAT))))))
             (BESTIMMT
              ((MAS ((SG (GEN DAT AKK)) (PL (NOM GEN DAT AKK))))
               (FEM ((SG (GEN DAT)) (PL (NOM GEN DAT AKK))))
               (NTR ((SG (GEN DAT)) (PL (NOM GEN DAT AKK))))))))
           (ARTIKELWORT
            ((MAS ((SG (AKK)) (PL (DAT)))) (FEM ((PL (DAT)))) 
                                           (NTR ((PL (DAT)))))))) .
         "DX-EIN-EN")
        ))

;; neu kn

(SETQ *DEF-DET-TYPES*
      '(
        ((((MAS ((SG (GEN)))) (NTR ((SG (NOM AKK GEN)))))).
         "DX-DEF-ES")
        ((((MAS ((SG (NOM)) (PL (GEN)))) 
                    (FEM ((SG (GEN DAT)) (PL (GEN)))) 
                    (NTR ((PL (GEN)))))) .
          "DX-DEF-ER")
        ((((MAS ((SG (DAT)))) (NTR ((SG (DAT)))))) . 
         "DX-DEF-EM")
        ((((MAS ((PL (NOM AKK)))) 
                    (FEM ((SG (NOM AKK)) (PL (NOM AKK)))) 
                    (NTR ((PL (NOM AKK)))))) . 
         "DX-DEF-E")
        ((((MAS ((SG (AKK)) (PL (DAT)))) 
                    (FEM ((PL (DAT)))) 
                    (NTR ((PL (DAT)))))) .
         "DX-DEF-EN")
        ((((NTR ((SG (NOM AKK)))))) .
         "DX-DEF-DAS")
        ((((MAS ((SG (GEN)))) (NTR ((SG (GEN)))))) .
         "DX-DEF-DES") 
        ((((MAS ((SG (NOM)))))) .
         "DX-DEF-DER-X-E")   ; derjenige/derselbe   
        ((((MAS ((PL (GEN))))
                    (FEM ((SG (GEN DAT)) (PL (GEN))))
                    (NTR ((PL (GEN)))))) .
         "DX-DEF-DER-X-EN")
        ((((FEM ((SG (NOM AKK)))))) .
         "DX-DEF-DIE-X-E")
        ((((MAS ((PL (NOM AKK))))
                    (FEM ((PL (NOM AKK))))
                    (NTR ((PL (NOM AKK)))))) .
         "DX-DEF-DIE-X-EN")   
        ))

(SETQ *INDEF-DET-TYPES*
      '(
        ((((MAS ((SG (NOM)))) (NTR ((SG (NOM AKK)))))).
         "DX-EIN-NULL")
        ((((MAS ((SG (GEN)))) (NTR ((SG (GEN)))))).
         "DX-EIN-ES")
        ((((MAS ((PL (GEN)))) 
           (FEM ((SG (GEN DAT)) (PL (GEN)))) 
           (NTR ((PL (GEN)))))) .
          "DX-EIN-ER")
        ((((MAS ((SG (DAT)))) (NTR ((SG (DAT)))))) . 
         "DX-EIN-EM")
        ((((MAS ((PL (NOM AKK)))) 
                    (FEM ((SG (NOM AKK)) (PL (NOM AKK)))) 
                    (NTR ((PL (NOM AKK)))))) . 
         "DX-EIN-E")
        ((((MAS ((SG (AKK)) (PL (DAT)))) 
                    (FEM ((PL (DAT)))) 
                    (NTR ((PL (DAT)))))) .
         "DX-EIN-EN")
        ))

(setq *cat2pos* 
  
  '((NOMEN . "NX")
    
    (VERB . "VX")
    (HILFSVERB . "AUX")
    (MODALVERB . "VMOD")
    (PARTIZIP-PERFEKT . "VPPERF")
    (PARTIZIP-PRAESENS . "VPPRES")
    (PARTIZIP-PRAESENS-MIT-ZU . "VPZU")
    
    (ADJEKTIV . "ADJ")
    ;; falsch da nicht zugreifbar
    (ATTRIBUTIV-GEBRAUCHTES . "ADJ")
    
    (DETERMINATIV  . "DET")
    (DETERMINATIV-INDEF . "INDEF")
    
    (RELATIVPRONOMEN . "RELPRON")
    (PERSONALPRONOMEN . "PERSPRON")
    (REFLEXIVPRONOMEN . "REFPRON")
    (POSSESSIVPRONOMEN . "POSSPRON")
    (INTERROGATIVPRONOMEN . "WHPRON")
    
    (ORDINALZAHL . "ORDINAL")
    (PRAEPOSITION . "PREP")
    (INTERPUNKTION . "S-SIGN")
    (VERBZUSATZ . "VPREF")
    (ADVERB . "ADV")
    (KOORD-KONJUNKTION . "COORD")
    (SUBORD-KONJUNKTION . "SUBORD")
    (FRAGEADVERB . "WHADV")
    (KARDINALZAHL . "CARDINAL")))

(defun get-all-types ()
  (mapcar #'rest (append *NOUN-TYPES* 
                         *VERB-TYPES* 
                         *ADJ-TYPES* 
                         *PARTICPLE-PERFECT-ATTR-TYPES* 
                         *PARTICPLE-PRAESENS-ATTR-TYPES* 
                         *PARTICPLE-PRAESENS-MIT-ZU-TYPES* 
                         *ORDINAL-TYPES* 
                         *POSSESSIV-TYPES*
                         *DEF-DET-TYPES*
                         *INDEF-DET-TYPES*
                         *SPECIAL-TYPES*)))


;;; wo die sind die satzzeichen?

(defun return-types (morphix-result &optional (with-cat? t))
  (loop for reading in morphix-result
        for cat = (unless (stringp (second reading))
                    (first (last (assoc 'WORTART (rest reading)))))
      collect
        (let ((type-info
               (case cat
                 ((NOMEN) (rest (assoc (rest (assoc 'flexion (rest reading)))
                                             *noun-types*
                                             :test #'equal)))
                 ((VERB MODALVERB HILFSVERB)
                  (rest (assoc (rest (assoc 'flexion (rest reading)))
                                     *verb-types*
                                     :test #'equal)))
                 ((ADJEKTIV)
                  (rest (assoc (rest (assoc 'flexion (rest reading)))
                                     *adj-types*
                                     :test #'equal)))
                 ((PARTIZIP-PERFEKT)
                  (rest (assoc (rest (assoc 'flexion (rest reading)))
                                     *particple-perfect-attr-types*
                                     :test #'equal)))
                 ((PARTIZIP-PRAESENS)
                  (rest (assoc (rest (assoc 'flexion (rest reading)))
                                     *particple-praesens-attr-types*
                                     :test #'equal)))

                 ((PARTIZIP-PRAESENS-MIT-ZU)
                  (rest (assoc (rest (assoc 'flexion (rest reading)))
                                     *particple-praesens-mit-zu-types*
                                     :test #'equal)))
                 
                 ((PERSONALPRONOMEN REFLEXIVPRONOMEN
                   INTERROGATIVPRONOMEN PARTIKEL
                   KOORD-KONJUNKTION SUBORD-KONJUNKTION RELATIVPRONOMEN)
                  "PX-ALL")
                 ((PRAEPOSITION VERBZUSATZ)
                  "PX-ALL") ;; GN on: 17/6/: "PX-P-PRFX" to "PX-ALL"
                 ((FRAGEADVERB ADVERB)
                  "PX-ALL")    ;; GN on: 17/6/: "PX-ADV" to "PX-ALL"
                 ((INTERPUNKTION)
                  "INTERPUNKTION")
                 ((KARDINALZAHL)
                  "CARDINAL")
                 ((ORDINALZAHL)
                  (rest (assoc (rest (assoc 'flexion (rest reading)))
                                     *ordinal-types*
                                     :test #'equal)))
                 ;; neu kn                 
                 ((DETERMINATIV)
                  (rest (assoc (rest (assoc 'flexion (rest reading)))
                                     *def-det-types*
                                     :test #'equal)))
                 ((DETERMINATIV-INDEF)
                  (rest (assoc (rest (assoc 'flexion (rest reading)))
                                     *indef-det-types*
                                     :test #'equal)))
                 ((POSSESSIVPRONOMEN)
                  (rest (assoc (rest (assoc 'flexion (rest reading)))
                                     *possessiv-types*
                                     :test #'equal)))
                 (OTHERWISE :NO-CAT))))
          
          (cons 
           (first reading) 
           (if with-cat?
               (cons type-info 
                     (cat2pos cat))
             type-info)))))

(setf (get 'mapper  :morph-fct) #'return-types)
