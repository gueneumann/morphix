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

;; This is a version of the type interface that has been adapted
;; for a disjunction free grammar.
;; St. Mue. 5th June, 2000
;; GN, 10 June 2000: adapted output-function return-types

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
                        ("PX-PRFX" . "PX-PRFX")
                        ("PX-P-PRFX" . "PX-P-PRFX")))





(SETQ *NOUN-TYPES* 
  '(
    ;;; THE NOUN TYPES
    ((((FEM ((SG (NOM DAT AKK)))))) "NX-FEM-NDA-SG")   ; neu 29/8/94 GN

    ((((FEM ((SG (GEN)))))) "NX-FEM-G-SG")             ; neu 29/8/94 GN
    ((((FEM ((SG (NOM GEN DAT AKK)))))) "NX-FEM-SG")
    ((((FEM ((PL (NOM GEN DAT AKK)))))) "NX-FEM-PL")
    ((((NTR ((SG (NOM DAT AKK)))))) "NX-NEU-NDA-SG")
    ((((NTR ((SG (GEN)))))) "NX-NEU-G-SG")
    ((((NTR ((PL (NOM GEN AKK)))))) "NX-NEU-NGA-PL")
    ((((NTR ((PL (DAT)))))) "NX-NEU-D-PL")
;;  ((((NTR ((PL (NOM GEN AKK)) (SG (NOM DAT AKK))))))  "NX-NEU-NDA-SG-NGA-PL")
    ((((NTR ((PL (NOM GEN AKK)) (SG (NOM DAT AKK))))))  "NX-NEU-NDA-SG" "NX-NEU-NGA-PL")
    ((((MAS ((SG (NOM DAT AKK)))))) "NX-MAS-NDA-SG")
    ((((MAS ((SG (GEN)))))) "NX-MAS-G-SG")
;;  ((((MAS ((PL (NOM GEN AKK)) (SG (DAT)))))) "NX-MAS-D-SG-NGA-PL")
    ((((MAS ((PL (NOM GEN AKK)) (SG (DAT)))))) "NX-MAS-D-SG" "NX-MAS-NGA-PL")
    ((((MAS ((PL (DAT)))))) "NX-MAS-D-PL")
    ((((MAS ((PL (NOM GEN AKK)))))) "NX-MAS-NGA-PL")
;;  ((((NTR ((PL (NOM GEN AKK)) (SG (DAT)))))) "NX-NEU-D-SG-NGA-PL")
    ((((NTR ((PL (NOM GEN AKK)) (SG (DAT)))))) "NX-NEU-D-SG" "NX-NEU-NGA-PL")
;;  ((((NTR ((PL (NOM GEN DAT AKK)) (SG (NOM DAT AKK)))))) "NX-NEU-NDA-SG-PL")
    ((((NTR ((PL (NOM GEN DAT AKK)) (SG (NOM DAT AKK)))))) "NX-NEU-NDA-SG" "NX-NEU-PL")
    ((((MAS ((PL (NOM GEN AKK)) (SG (NOM DAT AKK)))))) "NX-MAS-NDA-SG" "NX-MAS-NGA-PL")
;;  ((((NTR ((PL (NOM GEN DAT AKK)) (SG (GEN)))))) "NX-NEU-G-SG-PL")
    ((((NTR ((PL (NOM GEN DAT AKK)) (SG (GEN)))))) "NX-NEU-G-SG" "NX-NEU-PL")
    ((((NTR ((PL (NOM GEN DAT AKK)))))) "NX-NEU-PL")
;;  ((((MAS ((PL (NOM GEN DAT AKK)) (SG (NOM DAT AKK)))))) "NX-MAS-NDA-SG-PL")
    ((((MAS ((PL (NOM GEN DAT AKK)) (SG (NOM DAT AKK)))))) "NX-MAS-NDA-SG" "NX-MAS-PL")
    ((((MAS ((SG (NOM)))))) "NX-MAS-N-SG")
;;  ((((MAS ((PL (NOM GEN DAT AKK)) (SG (GEN DAT AKK)))))) "NX-MAS-GDA-SG-PL")
    ((((MAS ((PL (NOM GEN DAT AKK)) (SG (GEN DAT AKK)))))) "NX-MAS-GDA-SG" "NX-MAS-PL")
    ((((FEM ((PL (NOM GEN AKK)))))) "NX-FEM-NGA-PL")
    ((((FEM ((PL (DAT)))))) "NX-FEM-D-PL")
    ((((MAS ((PL (NOM GEN DAT AKK)))))) "NX-MAS-PL")
    ((((MAS ((SG (DAT)))))) "NX-MAS-D-SG")
    ((((NTR ((SG (DAT)))))) "NX-NEU-D-SG")
;;  ((((MAS ((PL (NOM GEN DAT AKK)) (SG (GEN)))))) "NX-MAS-G-SG-PL")
    ((((MAS ((PL (NOM GEN DAT AKK)) (SG (GEN)))))) "NX-MAS-G-SG" "NX-MAS-PL")
    ((((MAS ((SG (GEN DAT AKK)))))) "NX-MAS-GDA-SG")
    ((((NIL ((PL (NOM GEN DAT AKK)))))) "NX-PL")
    ((((FEM ((PL (NOM GEN AKK)) (SG (NOM GEN DAT AKK)))))) :wrong)
    ((((FEM ((PL (NOM GEN DAT AKK)) (SG (NOM GEN DAT AKK)))))) "NX-FEM-SG-PL")
    ((((NTR
         ((PL (NOM GEN DAT AKK)) (SG (NOM GEN DAT AKK)))))) "NX-NEU-SG-PL")
    ((((NTR ((SG (NOM GEN DAT AKK)))))) "NX-NEU-SG")
    ((((MAS ((PL (NOM GEN AKK)) (SG (NOM GEN DAT AKK)))))) 
     ;;     "NX-MAS-SG-NGA-PL"
     :wrong)
    ((((MAS ((SG (NOM GEN DAT AKK)))))) "NX-MAS-SG")
    ((((MAS ((SG (DAT AKK)))))) "NX-MAS-DA-SG")
    ((((FEM ((SG (NOM)))))) :wrong)
    ((((FEM
         ((PL (NOM GEN DAT AKK))
          (SG (GEN DAT AKK)))))) :NX-ERROR)
    ((((NTR ((SG (GEN DAT AKK)))))) "NX-NEU-GDA-SG")
    ((((NIL ((PL (NOM GEN AKK)))))) "NX-NAG-PL")
    ((((NIL ((PL (DAT)))))) "NX-DAT-PL")
;;  ((((MAS ((PL (NOM GEN DAT AKK)) (SG (DAT AKK)))))) "NX-MAS-DA-SG-PL")
    ((((MAS ((PL (NOM GEN DAT AKK)) (SG (DAT AKK)))))) "NX-MAS-DA-SG" "NX-MAS-PL")
    ((((NTR ((SG (NOM AKK)))))) "NX-NEU-NA-SG")
;;  ((((NTR ((PL (NOM GEN DAT AKK)) (SG (DAT)))))) "NX-NEU-D-SG-PL")
    ((((NTR ((PL (NOM GEN DAT AKK)) (SG (DAT)))))) "NX-NEU-D-SG" "NX-NEU-PL")
    ((((MAS ((PL (NOM GEN DAT AKK)) (SG (NOM GEN DAT AKK)))))) "NX-MAS-SG-PL")
    ((((NTR ((SG (NOM)))))) "NX-NEU-N-SG")
    ((((NTR
         ((PL (NOM GEN DAT AKK))
          (SG (GEN DAT AKK)))))) 
     :wrong)
    ))


(SETQ *VERB-TYPES* 
  '(
    ;;; THE VERB TYPES
   
    ((((PRAESENS ((SG (ANREDE)) (PL (1 3 ANREDE))))
       (KONJUNKTIV-1 ((SG (ANREDE)) (PL (1 3 ANREDE)))) (INFINITIV)
       (IMPERATIV (ANREDE)))) 
     "VX-SUP-BARE" "VX-PRES-PL-1-3") ; 8
    (((((A1 A2 A3 A4) (PRAESENS ((SG (ANREDE)) (PL (1 3 ANREDE))))
     (KONJUNKTIV-1 ((SG (ANREDE)) (PL (1 3 ANREDE)))) (INFINITIV)
     (IMPERATIV (ANREDE))))) 
     "VX-SUP-BARE" "VX-PRES-PL-1-3") ; 8
    ((((PARTIZIP-PERFEKT) (PRAESENS ((SG (3)) (PL (2))))
       (IMPERATIV (PL))))
     "VX-SUP-PERF" "VX-PRES-IND-SG-3" "VX-PRES-IND-IMP-PL-2") ; 9
    ((((PARTIZIP-PERFEKT) (PRAESENS ((SG (3)) (PL (2))))
       (KONJUNKTIV-1 ((PL (2))))))
     "VX-SUP-PERF" "VX-PRES-IND-SG-3" "VX-PRES-IND-CONJ-IMP-PL-2")
    ((((PRAESENS ((SG (1)))) (KONJUNKTIV-1 ((SG (1 3))))
       (IMPERATIV (SG)))) 
     "VX-PRES-SG-1" "VX-PRES-SG-CONJ-3" "VX-PRES-SG-IMP-2") ; 10
    ((((PRAESENS ((SG (2)))))) "VX-PRES-IND-SG-2") ; 11
    ((((KONJUNKTIV-1 ((SG (2)))))) "VX-PRES-CONJ-SG-2") ; 12
    ((((KONJUNKTIV-1 ((PL (2)))))) "VX-PRES-CONJ-PL-2") ; 13
    ((((IMPERFEKT ((SG (1 3)))) (KONJUNKTIV-2 ((SG (1 3)))))) 
     "VX-PAST-SG-1-3")            ; 14
    ((((IMPERFEKT ((SG (2)))) (KONJUNKTIV-2 ((SG (2)))))) 
     "VX-PAST-SG-2")            ;16
    ((((IMPERFEKT ((SG (ANREDE)) (PL (1 3 ANREDE))))
       (KONJUNKTIV-2 ((SG (ANREDE)) (PL (1 3 ANREDE)))))) 
     "VX-PAST-PL-1-3")            ; 18
    ((((IMPERFEKT ((PL (2)))) (KONJUNKTIV-2 ((PL (2)))))) 
     "VX-PAST-PL-2")            ;20
    ((((PARTIZIP-PRAESENS))) 
     "VX-PZP-BARE-NULL")        ; 21
    ((((ERWEITERTER-INFINITIV))) 
     "VX-SUP-ZU")               ;25
    ((((PRAESENS ((SG (ANREDE)) (PL (1 3 ANREDE))))
       (KONJUNKTIV-1 ((SG (ANREDE)) (PL (1 3 ANREDE))))
       (INFINITIV))) 
     "VX-SUP-BARE" "VX-PRES-PL-1-3") ;26
    ((((PRAESENS ((PL (2)))) (IMPERATIV (PL))
       (KONJUNKTIV-1 ((PL (2)))))) 
     "VX-PRES-IND-CONJ-IMP-PL-2") ;27
    ((((PRAESENS ((SG (3)))) (IMPERATIV (SG)))) 
     "VX-PRES-IND-SG-3" "VX-PRES-IMP-SG-2")  ;28
    ((((PRAESENS ((SG (1)))) (KONJUNKTIV-1 ((SG (1 3)))))) 
      "VX-PRES-SG-1" "VX-PRES-SG-CONJ-3")      ;29
    ((((PRAESENS ((SG (3)))))) 
     "VX-PRES-IND-SG-3")        ;31
    ((((PRAESENS ((PL (2)))) (KONJUNKTIV-1 ((PL (2)))))) 
     "VX-PRES-PL-2")            ;33
    ((((IMPERFEKT ((SG (1 3)))))) 
     "VX-PAST-IND-SG-1-3")      ;34
    ((((IMPERFEKT ((SG (2)))))) 
     "VX-PAST-IND-SG-2")        ; 35
    ((((IMPERFEKT ((SG (ANREDE)) (PL (1 3 ANREDE)))))) 
     "VX-PAST-IND-PL-1-3")        ;36
    ((((IMPERFEKT ((PL (2)))))) 
     "VX-PAST-IND-PL-2")        ; 37
    ((((KONJUNKTIV-2 ((SG (1 3)))))) 
     "VX-PAST-CONJ-SG-1-3")     ;38
    ((((KONJUNKTIV-2 ((SG (2)))))) 
     "VX-PAST-CONJ-SG-2")       ;39
    ((((KONJUNKTIV-2 ((SG (ANREDE)) (PL (1 3 ANREDE)))))) 
     "VX-PAST-CONJ-PL-1-3")       ;40
    ((((KONJUNKTIV-2 ((PL (2)))))) 
     "VX-PAST-CONJ-PL-2")       ; 41
    ((((PARTIZIP-PRAESENS-MIT-ZU))) :WRONG) ; 42
    ((((PARTIZIP-PERFEKT))) 
     "VX-SUP-PERF")             ;43
    ((((PRAESENS ((SG (3)) (PL (2)))) (IMPERATIV (PL)))) 
     "VX-PRES-IND-SG-3" "VX-PRES-IND-IMP-PL-2") ;52
    ((((PRAESENS ((SG (3)) (PL (2)))))) 
     "VX-PRES-IND-SG-3" "VXPRES-IND-PL-2")   ;53
    ((((PRAESENS ((SG (3)) (PL (2)))) (KONJUNKTIV-1 ((SG (3)) (PL (2))))
       (IMPERATIV (PL)))) 
     "VX-PRES-IND-SG-3" "VX-PRES-IND-CONJ-IMP-PL-2")  ;54
    ((((A1 (PRAESENS ((SG (3)) (PL (2)))) (IMPERATIV (PL)))
       (A3 (PRAESENS ((PL (2)))) (IMPERATIV (PL)))
       (A4 (PRAESENS ((SG (2 3)) (PL (2)))) (IMPERATIV (PL)))
       (A5 (PRAESENS ((SG (3)) (PL (2)))) (KONJUNKTIV-1 ((SG (3)) (PL (2))))
           (IMPERATIV (PL)))
       (A6 (PRAESENS ((SG (3)) (PL (2)))) (KONJUNKTIV-1 ((SG (3)) (PL (2))))
           (IMPERATIV (PL))))) 
     "VX-PRES-IND-SG-3" "VX-PRES-IND-CONJ-IMP-PL-2")  ;54
    ((((PRAESENS ((SG (2)))) (KONJUNKTIV-1 ((SG (2)))))) 
     "VX-PRES-SG-2")            ;55
    ((((PRAESENS ((PL (2)))) (IMPERATIV (PL)))) 
     "VX-PRES-IND-IMP-PL-2")    ;56
    ((((PRAESENS ((SG (2 3)))))) 
     "VX-PRES-IND-SG-2-3")      ;57
    ((((PRAESENS ((PL (2)))))) 
     "VX-PRES-IND-PL-2")        ;58
    ((((IMPERATIV (SG)))) 
     "VX-IMP-SG-2")             ;71
    ((((PRAESENS ((SG (3)) (PL (2))))
       (KONJUNKTIV-1 ((SG (3)) (PL (2)))))) 
     "VX-PRES-SG-3" "VX-PRES-PL-2")       ;72
    ((((PARTIZIP-PERFEKT) (PRAESENS ((SG (2 3)) (PL (2))))
       (IMPERATIV (PL)))) 
     "VX-SUP-PERF" "VX-PRES-IND-SG-2-3" "VX-PRES-IND-IMP-PL-2") ;73
    ((((PRAESENS ((SG (2 3)) (PL (2)))) (IMPERATIV (PL)))) 
     "VX-PRES-IND-SG-2-3" "VX-PRES-IND-IMP-PL-2") ;74
    ((((PARTIZIP-PERFEKT) (PRAESENS ((SG (3)) (PL (2))))
       (KONJUNKTIV-1 ((SG (3)) (PL (2)))) (IMPERATIV (PL)))) 
     "VX-SUP-PERF" "VX-PRES-IND-SG-3" "VX-PRES-IND-CONJ-IMP-PL-2")                      ;77
    ((((PARTIZIP-PERFEKT) (PRAESENS ((SG (3)) (PL (2)))) (IMPERATIV (PL))
       (KONJUNKTIV-1 ((PL (2)))))) 
     "VX-SUP-PERF" "VX-PRES-IND-SG-3" "VX-PRES-IND-CONJ-IMP-PL-2") ;78
    ((((PARTIZIP-PERFEKT) (PRAESENS ((SG (3)) (PL (2)))))) 
     "VX-SUP-PERF" "VX-PRES-IND-SG-3" "VX-PRES-IND-PL-2") ; 79
    ((((PRAESENS ((SG (3)) (PL (2)))) (IMPERATIV (PL))
       (KONJUNKTIV-1 ((PL (2)))))) 
     "VX-PRES-IND-SG-3" "VX-PRES-IND-CONJ-IMP-PL-2") ;80
    ((((PRAESENS ((SG (3)) (PL (2))))
       (KONJUNKTIV-1 ((PL (2)))))) 
     "VX-PRES-PL-2" "VX-PRES-IND-SG-3")   ;81
    ((((PRAESENS ((SG (2 3)) (PL (2)))))) 
     "VX-PRES-IND-SG-2-3" "VX-PRES-IND-PL-2") ; 83
    ((((PARTIZIP-PERFEKT) (PRAESENS ((SG (ANREDE)) (PL (1 3 ANREDE))))
       (KONJUNKTIV-1 ((SG (ANREDE)) (PL (1 3 ANREDE))))
       (INFINITIV))) 
     "VX-SUP-BARE-PERF" "VX-PRES-PL-1-3") ; 92
    
    ((((PARTIZIP-PERFEKT) (PRAESENS ((SG (ANREDE)) (PL (1 3 ANREDE))))
       (KONJUNKTIV-1 ((SG (ANREDE)) (PL (1 3 ANREDE)))) (INFINITIV)
       (IMPERATIV (ANREDE)))) 
     "VX-SUP-BARE-PERF" "VX-PRES-PL-1-3")                          ;93

    ((((PARTIZIP-PERFEKT)
    ((A1 A2 A3 A4) (PRAESENS ((SG (ANREDE)) (PL (1 3 ANREDE))))
     (KONJUNKTIV-1 ((SG (ANREDE)) (PL (1 3 ANREDE)))) (INFINITIV)
     (IMPERATIV (ANREDE))))) 
     "VX-SUP-BARE-PERF" "VX-PRES-PL-1-3")                          ;93

    ((((PRAESENS ((SG (3)) (PL (2)))) (KONJUNKTIV-1 ((PL (2))))
       (IMPERATIV (PL)))) "VX-PRES-SG-3-IND-CONJ-IMP-PL-2") ; 95
    ((((PARTIZIP-PERFEKT)
       (IMPERFEKT ((SG (ANREDE)) (PL (1 3 ANREDE)))))) 
     "VX-SUP-PERF" "VX-PAST-IND-PL-1-3") ;96
    ((((PRAESENS ((SG (1 3)))))) 
     "VX-PRES-IND-SG-1-3")      ; 97
    ((((KONJUNKTIV-1 ((SG (3)) (PL (2)))))) 
     ;; "VX-PRES-CONJ-SG-3-PL-2"  ; 98, Morphix-Fehler, Typ kann am Ende
     ;; eliminiert werden; relevante Beispiele wasche/waschet
        "VX-PRES-CONJ-PL-2")  ; 98
    ((((PARTIZIP-PERFEKT) (PRAESENS ((PL (2))))
       (IMPERATIV (PL)))) :wrong) ;100
    ((((PARTIZIP-PERFEKT) (IMPERFEKT ((SG (ANREDE)) (PL (1 3 ANREDE))))
       (KONJUNKTIV-2 ((SG (ANREDE)) (PL (1 3 ANREDE)))))) 
     "VX-SUP-PERF" "VX-PAST-PL-1-3")   ;101
    ((((KONJUNKTIV-1 ((SG (1 3)))) (IMPERATIV (SG)))) 
     "VX-PRES-CONJ-SG-1-3" "VX-PRES-IMP-SG-2") ; 119
    ((((PARTIZIP-PERFEKT) (PRAESENS ((SG (3)) (PL (2))))
       (KONJUNKTIV-1 ((SG (3)) (PL (2)))))) 
     "VX-SUP-PERF" "VX-PRES-SG-3-PL-2") ;120
    ((((KONJUNKTIV-1 ((SG (1 3)))))) 
     "VX-PRES-CONJ-SG-1-3")     ; 124
    ((((INFINITIV))) "VX-SUP-BARE") ; 129
    ((((PARTIZIP-PERFEKT)
       (PRAESENS ((SG (2 3)) (PL (2)))))) :wrong) ;133
    ((((PARTIZIP-PERFEKT) (PRAESENS ((SG (3)) (PL (2))))
       (KONJUNKTIV-1 ((PL (2)))))) :wrong) ; 134
    ((((PARTIZIP-PERFEKT) (KONJUNKTIV-1 ((PL (2)))))) 
     "VX-SUP-PERF" "VX-PRES-CONJ-PL-2") ;135
    ((((KONJUNKTIV-1 ((SG (ANREDE)) (PL (1 3 ANREDE))))
       (IMPERATIV (ANREDE)))) 
     "VX-PRES-CONJ-PL-1-3 ")    ;148
    ((((PRAESENS ((SG (1)))))) 
     "VX-PRES-IND-SG-1")        ;149
    ((((PRAESENS ((SG (ANREDE)) (PL (1 3 ANREDE)))))) 
     "VX-PRES-IND-PL-1-3")       ;150
    ((((PARTIZIP-PERFEKT) (PRAESENS ((PL (2)))) (IMPERATIV (PL))
       (KONJUNKTIV-1 ((PL (2)))))) :wrong) ;151
    ((((PRAESENS ((SG (2 3)))) (IMPERATIV (SG)))) :wrong) ;152
    ((((KONJUNKTIV-1
        ((SG (ANREDE)) (PL (1 3 ANREDE)))))) 
     "VX-PRES-CONJ-PL-1-3")     ;160
      
    ))

(SETQ *ADJ-TYPES*
  '(
    ((((POS ((PRAEDIKATIV-GEBRAUCHT))))) "AX-POS-NULL") ; 48
    ((((POS
        ((OHNE
          ((MAS ((SG (NOM)) (PL (GEN)))) (FEM ((SG (GEN DAT)) (PL (GEN))))
           (NTR ((PL (GEN))))))
         (UNBESTIMMT ((MAS ((SG (NOM)))))))))) 
        "AX-POS-ER-nom-mas-sg-agr"
        "ax-pos-er-dg-fem-sg-agr"
        "ax-pos-er-gen-pl-agr"
        ) ; 49
    ((((POS
     ((OHNE
       ((MAS ((PL (NOM AKK)))) (FEM ((SG (NOM AKK)) (PL (NOM AKK))))
        (NTR ((PL (NOM AKK))))))
      (BESTIMMT
       ((MAS ((SG (NOM)))) (FEM ((SG (NOM AKK)))) (NTR ((SG (NOM AKK))))))
      (UNBESTIMMT ((FEM ((SG (NOM AKK)))))))))) 
        "ax-pos-e-agr-e-type-na-fem-sg-agr"
        "ax-pos-e-agr-e-type-na-pl-agr"
        "ax-pos-e-agr-e-type-na-fn-sg-agr"
        "ax-pos-e-agr-e-type-nom-mas-sg-agr")
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
        (NTR ((SG (GEN DAT)) (PL (NOM GEN DAT AKK)))))))))) 
        "ax-pos-en-agr-en-type-gen-mn-sg-agr"
        "ax-pos-en-agr-en-type-dat-pl-agr"
        "ax-pos-en-agr-en-type-dg-sg-agr"
        "ax-pos-en-agr-en-type-pl-type"
        "ax-pos-en-agr-en-type-acc-mas-sg-agr")
    ((((POS
     ((OHNE ((NTR ((SG (NOM AKK))))))
      (UNBESTIMMT ((NTR ((SG (NOM AKK)))))))))) "AX-POS-ES")
    ((((POS ((OHNE ((MAS ((SG (DAT)))) (NTR ((SG (DAT)))))))))) "AX-POS-EM")
    
    ((((KOM ((PRAEDIKATIV-GEBRAUCHT)))
       (POS
        ((OHNE
          ((MAS ((SG (NOM)) (PL (GEN)))) (FEM ((SG (GEN DAT)) (PL (GEN))))
                                         (NTR ((PL (GEN))))))
         (UNBESTIMMT ((MAS ((SG (NOM))))))))))
         "AX-POS-ER" "AX-COMP-NULL")
    
    ((((KOM ((PRAEDIKATIV-GEBRAUCHT))))) "AX-COMP-NULL") ; 91
    ((((KOM
        ((OHNE
          ((MAS ((PL (NOM AKK)))) (FEM ((SG (NOM AKK)) (PL (NOM AKK))))
           (NTR ((PL (NOM AKK))))))
         (BESTIMMT
          ((MAS ((SG (NOM)))) (FEM ((SG (NOM AKK)))) (NTR ((SG (NOM AKK))))))
         (UNBESTIMMT ((FEM ((SG (NOM AKK)))))))))) 
         "ax-comp-e-agr-e-type-na-fem-sg-agr"
         "ax-comp-e-agr-e-type-na-pl-agr"
         "ax-comp-e-agr-e-type-na-fn-sg-agr"
         "ax-comp-e-agr-e-type-nom-mas-sg-agr" ) ; 61
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
             (PL (NOM GEN DAT AKK)))))))))) 
        "ax-comp-en-agr-en-type-gen-mn-sg-agr"
        "ax-comp-en-agr-en-type-dat-pl-agr"
        "ax-comp-en-agr-en-type-dg-sg-agr"
        "ax-comp-en-agr-en-type-pl-type"
        "ax-comp-en-agr-en-type-acc-mas-sg-agr") ; 62
    ((((KOM
        ((OHNE ((NTR ((SG (NOM AKK))))))
         (UNBESTIMMT ((NTR ((SG (NOM AKK)))))))))) "AX-COMP-ES") ; 63
    ((((KOM
        ((OHNE
          ((MAS ((SG (NOM)) (PL (GEN)))) (FEM ((SG (GEN DAT)) (PL (GEN))))
           (NTR ((PL (GEN))))))
         (UNBESTIMMT ((MAS ((SG (NOM)))))))))) 
         "ax-comp-er-agr-er-type-nom-mas-sg-agr"
         "ax-comp-er-agr-er-type-dg-fem-sg-agr" 
         "ax-comp-er-agr-er-type-gen-pl-agr") ; 64
    ((((KOM
        ((OHNE
          ((MAS ((SG (DAT)))) (NTR ((SG (DAT)))))))))) "AX-COMP-EM") ; 65
    ((((SUP
        ((OHNE
          ((MAS ((PL (NOM AKK)))) (FEM ((SG (NOM AKK)) (PL (NOM AKK))))
           (NTR ((PL (NOM AKK))))))
         (BESTIMMT
          ((MAS ((SG (NOM)))) (FEM ((SG (NOM AKK)))) (NTR ((SG (NOM AKK))))))
         (UNBESTIMMT ((FEM ((SG (NOM AKK)))))))))) 
        "ax-super-e-agr-e-type-na-fem-sg-agr"
        "ax-super-e-agr-e-type-na-pl-agr"
        "ax-super-e-agr-e-type-na-fn-sg-agr"
        "ax-super-e-agr-e-type-nom-mas-sg-agr" ) ; 66
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
             (PL (NOM GEN DAT AKK)))))))))) 
       "ax-super-en-agr-en-type-gen-mn-sg-agr"
       "ax-super-en-agr-en-type-dat-pl-agr"
       "ax-super-en-agr-en-type-dg-sg-agr"
       "ax-super-en-agr-en-type-pl-type") ; 67
    ((((SUP
        ((OHNE ((NTR ((SG (NOM AKK))))))
         (UNBESTIMMT ((NTR ((SG (NOM AKK)))))))))) "AX-SUPER-ES") ; 68
    ((((SUP
        ((OHNE
          ((MAS ((SG (NOM)) (PL (GEN)))) (FEM ((SG (GEN DAT)) (PL (GEN))))
           (NTR ((PL (GEN))))))
         (UNBESTIMMT ((MAS ((SG (NOM)))))))))) 
      "ax-super-er-agr-er-type-nom-mas-sg-agr"
      "ax-super-er-agr-er-type-dg-fem-sg-agr"
      "ax-super-er-agr-er-type-gen-pl-agr" ) ; 69
    ((((SUP
        ((OHNE
          ((MAS ((SG (DAT)))) (NTR ((SG (DAT)))))))))) "AX-SUPER-EM") ; 70
    ((((POS ((PRAEDIKATIV-GEBRAUCHT)))
       (POS
        ((OHNE
          ((MAS ((PL (NOM AKK)))) (FEM ((SG (NOM AKK)) (PL (NOM AKK))))
           (NTR ((PL (NOM AKK))))))
         (BESTIMMT
          ((MAS ((SG (NOM)))) (FEM ((SG (NOM AKK)))) (NTR ((SG (NOM AKK))))))
         (UNBESTIMMT ((FEM ((SG (NOM AKK)))))))))) 
     "ax-pos-null"
     "ax-pos-e-agr-e-type-na-fem-sg-agr"
     "ax-pos-e-agr-e-type-na-pl-agr"
     "ax-pos-e-agr-e-type-na-fn-sg-agr"
     "ax-pos-e-agr-e-type-nom-mas-sg-agr")            ; "IRRE" 76

    ((((SUP ((PRAEDIKATIV-GEBRAUCHT))))) :wrong)
    ))

(SETQ *PARTICPLE-PERFECT-ATTR-TYPES*
  '(
    
    #|
    ((((POS
        ((OHNE
          ((MAS ((SG (NOM)) (PL (GEN)))) (FEM ((SG (GEN DAT)) (PL (GEN))))
           (NTR ((PL (GEN))))))
           (UNBESTIMMT ((MAS ((SG (NOM)))))))))) "VX-PZP-PERF-POS-ER") ; 49
           |#
    ((((POS
     ((OHNE
       ((MAS ((PL (NOM AKK)))) (FEM ((SG (NOM AKK)) (PL (NOM AKK))))
        (NTR ((PL (NOM AKK))))))
      (BESTIMMT
       ((MAS ((SG (NOM)))) (FEM ((SG (NOM AKK)))) (NTR ((SG (NOM AKK))))))
      (UNBESTIMMT ((FEM ((SG (NOM AKK)))))))))) 
      "vx-pzp-perf-pos-e-agr-e-type-na-fem-sg-agr"
      "vx-pzp-perf-pos-e-agr-e-type-na-pl-agr"
      "vx-pzp-perf-pos-e-agr-e-type-na-fn-sg-agr"
      "vx-pzp-perf-pos-e-agr-e-type-nom-mas-sg-agr")
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
        (NTR ((SG (GEN DAT)) (PL (NOM GEN DAT AKK)))))))))) 
     "vx-pzp-perf-pos-en-agr-en-type-gen-mn-sg-agr"
     "vx-pzp-perf-pos-en-agr-en-type-dat-pl-agr"
     "vx-pzp-perf-pos-en-agr-en-type-dg-sg-agr"
     "vx-pzp-perf-pos-en-agr-en-type-pl-type"
     "vx-pzp-perf-pos-en-agr-en-type-acc-mas-sg-agr")
    ((((POS
     ((OHNE ((NTR ((SG (NOM AKK))))))
      (UNBESTIMMT ((NTR ((SG (NOM AKK)))))))))) "VX-PZP-PERF-POS-ES")
    ((((POS ((OHNE ((MAS ((SG (DAT)))) (NTR ((SG (DAT)))))))))) 
     "VX-PZP-PERF-POS-EM")
    
    ((((KOM ((PRAEDIKATIV-GEBRAUCHT)))
       (POS
        ((OHNE
          ((MAS ((SG (NOM)) (PL (GEN)))) (FEM ((SG (GEN DAT)) (PL (GEN))))
                                         (NTR ((PL (GEN))))))
         (UNBESTIMMT ((MAS ((SG (NOM)))))))))) 
     "VX-PZP-PERF-POS-ER-COMP-NULL")
    
    ;;((((KOM ((PRAEDIKATIV-GEBRAUCHT))))) "AX-COMP-NULL") ; 91
    ((((KOM
        ((OHNE
          ((MAS ((PL (NOM AKK)))) (FEM ((SG (NOM AKK)) (PL (NOM AKK))))
           (NTR ((PL (NOM AKK))))))
         (BESTIMMT
          ((MAS ((SG (NOM)))) (FEM ((SG (NOM AKK)))) (NTR ((SG (NOM AKK))))))
         (UNBESTIMMT ((FEM ((SG (NOM AKK)))))))))) 
     "vx-pzp-perf-comp-e-agr-e-type-na-fem-sg-agr"
     "vx-pzp-perf-comp-e-agr-e-type-na-pl-agr"
     "vx-pzp-perf-comp-e-agr-e-type-na-fn-sg-agr"
     "vx-pzp-perf-comp-e-agr-e-type-nom-mas-sg-agr"
)      ; 61
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
             (PL (NOM GEN DAT AKK)))))))))) 
     "vx-pzp-perf-comp-en-agr-en-type-gen-mn-sg-agr"
     "vx-pzp-perf-comp-en-agr-en-type-dat-pl-agr"
     "vx-pzp-perf-comp-en-agr-en-type-dg-sg-agr"
     "vx-pzp-perf-comp-en-agr-en-type-pl-type"
     "vx-pzp-perf-comp-en-agr-en-type-acc-mas-sg-agr"
)     ; 62
    ((((KOM
        ((OHNE ((NTR ((SG (NOM AKK))))))
         (UNBESTIMMT ((NTR ((SG (NOM AKK)))))))))) 
     "VX-PZP-PERF-COMP-ES")     ; 63
    ((((KOM
        ((OHNE
          ((MAS ((SG (NOM)) (PL (GEN)))) (FEM ((SG (GEN DAT)) (PL (GEN))))
           (NTR ((PL (GEN))))))
         (UNBESTIMMT ((MAS ((SG (NOM)))))))))) 
     "vx-pzp-perf-comp-er-agr-er-type-nom-mas-sg-agr"
     "vx-pzp-perf-comp-er-agr-er-type-dg-fem-sg-agr"
     "vx-pzp-perf-comp-er-agr-er-type-gen-pl-agr"
)     ; 64
    ((((KOM
        ((OHNE
          ((MAS ((SG (DAT)))) (NTR ((SG (DAT)))))))))) 
     "VX-PZP-PERF-COMP-EM")     ; 65
    ((((SUP
        ((OHNE
          ((MAS ((PL (NOM AKK)))) (FEM ((SG (NOM AKK)) (PL (NOM AKK))))
           (NTR ((PL (NOM AKK))))))
         (BESTIMMT
          ((MAS ((SG (NOM)))) (FEM ((SG (NOM AKK)))) (NTR ((SG (NOM AKK))))))
         (UNBESTIMMT ((FEM ((SG (NOM AKK)))))))))) 
        "vx-pzp-perf-super-e-agr-e-type-na-fem-sg-agr"
        "vx-pzp-perf-super-e-agr-e-type-na-pl-agr"
        "vx-pzp-perf-super-e-agr-e-type-na-fn-sg-agr"
        "vx-pzp-perf-super-e-agr-e-type-nom-mas-sg-agr"
)     ; 66
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
             (PL (NOM GEN DAT AKK)))))))))) 
     "vx-pzp-perf-super-en-agr-en-type-gen-mn-sg-agr"
     "vx-pzp-perf-super-en-agr-en-type-dat-pl-agr"
     "vx-pzp-perf-super-en-agr-en-type-dg-sg-agr"
     "vx-pzp-perf-super-en-agr-en-type-pl-type"
     "vx-pzp-perf-super-en-agr-en-type-acc-mas-sg-agr"
) ; 67
    ((((SUP
        ((OHNE ((NTR ((SG (NOM AKK))))))
         (UNBESTIMMT ((NTR ((SG (NOM AKK)))))))))) 
     "VX-PZP-PERF-SUPER-ES")    ; 68
    ((((SUP
        ((OHNE
          ((MAS ((SG (NOM)) (PL (GEN)))) (FEM ((SG (GEN DAT)) (PL (GEN))))
           (NTR ((PL (GEN))))))
         (UNBESTIMMT ((MAS ((SG (NOM)))))))))) 
     "vx-pzp-perf-super-er-agr-er-type-nom-mas-sg-agr"
     "vx-pzp-perf-super-er-agr-er-type-dg-fem-sg-agr"
     "vx-pzp-perf-super-er-agr-er-type-gen-pl-agr"
) ; 69
    ((((SUP
        ((OHNE
          ((MAS ((SG (DAT)))) (NTR ((SG (DAT)))))))))) 
     "VX-PZP-PERF-SUPER-EM")    ; 70
    
    ((((SUP ((PRAEDIKATIV-GEBRAUCHT))))) :wrong)
    ))

(SETQ *PARTICPLE-PRAESENS-ATTR-TYPES*
  '(

    ((((POS
        ((OHNE
          ((MAS ((SG (NOM)) (PL (GEN)))) (FEM ((SG (GEN DAT)) (PL (GEN))))
           (NTR ((PL (GEN))))))
         (UNBESTIMMT ((MAS ((SG (NOM)))))))))) 
        "vx-pzp-bare-er-agr-er-type-nom-mas-sg-agr"
        "vx-pzp-bare-er-agr-er-type-dg-fem-sg-agr"
        "vx-pzp-bare-er-agr-er-type-gen-pl-agr") ; 49
    ;; OBIGES IST RICHTIG, WIRD ABER NICT ERZEUGT, DAFUER ABER DAS FALSCHE
    ((((KOM ((PRAEDIKATIV-GEBRAUCHT)))
       (POS
        ((OHNE
          ((MAS ((SG (NOM)) (PL (GEN)))) (FEM ((SG (GEN DAT)) (PL (GEN))))
                                         (NTR ((PL (GEN))))))
         (UNBESTIMMT ((MAS ((SG (NOM)))))))))) 
        "vx-pzp-bare-er-agr-er-type-nom-mas-sg-agr"
        "vx-pzp-bare-er-agr-er-type-dg-fem-sg-agr"
        "vx-pzp-bare-er-agr-er-type-gen-pl-agr")
    ((((POS
     ((OHNE
       ((MAS ((PL (NOM AKK)))) (FEM ((SG (NOM AKK)) (PL (NOM AKK))))
        (NTR ((PL (NOM AKK))))))
      (BESTIMMT
       ((MAS ((SG (NOM)))) (FEM ((SG (NOM AKK)))) (NTR ((SG (NOM AKK))))))
      (UNBESTIMMT ((FEM ((SG (NOM AKK)))))))))) 
      "vx-pzp-bare-e-agr-e-type-na-fem-sg-agr"
      "vx-pzp-bare-e-agr-e-type-na-pl-agr"
      "vx-pzp-bare-e-agr-e-type-na-fn-sg-agr"
      "vx-pzp-bare-e-agr-e-type-nom-mas-sg-agr")
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
        (NTR ((SG (GEN DAT)) (PL (NOM GEN DAT AKK)))))))))) 
     "vx-pzp-bare-en-agr-en-type-gen-mn-sg-agr"
     "vx-pzp-bare-en-agr-en-type-dat-pl-agr"
     "vx-pzp-bare-en-agr-en-type-dg-sg-agr"
     "vx-pzp-bare-en-agr-en-type-pl-type"
     "vx-pzp-bare-en-agr-en-type-acc-mas-sg-agr"
)
    ((((POS
     ((OHNE ((NTR ((SG (NOM AKK))))))
      (UNBESTIMMT ((NTR ((SG (NOM AKK)))))))))) "VX-PZP-BARE-ES")
    ((((POS ((OHNE ((MAS ((SG (DAT)))) (NTR ((SG (DAT)))))))))) 
     "VX-PZP-BARE-EM")
        ;;((((KOM ((PRAEDIKATIV-GEBRAUCHT))))) "AX-COMP-NULL") ; 91
    ((((KOM
        ((OHNE
          ((MAS ((PL (NOM AKK)))) (FEM ((SG (NOM AKK)) (PL (NOM AKK))))
           (NTR ((PL (NOM AKK))))))
         (BESTIMMT
          ((MAS ((SG (NOM)))) (FEM ((SG (NOM AKK)))) (NTR ((SG (NOM AKK))))))
         (UNBESTIMMT ((FEM ((SG (NOM AKK)))))))))) 
         "vx-pzp-bare-comp-e-agr-e-type-na-fem-sg-agr"
         "vx-pzp-bare-comp-e-agr-e-type-na-pl-agr"
         "vx-pzp-bare-comp-e-agr-e-type-na-fn-sg-agr"
         "vx-pzp-bare-comp-e-agr-e-type-nom-mas-sg-agr"
)      ; 61
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
             (PL (NOM GEN DAT AKK))))))))))
             "vx-pzp-bare-comp-en-agr-en-type-gen-mn-sg-agr"
             "vx-pzp-bare-comp-en-agr-en-type-dat-pl-agr"
             "vx-pzp-bare-comp-en-agr-en-type-dg-sg-agr"
             "vx-pzp-bare-comp-en-agr-en-type-pl-type"
             "vx-pzp-bare-comp-en-agr-en-type-acc-mas-sg-agr")     ; 62
    ((((KOM
        ((OHNE ((NTR ((SG (NOM AKK))))))
         (UNBESTIMMT ((NTR ((SG (NOM AKK)))))))))) 
     "VX-PZP-BARE-COMP-ES")     ; 63
    ((((KOM
        ((OHNE
          ((MAS ((SG (NOM)) (PL (GEN)))) (FEM ((SG (GEN DAT)) (PL (GEN))))
           (NTR ((PL (GEN))))))
         (UNBESTIMMT ((MAS ((SG (NOM)))))))))) 
         "vx-pzp-bare-comp-er-agr-er-type-nom-mas-sg-agr"
         "vx-pzp-bare-comp-er-agr-er-type-dg-fem-sg-agr"
         "vx-pzp-bare-comp-er-agr-er-type-gen-pl-agr")     ; 64
    ((((KOM
        ((OHNE
          ((MAS ((SG (DAT)))) (NTR ((SG (DAT)))))))))) 
     "VX-PZP-BARE-COMP-EM")     ; 65
    ((((SUP
        ((OHNE
          ((MAS ((PL (NOM AKK)))) (FEM ((SG (NOM AKK)) (PL (NOM AKK))))
           (NTR ((PL (NOM AKK))))))
         (BESTIMMT
          ((MAS ((SG (NOM)))) (FEM ((SG (NOM AKK)))) (NTR ((SG (NOM AKK))))))
         (UNBESTIMMT ((FEM ((SG (NOM AKK))))))))))
         "vx-pzp-bare-super-e-agr-e-type-na-fem-sg-agr"
         "vx-pzp-bare-super-e-agr-e-type-na-pl-agr"
         "vx-pzp-bare-super-e-agr-e-type-na-fn-sg-agr"
         "vx-pzp-bare-super-e-agr-e-type-nom-mas-sg-agr")     ; 66
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
             (PL (NOM GEN DAT AKK)))))))))) 
             "vx-pzp-bare-super-en-agr-en-type-gen-mn-sg-agr"
             "vx-pzp-bare-super-en-agr-en-type-dat-pl-agr"
             "vx-pzp-bare-super-en-agr-en-type-dg-sg-agr"
             "vx-pzp-bare-super-en-agr-en-type-pl-type"
             "vx-pzp-bare-super-en-agr-en-type-acc-mas-sg-agr") ; 67
    ((((SUP
        ((OHNE ((NTR ((SG (NOM AKK))))))
         (UNBESTIMMT ((NTR ((SG (NOM AKK)))))))))) 
     "VX-PZP-BARE-SUPER-ES")    ; 68
    ((((SUP
        ((OHNE
          ((MAS ((SG (NOM)) (PL (GEN)))) (FEM ((SG (GEN DAT)) (PL (GEN))))
           (NTR ((PL (GEN))))))
         (UNBESTIMMT ((MAS ((SG (NOM)))))))))) "VX-PZP-BARE-SUPER-ER") ; 69
    ((((SUP
        ((OHNE
          ((MAS ((SG (DAT)))) (NTR ((SG (DAT)))))))))) 
     "VX-PZP-BARE-SUPER-EM")    ; 70
    
    ((((SUP ((PRAEDIKATIV-GEBRAUCHT))))) :wrong)
    ))

(SETQ *PARTICPLE-PRAESENS-MIT-ZU-TYPES*
  '(

    ((((POS
        ((OHNE
          ((MAS ((SG (NOM)) (PL (GEN)))) (FEM ((SG (GEN DAT)) (PL (GEN))))
           (NTR ((PL (GEN))))))
         (UNBESTIMMT ((MAS ((SG (NOM)))))))))) 
         "vx-pzp-zu-er-agr-er-type-nom-mas-sg-agr"
         "vx-pzp-zu-er-agr-er-type-dg-fem-sg-agr"
         "vx-pzp-zu-er-agr-er-type-gen-pl-agr") ; 49
    ;; OBIGES IST RICHTIG, WIRD ABER NICT ERZEUGT, DAFUER ABER DAS FALSCHE
    ((((KOM ((PRAEDIKATIV-GEBRAUCHT)))
       (POS
        ((OHNE
          ((MAS ((SG (NOM)) (PL (GEN)))) (FEM ((SG (GEN DAT)) (PL (GEN))))
                                         (NTR ((PL (GEN))))))
         (UNBESTIMMT ((MAS ((SG (NOM)))))))))) 
         "vx-pzp-zu-er-agr-er-type-nom-mas-sg-agr"
         "vx-pzp-zu-er-agr-er-type-dg-fem-sg-agr"
         "vx-pzp-zu-er-agr-er-type-gen-pl-agr")
    ((((POS
     ((OHNE
       ((MAS ((PL (NOM AKK)))) (FEM ((SG (NOM AKK)) (PL (NOM AKK))))
        (NTR ((PL (NOM AKK))))))
      (BESTIMMT
       ((MAS ((SG (NOM)))) (FEM ((SG (NOM AKK)))) (NTR ((SG (NOM AKK))))))
      (UNBESTIMMT ((FEM ((SG (NOM AKK)))))))))) 
      "vx-pzp-zu-e-agr-er-type-nom-mas-sg-agr"
      "vx-pzp-zu-e-agr-er-type-dg-fem-sg-agr"
      "vx-pzp-zu-e-agr-er-type-gen-pl-agr")
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
        (NTR ((SG (GEN DAT)) (PL (NOM GEN DAT AKK)))))))))) 
        "vx-pzp-zu-en-agr-en-type-gen-mn-sg-agr"
        "vx-pzp-zu-en-agr-en-type-dat-pl-agr"
        "vx-pzp-zu-en-agr-en-type-dg-sg-agr"
        "vx-pzp-zu-en-agr-en-type-pl-type"
        "vx-pzp-zu-en-agr-en-type-acc-mas-sg-agr")
    ((((POS
     ((OHNE ((NTR ((SG (NOM AKK))))))
      (UNBESTIMMT ((NTR ((SG (NOM AKK)))))))))) "VX-PZP-ZU-ES")
    ((((POS ((OHNE ((MAS ((SG (DAT)))) (NTR ((SG (DAT)))))))))) 
     "VX-PZP-ZU-EM")

    )) 

(SETQ *ORDINAL-TYPES*
      '(
        ((((PRAEDIKATIV-GEBRAUCHT))) "DX-INFL-NULL")
        ((((OHNE
            ((MAS ((SG (NOM)) (PL (GEN)))) (FEM ((SG (GEN DAT)) (PL (GEN))))
                                           (NTR ((PL (GEN))))))
           (UNBESTIMMT ((MAS ((SG (NOM)))))))) 
           "dx-infl-er-er-nom-mas-sg-agr"
           "dx-infl-er-er-dg-fem-sg-agr"
           "dx-infl-er-er-gen-pl-agr") 
        ((((OHNE
            ((MAS ((PL (NOM AKK)))) (FEM ((SG (NOM AKK)) (PL (NOM AKK))))
                                    (NTR ((PL (NOM AKK))))))
           (BESTIMMT
            ((MAS ((SG (NOM)))) (FEM ((SG (NOM AKK)))) (NTR ((SG (NOM AKK))))))
           (UNBESTIMMT ((FEM ((SG (NOM AKK)))))))) 
          "dx-infl-e-agr-e-type-na-fem-sg-agr"
          "dx-infl-e-agr-e-type-na-pl-agr"
          "dx-infl-e-agr-e-type-na-fn-sg-agr"
          "dx-infl-e-agr-e-type-nom-mas-sg-agr")
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
             (NTR ((SG (GEN DAT)) (PL (NOM GEN DAT AKK)))))))) 
             "dx-infl-en-agr-en-type-gen-mn-sg-agr"
             "dx-infl-en-agr-en-type-dat-pl-agr"
             "dx-infl-en-agr-en-type-dg-sg-agr"
             "dx-infl-en-agr-en-type-pl-type"
             "dx-infl-en-agr-en-type-acc-mas-sg-agr")
        ((((OHNE ((NTR ((SG (NOM AKK))))))
           (UNBESTIMMT ((NTR ((SG (NOM AKK)))))))) 
         "DX-INFL-ES")
        ((((OHNE ((MAS ((SG (DAT)))) (NTR ((SG (DAT)))))))) 
         "DX-INFL-EM")
        ))

;; neu kn ax-pos -> dx-ein
(SETQ *POSSESSIV-TYPES*
      '(
        ((((ARTIKELWORT ((MAS ((SG (NOM)))) (NTR ((SG (NOM AKK)))))))) 
        "dx-ein-null-agr-null-type-nom-mas-sg-agr"
        "dx-ein-null-agr-null-type-na-neu-sg-agr"
        )
        ((((ARTIKELWORT ((MAS ((SG (GEN)))) (NTR ((SG (GEN))))))
           (SUBSTANTIVWORT ((OHNE ((NTR ((SG (NOM GEN AKK))))))))))
         "dx-ein-es-agr-es2-type"
         "dx-ein-es-agr-es1-type")
        ((((SUBSTANTIVWORT
            ((OHNE
              ((MAS ((SG (NOM)) (PL (GEN)))) (FEM ((SG (GEN DAT)) (PL (GEN))))
                                             (NTR ((PL (GEN))))))))
           (ARTIKELWORT
            ((MAS ((PL (GEN)))) (FEM ((SG (GEN DAT)) (PL (GEN))))
                                (NTR ((PL (GEN)))))))) 
          "dx-ein-er-agr-er3-type"
          "dx-ein-er-agr-er1-type")
        ((((SUBSTANTIVWORT ((OHNE ((MAS ((SG (DAT)))) (NTR ((SG (DAT))))))))
           (ARTIKELWORT ((MAS ((SG (DAT)))) (NTR ((SG (DAT)))))))) 
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
                                    (NTR ((PL (NOM AKK)))))))) 
         "dx-ein-e-agr-e-st-type-na-fem-sg-agr"
         "dx-ein-e-agr-e-st-type-na-pl-agr")
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
                                           (NTR ((PL (DAT))))))))
         "dx-ein-en-agr-en3-type-acc-mas-sg-agr"
         "dx-ein-en-agr-en3-type-dat-pl-agr")
        ((((SUBSTANTIVWORT ((OHNE ((NTR ((SG (NOM AKK))))))))
           (ARTIKELWORT ((MAS ((SG (GEN)))) (NTR ((SG (GEN))))))))
         "dx-ein-es-agr-es2-type"
         "dx-ein-es-agr-es1-type")
        ))

;; neu kn

(SETQ *DEF-DET-TYPES*
      '(
        ((((MAS ((SG (GEN)))) (NTR ((SG (NOM AKK GEN))))))
         "dx-def-es-agr-es-type-na-neu-sg-agr"
         "dx-def-es-agr-es-type-gen-mn-sg-agr")
        ((((MAS ((SG (NOM)) (PL (GEN)))) 
                    (FEM ((SG (GEN DAT)) (PL (GEN)))) 
                    (NTR ((PL (GEN))))))
        "dx-def-er-agr-er-type-nom-mas-sg-agr"
        "dx-def-er-agr-er-type-dg-fem-sg-agr"
        "dx-def-er-agr-er-type-gen-pl-agr")
        ((((MAS ((SG (DAT)))) (NTR ((SG (DAT)))))) 
         "DX-DEF-EM")
        ((((MAS ((PL (NOM AKK)))) 
                    (FEM ((SG (NOM AKK)) (PL (NOM AKK)))) 
                    (NTR ((PL (NOM AKK)))))) 
         "dx-def-e-agr-e-st-type-na-fem-sg-agr"
         "dx-def-e-agr-e-st-type-na-pl-agr")
        ((((MAS ((SG (AKK)) (PL (DAT)))) 
                    (FEM ((PL (DAT)))) 
                    (NTR ((PL (DAT))))))
                    "dx-def-en-agr-en3-type-acc-mas-sg-agr"
                    "dx-def-en-agr-en3-type-dat-pl-agr")
        ((((NTR ((SG (NOM AKK))))))
         "DX-DEF-DAS")
        ((((MAS ((SG (GEN)))) (NTR ((SG (GEN))))))
         "DX-DEF-DES") 
        ((((MAS ((SG (NOM))))))
         "DX-DEF-DER-X-E")   ; derjenige/derselbe   
        ((((MAS ((PL (GEN))))
                    (FEM ((SG (GEN DAT)) (PL (GEN))))
                    (NTR ((PL (GEN)))))) 
        "dx-def-der-x-en-agr-er3-type-dg-fem-sg-agr"
        "dx-def-der-x-en-agr-er3-type-gen-pl-agr")
        ((((FEM ((SG (NOM AKK))))))
         "DX-DEF-DIE-X-E")
        ((((MAS ((PL (NOM AKK))))
                    (FEM ((PL (NOM AKK))))
                    (NTR ((PL (NOM AKK))))))
         "DX-DEF-DIE-X-EN")   
        ))

(SETQ *INDEF-DET-TYPES*
      '(
        ((((MAS ((SG (NOM)))) (NTR ((SG (NOM AKK))))))
        "dx-ein-null-agr-null-type-nom-mas-sg-agr"
        "dx-ein-null-agr-null-type-na-neu-sg-agr")
        ((((MAS ((SG (GEN)))) (NTR ((SG (GEN))))))
         "dx-ein-es-agr-es2-type"
         "dx-ein-es-agr-es1-type")
        ((((MAS ((PL (GEN)))) 
           (FEM ((SG (GEN DAT)) (PL (GEN)))) 
           (NTR ((PL (GEN))))))
          "dx-ein-er-agr-er3-type"
          "dx-ein-er-agr-er1-type")
        ((((MAS ((SG (DAT)))) (NTR ((SG (DAT)))))) 
         "DX-EIN-EM")
        ((((MAS ((PL (NOM AKK)))) 
                    (FEM ((SG (NOM AKK)) (PL (NOM AKK)))) 
                    (NTR ((PL (NOM AKK)))))) 
         "dx-ein-e-agr-e-st-type-na-fem-sg-agr"
         "dx-ein-e-agr-e-st-type-na-pl-agr")
        ((((MAS ((SG (AKK)) (PL (DAT)))) 
                    (FEM ((PL (DAT)))) 
                    (NTR ((PL (DAT))))))
         "dx-ein-en-agr-en3-type-acc-mas-sg-agr"
         "dx-ein-en-agr-en3-type-dat-pl-agr")
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
      append
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
                 ((VERBZUSATZ) "PX-PRFX") ;; neu WK: 21/4/99
                 ((PRAEPOSITION) "PX-ALL")
; alt:            ((PRAEPOSITION VERBZUSATZ)
;                  "PX-ALL") ;; GN on: 17/6/: "PX-P-PRFX" to "PX-ALL"
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
          
                     
           (loop for type in type-info collect
               (cons 
                 (first reading) 
                 (if with-cat?
                    (cons type (cat2pos cat))
                    type))))))

(setf (get 'mapper  :morph-fct) #'return-types)

(defun set-types ()
  (setf (get 'mapper  :morph-fct) #'return-types))





