;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: COMMON-LISP-USER -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;        file: morphix-package
;;;      module: morphix
;;;     version: 3.0
;;;  written by: Guenter Neumann
;;; last update: 12/12/97
;;;  updated by:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; author            | date        | modification
;;; ------------------|-------------|------------------------------------------
;;;                   |             |
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copywrights by Guenter Neumann
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "COMMON-LISP-USER")

(defpackage "MORPHIX" 
  (:use "COMMON-LISP")
  (:nicknames "MO")
  (:export :NOM :GEN :DAT :AKK :MAS :FEM :NTR
           :SG :PL :2a :PPRF
           :WORTART :FLEXION :INTERPUNKTION
           :NOMEN :VERB :ADJEKTIV
           :POSSESSIVPRONOMEN :KARDINALZAHL
           :ORDINALZAHL :HILFSVERB :MODALVERB
           :INTERROGATIVPRONOMEN :PARTIZIP-PERFEKT
           :PARTIZIP-PRAESENS :ATTRIBUTIV-GEBRAUCHTES
           :PARTIZIP-PRAESENS-MIT-ZU
           :VERBZUSATZ :ADVERB :PARTIKEL
           :KOORD-KONJUNKTION :SUBORD-KONJUNKTION
           :PRAEPOSITION :RELATIVPRONOMEN :DETERMINATIV
           :REFLEXIVPRONOMEN :PERSONALPRONOMEN :FRAGEADVERB
           :DETERMINATIV-INDEF :SATZZEICHEN
           :POS :KOM :SUP :OHNE :BESTIMMT :UNBESTIMMT
           :PRAEDIKATIV-GEBRAUCHT :SUBSTANTIVWORT :ARTIKELWORT
           :PRAESENS :IMPERFEKT :FUTUR-1 :FUTUR-2
           :PERFEKT :PLUSQUAMPERFEKT :AKTIV :PASSIV
           :IMPERATIV :INDIKATIV :KONJUNKTIV :KONJUNKTIV-1
           :KONJUNKTIV-2  :INFINITIV :ERWEITERTER-INFINITIV :ANREDE
           :AUSRUFEZEICHEN :KOMMA :BINDESTRICH :PUNKT
           :DOPPELPUNKT :SEMIKOLON :FRAGEZEICHEN
           :FOLGEKASUS
		 ;;; ---------------------------
		 ;;; now some toplevel functions
           :W-A  :C-S :L-S :L-F :L-Z :RUECKSETZEN
           :verb-inflection :noun-inflection :adjective-inflection
           :possessive-inflection :perspron-inflection
           :reflexive-inflection :detdef-inflection :query-inflection
           :determiner-inflection :determiner-indef-inflection
           :ordinal-inflection
           
           :rt))

