(in-package "MORPHIX")

(l-s "hab" '((WORTART . hilfsverb) (VGFA . A1) (VTYP . 6) (VGFB B1 . "ha") (VGFC C4 . "hatte") (VGFD . "haette") (PPRF . "gehabt") (PAUX . "hab")))
(l-s "ha" '((STAMM "hab" . VGFB)))
(l-s "hatte" '((STAMM "hab" . VGFC)))
(l-s "haette" '((STAMM "hab" . VGFD)))

(l-s "werd" '(230000 (VGFB B5 . "wir")
	 (VGFC C4 . "wurde") (VGFD . "wuerde")
	 (PPRF (t . "geworden") (hilfsverb . "worden"))
	))
(l-s "wir" '((STAMM "werd" . VGFB)))
(l-s "wurde" '((STAMM "werd" . VGFC)))
(l-s "wuerde" '((STAMM "werd" . VGFD)))
(l-s "word" '((STAMM "werd" . PPRF)))


;; 11
(l-s "sei" '((WORTART . hilfsverb) (VTYP . 11) (VGFA . A7) (SOND1 . "bin") (SOND2 . "bist") (SOND3 . "ist") (SOND4 . "sind") (VGFC C1 . "war") (VGFD . "waere") (PPRF . "gewesen") (PAUX . "sei")))
(l-s "bin" '((STAMM "sei" . SOND1)))
(l-s "bist" '((STAMM "sei" . SOND2)))
(l-s "ist" '((STAMM "sei" . SOND3)))
(l-s "sind" '((STAMM "sei" . SOND4)))
(l-s "war" '((STAMM "sei" . VGFC)))
(l-s "waere" '((STAMM "sei" . VGFD)))
(l-s "wes" '((STAMM "sei" . PPRF)))
