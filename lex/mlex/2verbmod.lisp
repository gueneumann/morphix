(in-package "MORPHIX")

;; 1

(l-s "soll" '((WORTART . MODALVERB) (VTYP . 1) (VGFA . A3) (PAUX . "hab")
	      (PPRF . "gesollt")))


;; 8

(l-s "woll" '((WORTART . modalverb) (VTYP . 8) (VGFA . A1) (VGFB B3 . "will") (PPRF . "gewollt") (PAUX . "hab")))
(l-s "will" '((STAMM "woll" . VGFB)))


;; 9
(l-s "koenn" '((WORTART . modalverb) (VTYP . 9) (VGFA . A1) (VGFB B3 . "kann") (VGFC C4 . "konnte") (PPRF . "gekonnt") (PAUX . "hab")))
(l-s "konnte" '((STAMM "koenn" . VGFC)))
(l-s "konn" '((STAMM "koenn" . PPRF)))
(l-s "kann" '((STAMM "koenn" . VGFB)))
(l-s "duerf" '((WORTART . modalverb) (VTYP . 9) (VGFA . A1) (VGFB B3 . "darf") (VGFC C4 . "durfte") (PPRF . "gedurft") (PAUX . "hab")))
(l-s "durfte" '((STAMM "duerf" . VGFC)))
(l-s "durf" '((STAMM "duerf" . PPRF)))
(l-s "darf" '((STAMM "duerf" . VGFB)))
(l-s "muess" '((WORTART . modalverb) (VTYP . 9) (VGFA . A4) (VGFB B6 . "muss") (VGFC C4 . "musste") (PPRF . "gemusst") (PAUX . "hab")))
(l-s "musste" '((STAMM "muess" . VGFC)))
(l-s "muss" '((STAMM "muess" . VGFB)))

;; 10
(l-s "moeg" '((WORTART . modalverb) (VTYP . 10) (VGFA . A1) (VGFB B3 . "mag") (VGFC C4 . "mochte") (VGFD . "moechte") (PPRF . "gemocht") (PAUX . "hab")))
(l-s "moechte" '((STAMM "moeg" . VGFD)))
(l-s "mochte" '((STAMM "moeg" . VGFC)))
(l-s "mag" '((STAMM "moeg" . VGFB)))
(l-s "moch" '((STAMM "moeg" . PPRF)))
(l-s "wiss" '((WORTART . modalverb) (VTYP . 10) (VGFA . A4) (VGFB B6 . "weiss") (VGFC C4 . "wusste") (VGFD . "wuesste") (PPRF . "gewusst") (PAUX . "hab")))
(l-s "wuesste" '((STAMM "wiss" . VGFD)))
(l-s "wusste" '((STAMM "wiss" . VGFC)))
(l-s "weiss" '((STAMM "wiss" . VGFB)))
(l-s "wuss" '((STAMM "wiss" . PPRF)))
