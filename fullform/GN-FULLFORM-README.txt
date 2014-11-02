Generierung von Vollformenlexikon in Morphix:
Date:  19.4.2010

definiere N-Gram basierten inverted index auf vollformen 
definier similarity function für dictionary lookup
-> mein NEREID


Date: 12.2.2009:

siehe auch dfki/CCS/acquire/...

Usage:

- load file make-ff-lexicon.lisp on top of morphix
- (init-ff <system>) -> integer code for inflexion

- (make-ccw-lex "/Users/gune00/dfki/src/Lisp/morphix/fullform/ccw-lex.txt")
- (make-ocw-lex "/Users/gune00/dfki/src/Lisp/morphix/fullform/ocw-lex.txt")

- (init-ff :system :python) -> feat vec
- (init-ff :system :sppc) -> feat val

Bemerkungen:
- die Eintraege in der Liste *homographs* kommen im CCW und OCW Lexikon vor
- currently, the reading encoding is returned (an integer)
- thus need to add additional a file with the reading-mapping
  - store results of (save-val-key-map) in a file
  - BUT note: reading structure depends on system type

Problems:
  - wenn :system :python -> Ausgaben noch nicht korrekt wenn feat-vec
    OK

  - Viele Fehler der Art:
     enkomium: error The value MULTIPLE is not of type LIST. 
     OK

  - UTF-basiertes Lexikon erzeugen -> eventuell in Zusammenhang mit Erlernem vom Zerlegungs und
    Derivationsregeln
    -> das kˆnnte ich eventuell mittels externen Systemen hinbekommen, zB Google
    
    -> Wiktionary auch !!!


Verbesserungen:

- Statistik hinzufügen, zB Google Count

- Lernen von Komposita und Derivation:
  - Obacht: das müsste auch für jedes POS-tagged Lexikon und Sprachen funktionieren

  - aktuell enthaehlt ocw-ff.txt Komposita
  - versuche daher mittels Lexikon automatisch Komposita-Zerlegung zu berechnen + Zerlegungsregls
    zB Arbeitskraft -> arbeit und kraft auch im Lexikon -> 
    Wort nicht aufnehmen, aber -s- als Infix + Kontext als neue Zerlegunsgregeln
  - wobei überprüfen. ob -kraft auch Teil anderer Wörter -> je mehr, desto sicher ist es ein
   Kompositum
  -> dies aber nur fuer bestimmte POS-POS Sequenzen, zB nur für N-N oder V-N
  -> wie mehrstellige Komposita erkennen ?

  - sodann auch eventuell Derivation (nach Komposita):
    - zB heizung -> suffixe -> heiz - ung -> checken, ob heiz = stem (von V/Adj) und
      ung Suffix von vielen andern Nomen -> Validerung, dass ung Derivationsendung ist

-> fullform Lexikon als getaggter Korpus betrachtet


Python-basierte Version für obiges:
- teste trie3.py und eventuell die anderen als Lexikonkomponente
- suffix tree einsetzen


April, 2013:

Habe jetzt CCW-based lexicon gebaut ala conll format.
Habe schon ä ö ü ersetzt ! -> siehe File ccw-morphix-all.xml

OBACHT: Habe dadurch einen quasi alignierten Korpus ! um später automatisch UE-Ü korrekte Übersetzung zu machen
