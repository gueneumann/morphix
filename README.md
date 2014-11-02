
Morphix - A Fast and Portable Morphological Component for Inflectional Languages
Günter Neumann, DFKI, Saarbrücken, 1999
Hi everybody,

Morphix is a very fast and robust morphological component for German. Besides inflectional analysis, it analyses compounds and is also able to generate wordforms from a given stem entry and some further (optional) morpho-syntactic information. Morphix is implemented in CommonAllegroLisp and should run under all operating systems which support ANSII CL.
History of Morphix
The development of Morphix has a rich history. A first version of Morphix has already been developed in 1986 as part of a programming course by Wolfgang Finkler (recently, he moved to the Deutsche Börse) and Günter Neumann (that's me). In 1988, we have improved Morphix in order to perform morphological generation from stems. Here are the two main publications during this area (unfortunately, no electronic versions are available, and even the latter is in German):

    W. Finkler, G. Neumann:

    MORPHIX. A Fast Realization of a Classification-Based Approach to Morphology.
    In: Trost, H. (ed.): 4. Österreichische Artificial-Intelligence-Tagung. Wiener Workshop - Wissensbasierte Sprachverarbeitung. Proceedings. Berlin etc.: Springer, 1988, 11-19.
     
    W. Finkler, G. Neumann:

    MORPHIX - Ein hochportabler Lemmatisierungsmodul für das Deutsche.
    FB Informatik, KI-Labor, Memo Nr. 8, Juli 1986.

The Newest Version
In 1995, I started to "re-implement" Morphix in order to make it ready for new research area of intelligent information extraction systems . The major improvements made during this time are:

    the use of tries as the unique storage device for all sort of lexical information in Morphix (e.g., for lexical entries, prefix, inflectional endings); besides the usual functionality (insertion, retrieval, deletion), a number of more complex functions are available most notably a regular trie matcher, and

     
    online processing of compounds which is realized by means a recursive trie traversal. During traversal two-level rules are applied for recognizing possible decompositions of the word form in question. Morphix also supports robust processing of compounds, such that compounds are recognized by means of longest matching substrings found in the lexicon; e.g., the word ``adfadfeimer'' will return result for ``eimer'' assuming that ``adfadf'' is no legal lexical stem.

     
    A generic and parameterizable output interface for Morphix has been implemented which returns a normalized feature vector representation of the computed morpho-syntactic information. It is possible to parameterize the set of relevant morpho-syntactic features which supports lexical tagging and feature relaxation.

     
    On basis of this new interface a very efficient specialized constraint solver  has been implemented which can be used in combination with parsing techniques in order to perform unification of the morpho-syntactic information.

In summary,  the basic processing strategy employed by the new Morphix version consists of trie traversal combined with the application of finite state automata.

The main references for this version are (unfortunately, I'm still too busy in order to write down a detailed report about the new Morphix version):

    G. Neumann, R. Backofen, J. Baur, M. Becker, C. Braun:

    An Information Extraction Core System for Real World German Text Processing.
    In Proceedings of 5th ANLP, Washington, March, 1997.
     
    G. Neumann and G. Mazzini:

    Domain-adaptive Information Extraction (Draft Version)
    DFKI, Technical Report, 1999.

Currently, Morphix has been used for the German and Italian language. An English version is under way. Morphix is implemented in Lisp following Standard Allegro Common Lisp. It has been tested under Solaris, Linux, Windows 98 and Windows-NT. The German version has a very broad coverage, and an excellent speed (5000 words/sec without compound handling, 2800 words/sec with compound processing (where for each compound all lexically possible decompositions are computed).
About the Software
The directory structure
Morphix comes with a gzipped tar file. Untaring will create the following folders:

    src: contains all the source files

    ;
     
    lex: contains lexical data

    ;
     
    bin: will contain the compiled .fasl files

    ;
     
    docu: contains some documentation

     

You will also find a file morphix-sys.lisp, a simple make file. Before compiling and running Morphix you should edit this file especially the pathname variables. Follow the information inside this file.
Compiling and Loading Morphix
Once you have set the correct Morphix path names, then compiling and loading is easy:

    Load the file morphix-sys.lisp into Lisp

     
    Compile Morphix by calling (COMPILE-MORPHIX)

     
    Load Morphix by calling (LOAD-MORPHIX); this will not load the lexicon data base

     
    Load Lexica by calling (LOAD-MLEX); all lexicon files which are listed in the variable *lex-file-name-list* will be loaded. Note that Morphix lexica have to have the suffix .mlex

     
    All functions and variables of Morphix are defined in the package "MORPHIX" or "MO". Thus, if you want to test morphix you might call (IN-PACKAGE :MO) after having loaded Morphix.

Performing benchmarks
There are two ways for performing a benchmark (see also file benchmar.lisp):

        (MORPHIX-BENCHMARK): its performs a run of morphix on a set sentences. Each sentence is analysed 100 times where the best and worst 30 sentences are disregared

         
        (BENCHTEXT): applies a benchmark on a file bound in variable *textpath*

The toplevel functions
Since Morphix has a long history, it also has a long list of top-level functions.
The mother of all top-level functions is (in the following I assume you are in the package "MORPHIX"):
(WORD-ANALYSIS WORD), where WORD should be string in lowercase (see also file main.lisp).
(W-A WORD) is a convenient abbreviation. WORD-ANALYSIS returns a list of readings in a very compact form.

The newest top-level functions are:

    (RT LOWER-CASE-STRING):

    this will return a list of readings where each reading is a tripple of the form (STEM INFLEXION POS), where STEM is either a string or a list of strings (in case of compounds). See file user-interface.lisp
     
    (OUTER-RT STRING), where STRING can be an arbitray string because it will be scanned by a specific scanner function MORPHIX-READ (see file morphix-reader.lisp)

The output of INFLEXION can be of different form depending on the active output interface:

    a string: the default output is a string coding the morphic-syntactic information. If you used to another output function you can re-use this one by calling (SET-TYPES) (see file morphix-to-fset.lisp)

     
    a DNF representation: call (SET-DNF)

     
    a feature vector representation based on the DNF form: call (SET-FEAT)

     
    a DNF-based symbol: call (SET-SYM)

Global variables
The following is a list of global variables which are usefull for setting of the general behaviour of Morphix (see also file globals.lisp):
 

    *property-retrieval*: if set to T then the result of each analysed wordform is cached so that next time a simple lookup is performed. In order to reset the cache call the function (RESET-RESULTS).
    *handle-composita*: switch off/on compound processing by either NIL or T
    *all-composita*: if T compute all possible segmentations; if NIL compute only the longest matching compounds
    *handle-unknowns*: switch off/on processing of robust compound processing
    *all-unknown-composita*: if T then compute all possible robust segmentations
    *relevant-morphix-cats*: a list of word categories for which compound processing should be performed. Possinble values are VERB. NOMEN, ADJEKTIV. Default list is '(NOMEN ADJEKTIV)

Other toplevel functions:
The file user-interface.lisp a set of file based functions are defined which can be used e.g., to generate a fullform lexicon.
The main function used here are regular trie matcher (see file dtree-matcher.lisp). The main caller is
(DTREE:MATCH-ENTRY REGULAR-STRING LEXICON), where REGULAR-STRING is a string containing regular expression following standard grep syntax. LEXICON is either mo::*stem-lexicon* or mo::*fullform-lexicon*.
For example (dtree::match-entry "h.*" mo::*stem-lexicon*) will match all lexical entries beginning with the letter h.
Note that the function (dtree::match-entry-help NIL) gives a description of possible regular syntax (see file dtree-matcher.lisp).
 
Generation
The toplevel function  for generation is defined in file morphix-gen.lisp which contains a description of all possible parametrizations. The toplevel function is (GENERATE STEM CAT OTHER-KEYS) where STEM is a stem, CAT a possible word category and OTHER-KEYS appropriate morpho-syntactic constraints. The possible values for CAT and OTHER-KEYS can be find in file morphix-gen.lisp. If OTHER-KEYS is not specified then some default values are used.
Try: (generate "haus" 'NOUN)
Note that generation does not support generation of compounds.

     

