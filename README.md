# potential-doodle

Potential-doodle is intended to be a program into which one can fairly easily input a description of a conlang (or natlang) which can then be used to generate translations of various annotated texts or to generate random sentences and their translations for various purposes, particularly amusement and attempting to become fluent.

At present the system is relatively incomplete and in a state of significant flux. The description and todo list below reflect the prior design, which can be further examined in the various Python files (other than submitlang.py and ipa.py) and everything in langs/ except langs/2/lang.json. This generator and data storage system were temporarily abandoned when it became clear that, while they were quite functional, the resulting user interface would mostly like end up horribly unfriendly. The solution currently being attempted is to first create the user interface (see, in particular, phonology.php and morphosyntax.php) and let that dictate the structure of the data the generator deals with (see langs/2/lang.json). When this interface is functional, the plan is to then rewrite and document the old code appropriately.

The rest of this file represents the state of this project just before I switched my focus from the generator to the input. The current todo list for the immediate future can be found at the beginning of morphosyntax.php.

Potential-doodle is a combination of a machine readable language representation format, and associated programs for translating to and from that format, as well as a system for practicing them for human learners. It is primarily intended for use with conlangs.

TODO
- [ ] store langs
  - [ ] clearly organize
  - [ ] syntax/morphology
    - [x] deep structure
    - [x] movement
    - [x] conjugation
    - [ ] prefix/suffix
      - [x] concatenative
      - [ ] stem changes
        - [ ] regular
        - [x] irregular
      - [ ] vary form by root phonology
    - [x] infix
    - [ ] agreement
  - [x] lexicon
    - [x] glosses in various languages
    - [x] semantics/usage notes
    - [x] irregular forms
    - [x] syntactical oddities
    - [x] language-specific lexical properties
- [ ] create langs
  - [ ] syntax
  - [ ] morphology
  - [ ] lexicon
    - [ ] import
- [ ] view langs
  - [ ] syntax
  - [ ] morphology
  - [ ] lexicon
- [ ] learn langs
  - [ ] store progress
  - [ ] generate sentences
    - [x] generate deep structure
    - [x] translate deep structure
    - [x] apply movement
    - [ ] recursively apply rules (or apply in layers)
    - [ ] generate text
  - [ ] check input against translations
    - [ ] sentences sometimes specify grammatical information not marked by language
- [ ] current langs
  - [ ] English (1)
    - [x] fix genitive rule
    - [ ] add assorted conjugations
    - [ ] add derivation?
  - [ ] Sajem Tan (2)
    - [ ] fill in lexicon
    - [ ] switch orthographies?
    - [x] add auxilliary tenses
    - [ ] translation of auxillaries and aspects
    - [ ] add derivation?
  - [ ] Vimium (3)
    - [ ] convert to new format
  - [ ] (4)
