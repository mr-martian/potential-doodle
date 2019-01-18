(register-many-morphs '(adjective :after) ()
                      '((alone t) chocolate complete cosmic elect evil excellent full hopeful new other sweet two unproductive))

(register-many-morphs 'adverb ()
                      '(also forcefully forever just greatly not now))

(register-many-morphs 'noun ()
                      '(Calcium Copper Gold Helium Hydrogen Iron OCD Oxygen Silicon Silver Tin adjective adventure adverb afternoon alphabet amusement anarchy ant anthill antichrist apple arm astronaut attempt axe bacon bathroom beacon bear bee beehive beginning binding bird birth blizzard boundary bovine bow bravery bread breeze brick buliding camera cane cart cat chameleon chaos charcoal cheese chicken chopstick chromesthesia circle cliff clock cloth cloud clover club cold color commandment communism community compassion consonant cookie cosmonaut courage cow crack cudgel custom dad dam day death deceiver deed deluge destiny dignity dinosaur dipper dirt ditch dog domino donut door doughnut drop drum dusk ear echo effort elder electricity elephant emoji emotion end everyone eye eyewear fabric face famine farm fashion fate father fear female fern field fight fin fire fish flag flesh flipper flood flower fog food foot forcefield fort fortress friend fruit future game gas gelato glass glasses grace grass greeting group hail hand harvester here history hive hole home honey honor hook house hurricane husband ice information ink insect inspiration joy judge juice kid knot ladle lady land language laser leader leaf lemon letter library lie life lighthouse lightning limit lips liquid love mace magnet male man mathematics melodrama meme mercy metal milk mind mirror mist mistranslation mom monkey monocle month moon mountain mouth mud name nature nectar night nonahedron nose notebook nothing noun now object ocean one optimism orchard orthography overseer page pain panic paper paralysis particle past peace pedalpub person phaser phonology photograph picture pig pit pizza planet plant platoon pollen poop pragmatics pram praxis pregnancy problem producer projectile rain rainbow recreation reward river roof room root sandwich saxophone secret seed semen servant shelter shield shoe sibling silkworm silliness sister skeleton slope snail snake snow son sound spaceship spear spectacles spider staff steroids stone storm story storyteller strength suffix sundial swamp syntax table tale tea teaching tear telephone telescope tetrahedron thing things thought thunder timekeeper tongue tree tribe tribemember trolley troop trumpet truth tuba two underachiever unicycle untruth valley valor value victory vision vomit vowel warrior water wave way wheelbarrow wife wiki wind window wing wisdom woman wood word worker world worth writing year))

(register-many-morphs 'noun ()
                      '(book+cover bulletin+board card+game chicken+soup contact+lens couch+potato energy+beam french+horn hula+hoop ice+cream marinara+sauce movie+theater music+box olive+oil one+another outer+space pedal+boat playing+card ray+gun rubber+duckie sign+language space+helicopter sugar+glider termination+point thumbs+down thumbs+up vegetable+oil walking+stick writing+system))

(register-many-morphs 'noun '(:proper t)
                      '((Bethlehem "Bethlehem") (Christ "Christ") (Elimelech "Elimelech") (God "God") (Google+Glass "Google+Glass") (Jesus "Jesus") (Jesus+Christ "Jesus+Christ") (Judah "Judah") (Kilyon "Kilyon") (Klingon "Klingon") (Machlon "Machlon") (Moab "Moab") (Naomi "Naomi") (Obadiah "Obadiah") (Photoshop "Photoshop") (Pokemon "Pokemon") (Quizlet "Quizlet") (Sherlock+Holmes "Sherlock+Holmes")))

(register-many-morphs 'preposition ()
                      '((O "O") according+to ahead along+with as as+if at because+of between for from in into of on out to towards with))

; find (verb)
;   output (find<v><pst>): found
;     lexicon: VerbInfl-Pers-and-Num
;   output (find<v><pstprt>): found
;     lexicon: VerbInfl-Pers-and-Num
; leave
;   objects: 1
;   object1: NP
;   output (tense=pst): left
;   output (tense=pstprt): left

(register-many-morphs 'verb () ; unsorted
                      '(command confess do+so find give greet hear hope live lose receive rejoice remain share speak visit walk want watch work write))

(register-many-morphs 'verb '(:objects (NP))
                      '(borrow bring copy imitate judge kill know leave love measure see understand))

(register-many-morphs 'verb '(:objects (CP))
                      '(ask))

(register-many-morphs 'verb '(:objects (PP))
                      '((come-PP "come") dwell excell reside))

(register-many-morphs 'verb '(:objects (IP))
                      '((come-IP "come")))

(register-many-morphs 'verb '(:objects (PP PP))
                      '(go))

(register-many-morphs 'verb '(:objects ())
                      '(cower die hide))

(register-many-morphs '(pronoun :person :number :is-rel) '(:hasspec nil)
                      '((I "I" 1 SG nil)
                        (me 1 SG nil)
                        (we 1 PL nil)
                        (us 1 PL nil)
                        (you 2 SG nil)
                        (he 3 SG nil)
                        (him 3 SG nil)
                        (she 3 SG nil)
                        (her 3 SG nil)
                        (it 3 SG nil)
                        (they 3 PL nil)
                        (them 3 PL nil)
                        (who 3 SG t)
                        (that 3 SG t)
                        (there 3 SG nil)
                        (anyone 3 SG nil)
                        (this 3 SG nil)))
(register-morph 'pronoun 'null :person 3 :number 'SG :is-rel t :audible nil)
                                        ; inaudible subject of relative clause
(register-morph 'pronoun 'copy :person 3 :number 'SG :is-rel nil :audible nil)
                                        ; implicit subject after conjunction
                                        ; for both, overwrite person/number
                                        ; when needed

(register-many-morphs 'punct ()
                      '((comma "-,") (lpar "(-") (rpar "-)") (colon "-:")
                        (period "-.") (excl "-!")))

(register-many-morphs '(tense :conj :person :number) '(:audible nil)
                      '((past pst) (nonpast nonpst)
                        (object prsprt 3 SG) (gerund prsprt 3 SG)
                        (inf inf 3 SG)))



(register-many-morphs 'noun ()
                      '(child mouse))



(register-morph 'determiner 'the :person 3 :number 'SG :is-rel nil :hasspec nil)
(register-morph 'determiner 'your :person 3 :number 'SG :is-rel nil :hasspec nil)
(register-morph 'aux 'do :conj 'inf)
(register-morph 'complementizer '+Q :audible nil)
(register-morph 'complementizer 'that :display "that")
