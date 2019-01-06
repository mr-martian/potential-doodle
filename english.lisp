(setq *lang* 'english)
(register-morph 'tense 'nonpast :audible nil :tense 'pst :conj 'pst)
(register-morph 'determiner 'the :person 3 :number 'SG :is-rel nil :hasspec nil)
(register-morph 'noun 'child)
(register-morph 'preposition 'of)
(register-morph 'determiner 'your :person 3 :number 'SG :is-rel nil :hasspec nil)
(register-morph 'adjective 'elect)
(register-morph 'noun 'sister)
(register-morph 'verb 'greet)
(register-morph 'pronoun 'you :number 'SG :person 2 :is-rel nil)
(register-morph 'aux 'do :conj 'inf)
(register-morph 'complementizer '+Q)
(language-syntax
 'english
 (list
  (context-rule ; do insertion
   (barnode-var CP C :head (morph complementizer () +Q)
                :bar (barnode-var IP I :head (var @)))
   (collect I (var tense :ntype 'tense :opt t))
   (distribute I tense (morph aux () do))))
 (list
  (multi-rule ; gather person/number
   (((barnode-var DP D1 :head (var notrel)
                  :bar (barnode-var NP N :bar (var @))))
    ((var @)))
   (((barnode-var CP C :bar (var @))))
   (((barnode IP ()
              (collect I (var tense :ntype 'tense)
                       (var other :multi t))
              (var @))
     (barnode IP () (distribute I
                                (if notrel
                                    (copyprop notrel 'person
                                              (copyprop notrel 'number tense))
                                  (copyprop subj 'person
                                            (copyprop subj 'number tense)))
                                other)
              @)))
   (((barnode-var VP V :spec (var @))))
   (((barnode-var DP D2 :head (var subj))))))
 )
