(setq *lang* 'english)
(setf (get *lang* 'transducer)
      '("hfst-lookup" "-q" "-b" "0" "-i" "langs/1/.generated/gen.hfst")
      (get *lang* 'parser)
      '("hfst-lookup" "-x" "-w" "langs/1/.generated/parse.hfst"))
(load "english_lexicon.lisp")
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
     (barnode IP () (distribute I (copyprops (if notrel notrel subj)
                                             tense
                                             '(number person))
                                other)
              @)))
   (((barnode-var VP V :spec (var @))))
   (((barnode-var DP D2 :head (var subj))))))
 (list
  (multi-rule ; agreement and subject position
   (((barnode IP ()
              (collect I (var tense :ntype 'tense :opt t)
                       (var perfect :ntype 'perfect :opt t)
                       (var continuous :ntype 'continuous :opt t)
                       (var neg :ntype 'negative :opt t)
                       (var mood :opt t))
              (var @))
     (progn
       (when (and neg (not (or mood perfect continuous)))
         (setf mood (morph aux () do)))
       (spreadprops (list tense mood perfect continuous verb)
                    '((conj tense) (person) (number)))
       (barnode IP () subj
                (distribute I tense mood perfect continuous neg)
                @))))
   (((barnode-var VP V :head (var verb) :spec (var subj :opt t))
     (barnode-var VP V :head verb :spec nil)))))
 (list
  (basic-rule ; negative position
   (collect I (var tense :ntype 'tense) (var neg :ntype 'negative)
            (var first) (var rest :multi t))
   (distribute I tense first neg rest)))
 (list
  (basic-rule ; aux-inversion
   (barnode CP () nil (var mod :opt t) (morph complementizer () +Q)
            (barnode-var IP I :head (collect I (var tense :ntype 'tense :opt t)
                                             (var neg :ntype 'negative :opt t)
                                             (var first) (var other :multi t))))
   (barnode CP () (node aux () first neg) mod (morph complementizer () +Q)
            (barnode-var IP I :head (distribute tense other)))))
 )
(language-taggers 'english
                  (basic-tag noun ("n") ((number "SG") (case "nom")))
                  (basic-tag verb ("v")
                             ((tense "nonpst") (person 3) (number "SG")))
                  (basic-tag (mood aux aspect) ("aux")
                             ((tense "nonpst") (person 3) (number "SG")))
                  (basic-tag adjective "adj")
                  (basic-tag determiner "det")
                  (basic-tag pronoun "prn")
                  (basic-tag preposition "prep")
                  (basic-tag conjunction "conj")
                  (basic-tag adverb "adv")
                  (basic-tag quantifier "quant")
                  (basic-tag complementizer "comp"))
