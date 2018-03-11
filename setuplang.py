import sys, os
pth = 'langs/%s/' % sys.argv[1]
os.mkdir(pth)
f = open(pth+'lang.txt', 'w')
f.write('''metadata
  name
    local: %s
  creator: Natlang
syntax
  start-with: CP
  auto-setlang: AP; CP; IP, NP, PP, DP, VP
  node-types
    AP
      xbar: ~; ~; adjective; ~
    CP
      xbar: ~; ~; complementizer; IP
    IP
      xbar: ~; ~; I; VP
    NP
      xbar: ~; AP?; noun; ~
    PP
      xbar: ~; ~; preposition; DP
    I
      variable: $t:tense
      variable: $a:aspect
      variable: $m:mood?
      structure: [I $t $a $m]
      translation (2): [I $t $a $m]
    DP
      variable: $head:determiner
      option ($head(hasspec=true))
        xbar: DP; ~; $head; NP
      option ($head(hasspec=false))
        xbar: ~; ~; $head; NP
    VP
      variable: $head:verb
      option ($head(objects=1))
        xbar: DP; adverb?; $head; DP
      option ($head(objects=0))
        xbar: DP; adverb?; $head; ~
transform
#possession
  rule
    form: |[DP $spec:DP $mod? $head $comp?]
    result: |[DP <suffix $spec $head> $mod ~ $comp]
#subject position
  rule
    form: |[IP ~ $imod? $ihead |[VP $subj $mod? $head $comp?]]
    result: |[IP $subj $imod $ihead |[VP ~ $mod $head $comp]]
#rotations
  rotate: Vmod
  rotate: Nmod''' % sys.argv[2])
f.close()

f = open(pth+'lexicon.txt', 'w')
f.write('some-noun (noun)\n  display: monkey')
f.close()
