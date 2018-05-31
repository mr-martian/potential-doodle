import sys, os
pth = 'langs/%s/' % sys.argv[1]
os.mkdir(pth)
os.mkdir(pth+'translate/')
os.mkdir(pth+'.generated/')
os.mkdir(pth+'.temporary/')
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
      translation (1): [I $t $a $m]
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
lexc: hfst
  split-root: _
  noun
    format: {root[0]}<n>{number}{case}
    tags
      number: number
        default: <sg>
      case: case
        default: <nom>
    in: NounRoot
    to: NounInfl
transform
#possession
  rule
    form: |[DP $spec:DP $mod? $head $comp?]
    result: |[DP [GEN $spec $head] $mod ~ $comp]
#subject position
  rule
    form: |[IP ~ $imod? $ihead |[VP $subj $mod? $head $comp?]]
    result: |[IP $subj $imod $ihead |[VP ~ $mod $head $comp]]
#rotations
  rotate: Vmod
  rotate: Nmod''' % sys.argv[2])
f.close()

f = open(pth+'lexicon.txt', 'w')
f.write('-Q (complementizer)\n\nsome-noun (noun)\n  display: monkey')
f.close()

f = open(pth+'morphology.lexc', 'w')
f.write('''! Morphological Transducer for {}

Multichar_Symbols

! Part of speech categories
%<n%>     ! Noun
%<sg%>    ! singular
%<pl%>    ! plural
%<nom%>   ! nominative

! Other symbols
%>      ! Morpheme boundary

LEXICON Root

NounRoot ;
Punctuation ;

LEXICON NounInfl

%<n%>%<sg%>%<nom%>: # ;
%<n%>%<pl%>%<nom%>:s # ;

LEXICON Punctuation

.%<sent%>:. # ;
..%<sent%>:.. # ;
...%<sent%>:... # ;
%;%<sent%>:%; # ;
%:%<sent%>:%: # ;
%!%<sent%>:%! # ;
%-%<guio%>:%- # ;
%—%<guio%>:%— # ;
,%<cm%>:, # ;
%?%<sent%>:%? # ;
%'%<apos%>:%' # ;
%"%<sent%>:%" # ;
%«%<lquot%>:%« # ;
%»%<rquot%>:%» # ;
%”%<rquot%>:%” # ;
%“%<lquot%>:%“ # ;
%(%<lpar%>:%( # ;
%]%<rpar%>:%] # ;
%[%<lpar%>:%[ # ;
%)%<rpar%>:%) # ;
\%<sent%>:\ # ;
/%<sent%>:/ # ;'''.format(sys.argv[2]))
f.close()

f = open(pth+'morphology.twol', 'w')
f.write('''Alphabet
 A B C D E F G H I J K L M N O P Q R S T U V W X Y Z
 a b c d e f g h i j k l m n o p q r s t u v w x y z
 %{Tf%}:0 ;

Sets

FinalCons = t k m n c ;

Rules

"{Tf} becomes 'HA' before FinalCons"
%{Tf%}:HA <=> _ :FinalCons ;''')
f.close()
os.system('python3 maketransducer.py %s' % sys.argv[1])
