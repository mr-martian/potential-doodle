cd langs/$1
cd .temporary
hfst-twolc --input ../morphology.twol --output twol.hfst
hfst-lexc -v -f ofst ../morphology.lexc ../.generated/lexicon.lexc -o debug.gen.hfst
hfst-invert -v debug.gen.hfst -o debug.parse.hfst
hfst-compose-intersect debug.gen.hfst twol.hfst -o debug.gen.twol.hfst
hfst-compose-intersect debug.parse.hfst twol.hfst -o debug.parse.twol.hfst
hfst-fst2fst -v debug.parse.twol.hfst -f olw -o ../.generated/parse.hfst
hfst-fst2fst -v debug.gen.twol.hfst -f olw -o ../.generated/gen.hfst
cd ../../..
