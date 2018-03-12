cd langs/$1
hfst-twolc --input morphology.twol --output twol.hfst
hfst-lexc -v -f foma morphology.lexc lexicon.lexc -o debug.gen.hfst
hfst-invert -v debug.gen.hfst -o debug.parse.hfst
hfst-compose-intersect debug.gen.hfst twol.hfst -o debug.gen.twol.hfst
hfst-compose-intersect debug.parse.hfst twol.hfst -o debug.parse.twol.hfst
hfst-fst2fst -v debug.parse.twol.hfst -f olw -o parse.hfst
hfst-fst2fst -v debug.gen.twol.hfst -f olw -o gen.hfst
cd ../..
