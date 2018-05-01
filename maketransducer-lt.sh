cd langs/$1
cd .generated
lt-comp lr ../lang.dix parse.bin
lt-comp rl ../lang.dix gen.bin
cd ../../..
