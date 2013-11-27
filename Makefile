CC=ghc
CFLAGS=-O2 -fforce-recomp -threaded
PFLAGS=-prof -fprof-auto -caf-all
LFLAGS=-rtsopts
ODIR=./bin

wikiAnalyzerMake: 
	$(CC) $(CFLAGS) $(LFLAGS) $(PFLAGS) -odir $(ODIR) -hidir $(ODIR) -o ./bin/wikiAnalyzer wikiAnalyzer.hs
