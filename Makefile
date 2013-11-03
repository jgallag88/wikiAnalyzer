CC=ghc
CFLAGS=-O2
ODIR=./bin

wikiAnalyzerMake: 
	$(CC) $(CFLAGS) -odir $(ODIR) -hidir $(ODIR) -o ./bin/wikiAnalyzer wikiAnalyzer.hs
