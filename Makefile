
all:
	cabal configure --enable-benchmarks
	cabal build
	cabal bench --benchmark-options="+RTS -N1 -RTS --output report1.html"
	cabal bench --benchmark-options="+RTS -N2 -RTS --output report2.html"
	cabal bench --benchmark-options="+RTS -N3 -RTS --output report3.html"
	cabal bench --benchmark-options="+RTS -N4 -RTS --output report4.html"
	cabal bench --benchmark-options="+RTS -N5 -RTS --output report5.html"
	cabal bench --benchmark-options="+RTS -N6 -RTS --output report6.html"
	cabal bench --benchmark-options="+RTS -N7 -RTS --output report7.html"
	cabal bench --benchmark-options="+RTS -N8 -RTS --output report8.html"

clean:
	cabal clean


