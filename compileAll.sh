ghc Main
for i in 1 2 divBy3
do
	./Main samples/$i.dfa samples/$i.txt samples/$i.tex
	cd samples
	pdflatex $i.tex
	cd ..
done
