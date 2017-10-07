cpif    ?= | cpif
tangle   = notangle -R'$@' -filter btdefn $< ${cpif} $@
weave    = noweave -filter btdefn -n -delay -index $< ${cpif} $@
GAPROOT ?= /run/current-system/sw/share/gap/build-dir
gap     ?= ${GAPROOT}/bin/gap.sh


.SUFFIXES: .tex .pdf
.tex.pdf:
	latexmk --shell-escape -outdir=tex -pdf $<


all: \
	lib/PerfectNumbers.gd \
	lib/PerfectNumbers.gi \
	tst/PerfectNumbers.tst \
	tst/testall.g \
	doc \
	tex/aaig.pdf \
	check


check: *.g lib/*.gd lib/*.gi tst/*.g tst/*.tst
	@ ${gap} -l "${GAPROOT};." -q tst/testall.g


doc: makedoc.g *.g lib/*.gd lib/*.gi tst/*.tst
	@ ${gap} -b $<


lib/*.gd lib/*.gi tst/*.g tst/*.tst: aaig.nw
	${tangle}


tex/aaig.tex: aaig.nw
	${weave}
