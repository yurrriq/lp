cpif   ?= | cpif
tangle  = notangle -R'$@' -filter btdefn $< ${cpif} $@
weave   = noweave -filter btdefn -n -delay -index $< ${cpif} $@
gap    ?= gap.sh


.SUFFIXES: .tex .pdf
.tex.pdf:
	latexmk --shell-escape -outdir=tex -pdf $<


all: \
	lib/PerfectNumbers.gd \
	lib/PerfectNumbers.gi \
	tst/PerfectNumbers.tst \
	doc \
	tex/aaig.pdf \


# TODO: check


lib/*.gd lib/*.gi tst/*.tst: aaig.nw
	${tangle}


bin/runtests: aaig.nw
	${tangle}
	chmod +x $@


tex/aaig.tex: aaig.nw
	${weave}


doc: makedoc.g *.g lib/*.gd lib/*.gi tst/*.tst
	@ ${gap} -b $<
