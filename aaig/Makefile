cpif   ?= | cpif
tangle  = notangle -R'$@' -filter btdefn $< ${cpif} $@
weave   = noweave -filter btdefn -n -delay -index $< ${cpif} $@

.SUFFIXES: .tex .pdf
.tex.pdf:
	latexmk --shell-escape -outdir=tex -pdf $<

all: \
	src/PerfectNumbers.g \
	src/PerfectNumbers.tst \
	tex/aaig.pdf

# check: bin/runtests all
# 	@ echo
# 	@ for subpkg in intro gps; do \
# 		echo "#:paip.$$subpkg" ; \
# 		$< $$subpkg ; \
# 	done

src/*.g src/*.tst: aaig.nw
	${tangle}

bin/runtests: aaig.nw
	${tangle}
	chmod +x $@

tex/aaig.tex: aaig.nw
	${weave}
