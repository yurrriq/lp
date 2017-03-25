cpif   ?= | cpif
tangle  = notangle -R'$@' $< ${cpif} $@
weave   = noweave -autodefs lisp -n -delay -index $< ${cpif} $@

.SUFFIXES: .tex .pdf
.tex.pdf:
	latexmk --shell-escape -outdir=tex -pdf $<
	@ cp $@ ../docs/$(notdir $@)

all: \
	paip.asd \
	init.lisp \
	src/intro.lisp \
	src/gps.lisp \
	bin/runtests \
	tex/paip.pdf

check: bin/runtests all
	@ echo
	@ for subpkg in intro gps; do \
		echo "#:paip.$$subpkg" ; \
		$< $$subpkg ; \
	done

paip.asd init.lisp src/*.lisp: paip.nw
	${tangle}

bin/runtests: paip.nw
	${tangle}
	chmod +x $@

tex/paip.tex: export FINDUSES_LISP=1
tex/paip.tex: paip.nw
	${weave}
