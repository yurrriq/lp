.SUFFIXES: .tex .pdf
.tex.pdf: ; latexmk --shell-escape -outdir=tex -pdf $<

all: \
	paip.asd \
	init.lisp \
	src/intro.lisp \
	src/gps.lisp \
	bin/runtests \
	tex/paip.pdf

paip.asd init.lisp src/*.lisp: paip.nw
	notangle -R'$(notdir $@)' $< | cpif $@

test: bin/runtests all
	@ for subpkg in intro gps; do \
		$< $$subpkg ; \
	done

bin/runtests: paip.nw
	@ notangle -R'$(notdir $@)' $< | cpif $@
	@ chmod +x $@

tex/paip.tex: export FINDUSES_LISP=1
tex/paip.tex: paip.nw
	@ noweave -autodefs lisp -n -delay -index $< | cpif $@
