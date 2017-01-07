all: hello.pdf

%.idr: %.nw
	notangle $< >$@

# NOTE: requires latex2html
%.html: %.nw
	noweave -filter l2h -index -html $< >$@

%.tex: %.nw
	noweave -n -delay -index $< >$@

%.pdf: %.tex
	latexmk -pdf $<

clean-latex: $(wildcard *.tex)
	@ if [ ! -z "$^" ]; then \
	latexmk -c -silent; \
	fi

clean: clean-latex
	@ rm -fr *.ibc *.tex

clobber: clean
	@ rm -fr *.html *.idr *.pdf
