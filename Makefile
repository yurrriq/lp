IDR_NW    := $(wildcard src/idris/*.nw)
IDR       := ${IDR_NW:.nw=.idr}
IDR_HTML  := ${IDR_NW:.nw=.html}
IDR_TEX   := ${IDR_NW:.nw=.tex}
IDR_PDF   := ${IDR_NW:.nw=.pdf}
IDR_SRCS  := ${IDR} ${IDR_TEX} ${IDR_PDF} ${IDR_HTML} # NOTE: order matters
IDR_DOCS  := $(patsubst src/%,docs/%,${IDR_HTML} ${IDR_PDF})
IDR_ALL   := ${IDR_SRCS} ${IDR_DOCS}

HTML_SRCS := ${IDR_HTML}
TEX_SRCS  := ${IDR_TEX}
PDF_SRCS  := ${IDR_PDF}

# http://stackoverflow.com/a/17807510
dirname    = $(patsubst %/,%,$(dir $1))

.SUFFIXES: .html .idr .nw .pdf .tex
.nw.idr: ; notangle -R'$(basename $(notdir $<))' -filter btdefn $< >$@
# NOTE: requires latex2html
.nw.html: ; noweave -filter btdefn -filter l2h -index -html $< >$@
.nw.tex:  ; noweave -filter btdefn -n -delay -index $< >$@
.tex.pdf: ; latexmk -pdf -outdir=$(call dirname,$<) $<

all: ${IDR_ALL}
	@ ln -sf idris/hello.html docs/index.html

docs/%.html: ${HTML_SRCS}
	@ mkdir -p $(dir $@)
	@ mv $< $@

docs/%.pdf: ${PDF_SRCS}
	@ mkdir -p $(dir $@)
	@ mv $< $@

.PHONY: clean clean-docs clobber

keep_regex := '.*.[nw|idr]'

clean:
	@ find src -type f \! -regex ${keep_regex} -delete

clean-docs:
	@ rm -fr docs

clobber:
	@ find src \! -name '*.nw' -type f -delete
