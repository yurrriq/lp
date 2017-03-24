IDR_NW    := $(wildcard src/idris/*.nw)
IDR       := ${IDR_NW:.nw=.idr}
IDR_HTML  := ${IDR_NW:.nw=.html}
IDR_TEX   := ${IDR_NW:.nw=.tex}
IDR_PDF   := ${IDR_NW:.nw=.pdf}
IDR_SRCS  := ${IDR} ${IDR_TEX} ${IDR_PDF} ${IDR_HTML}
IDR_DOCS  := $(patsubst src/%,docs/%,${IDR_HTML} ${IDR_PDF})
IDR_ALL   := ${IDR_SRCS} ${IDR_DOCS}

ERL_NW    := $(wildcard src/erlang/*.nw)
ERL       := ${ERL_NW:.nw=.erl}
# ERL_HTML  := ${ERL_NW:.nw=.html}
ERL_TEX   := ${ERL_NW:.nw=.tex}
ERL_PDF   := ${ERL_NW:.nw=.pdf}
ERL_SRCS  := ${ERL} ${ERL_TEX} ${ERL_PDF} # ${ERL_HTML}
# ERL_DOCS  := $(patsubst src/%,docs/%,${ERL_HTML} ${ERL_PDF})
ERL_DOCS  := $(patsubst src/%,docs/%,${ERL_PDF})
ERL_ALL   := ${ERL_SRCS} ${ERL_DOCS}

LOL_NW    := $(wildcard src/lol/*.nw)
LOL       := ${LOL_NW:.nw=.lisp}
LOL_TEX   := ${LOL_NW:.nw=.tex}
LOL_PDF   := ${LOL_NW:.nw=.pdf}
LOL_SRCS  := ${LOL} ${LOL_TEX} ${LOL_PDF}
LOL_DOCS  := $(patsubst src/%,docs/%,${LOL_PDF})
LOL_ALL   := ${LOL_SRCS} ${LOL_DOCS}

# http://stackoverflow.com/a/17807510
dirname    = $(patsubst %/,%,$(dir $1))

tangle     = notangle -R'$(basename $(notdir $<))' -filter btdefn $< >$@

.SUFFIXES: .nw .erl .idr .html .lisp .pdf .tex
.nw.erl: ; ${tangle}
.nw.idr: ; ${tangle}
.nw.lisp: ; ${tangle}
# NOTE: requires latex2html
.nw.html: ; noweave -filter btdefn -filter l2h -index -html $< >$@
.nw.tex:  ; noweave -filter btdefn -n -delay -index $< >$@
.tex.pdf: ; latexmk --shell-escape -pdf -outdir=$(call dirname,$<) $<

all: idris erlang
	@ ln -sf idris/hello.html docs/index.html

.PHONY: idris erlang

idris: ${IDR_ALL}
erlang: ${ERL_ALL}
lol: ${LOL_ALL}
paip:
	@ ${MAKE} -C paip
	@ ln -fs ../paip/tex/paip.pdf docs/

docs/%.html: src/%.html
	@ mkdir -p $(dir $@)
	@ mv $< $@

docs/%.pdf: src/%.pdf
	@ mkdir -p $(dir $@)
	@ mv $< $@

.PHONY: clean clean-docs clobber

clean_keep_regex := '.*.[bib|erl|idr|lisp|nw|tex]'

clean:
	@ find src -type f \! -regex ${clean_keep_regex} -delete

clean-docs:
	@ rm -fr docs

clobber_keep_regex := '.*.[bib|nw]'

clobber:
	@ find src -type f \! -regex ${clobber_keep_reges} -delete
