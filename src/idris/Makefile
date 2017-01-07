SRCS := $(wildcard *.nw)

.SUFFIXES: .html .idr .nw .pdf .tex
.nw.idr: ; notangle $< >$@
# NOTE: requires latex2html
.nw.html: ; noweave -filter l2h -index -html $< >$(notdir $@)
.nw.tex:  ; noweave -n -delay -index $< >$(notdir $@)
.tex.pdf: ; latexmk -pdf $(notdir $<)

all: idris html pdf

idris: ${SRCS:.nw=.idr}

html: ${SRCS:.nw=.html}
	@ ln -sf hello.html index.html

pdf: ${SRCS:.nw=.pdf}
