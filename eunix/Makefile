CFLAGS ?= -Wall -std=c99
cpif   ?= | cpif
NW_SRC := $(wildcard src/*.nw)
C_SRC  := ${NW_SRC:.nw=.c}
PDF    := $(patsubst src/%.nw,docs/%.pdf,${NW_SRC})
BIN    := $(patsubst src/%.c,bin/%,${C_SRC})


ifneq (,$(findstring B,$(MAKEFLAGS)))
latexmk_flags = -gg
endif

latexmk_flags += -cd -pdf


.PHONY: all check clean

.SUFFIXES: .nw .c .tex .pdf

.nw.c:
	notangle $< ${cpif} $@
	indent -kr -nut $@

.tex.pdf:
	ln -sf ../src/$(notdir $*).{bib,c} ../src/preamble.tex docs/
	latexmk ${latexmk_flags} $<
	rm $*.{bib,c} docs/preamble.tex



all: ${C_SRC} ${BIN} ${PDF}


check:
	@ bin/check


clean:
	$(foreach pdf,${PDF},latexmk ${latexmk_flags} -f -C ${pdf};)
	rm -fR ${BIN} ${C_SRC}{~,} docs/_minted-*


docs/%.tex: src/%.nw
	noweave -autodefs c -n -delay -index $< ${cpif} $@


bin/%: src/%.c
	@ mkdir -p $(dir $@)
	${CC} ${CFLAGS} -o $@ $<
