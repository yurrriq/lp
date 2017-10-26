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

# http://stackoverflow.com/a/17807510
dirname    = $(patsubst %/,%,$(dir $1))

tangle     = notangle -R'$(basename $(notdir $<))' -filter btdefn $< >$@

define subtree_pull =
	git fetch $@ master
	git subtree pull --prefix $@ $@ master --squash
endef

define subtree_pdf =
	$(call subtree_pull)
	ln -f $@/docs/$@.pdf docs/
	$(call add_docs_amend)
endef

define subtree_dir =
	$(call subtree_pull)
	mkdir -p docs/$@
	ln -f $@/docs/*.pdf docs/$@/
	$(call add_docs_amend)
endef

define add_docs_amend =
	git add docs
	git commit --amend --no-edit
endef


.SUFFIXES: .nw .erl .idr .html .lisp .pdf .tex
.nw.erl: ; ${tangle}
.nw.idr: ; ${tangle}
.nw.lisp: ; ${tangle}
# NOTE: requires latex2html
.nw.html: ; noweave -filter btdefn -filter l2h -index -html $< >$@
.nw.tex:  ; noweave -filter btdefn -n -delay -index $< >$@
.tex.pdf: ; latexmk --shell-escape -pdf -outdir=$(call dirname,$<) $<

all: aaig lol pandoc-minted idris erlang paip docs/index.html

.PHONY: idris erlang paip aaig lol pandoc-minted

idris: ${IDR_ALL}
erlang: ${ERL_ALL}
paip:
	@ ${MAKE} -C $@
	@ ln -f $@/tex/$@.pdf docs/

# git remote add -f aaig git@github.com:yurrriq/abstract-algebra-in-gap.git
# git subtree add --prefix aaig aaig master --squash
aaig:
	$(call subtree_pdf)


# git remote add -f lol git@github.com:yurrriq/land-of-lisp.git
# git subtree add --prefix lol lol master --squash
lol:
	$(call subtree_dir)


# git remote add -f pandoc-minted git@github.com:yurrriq/pandoc-minted.git
# git subtree add --prefix pandoc-minted pandoc-minted master --squash
pandoc-minted:
	$(call subtree_pdf)


docs/index.html: README.md
	pandoc -f markdown_github -t html5 -s $< -o $@


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
