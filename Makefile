SRCS := $(wildcard src/idris/*.nw)

all:
	make -C src/idris
	make $(addprefix docs/,$(notdir ${SRCS:.nw=.html})) \
	$(addprefix docs/,$(notdir ${SRCS:.nw=.pdf}))

docs/%: src/idris/%
	@ cp $< $@

# clean-latex: $(wildcard *.tex)
# 	@ if [ ! -z "$^" ]; then \
# 	latexmk -c -silent; \
# 	fi

# clean: clean-latex
# 	@ rm -fr *.ibc *.tex

# clobber: clean
# 	@ rm -fr docs/*.html *.idr *.pdf
# 	@ ln -s hello.html docs/index.html
