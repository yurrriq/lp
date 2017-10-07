cpif    ?= | cpif
tangle   = notangle -R'$@' -filter btdefn $< ${cpif} $@
weave    = noweave -filter btdefn -n -delay -index $< ${cpif} $@
GAPROOT ?= /run/current-system/sw/share/gap/build-dir
gap     ?= ${GAPROOT}/bin/gap.sh


srcs := \
	PackageInfo.g \
	default.nix \
	init.g \
	makedoc.g \
	read.g \
	gap/PerfectNumbers.gd \
	gap/PerfectNumbers.gi \
	tst/PerfectNumbers.tst \
	tst/testall.g


.SUFFIXES: .tex .pdf
.tex.pdf:
	latexmk -outdir=tex -pdf $<


all: \
	${srcs} \
	check \
	docs \
	docs/aaig.pdf \


clean:
	@ latexmk -f -C -outdir=tex -pdf tex/aaig.tex
	@ rm -fR ${srcs} auto/ docs/ gap/ result tex/_minted* tex/aaig.tex tst/


check: ${srcs}
	@ ${gap} -l "${GAPROOT};." -q tst/testall.g


docs: makedoc.g ${srcs}
	@ ${gap} -b $<


${srcs}: aaig.nw
	@ mkdir -p $(dir $@)
	${tangle}


tex/aaig.tex: aaig.nw
	${weave}

docs/aaig.pdf: tex/aaig.pdf
	@ ln -f $< $@
