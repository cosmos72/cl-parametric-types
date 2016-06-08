
PANDOC_AUTHORS="cl-parametric-type Project Team"

all: $(addprefix html/,$(addsuffix .html,$(basename $(notdir $(wildcard ../stl/*.lisp))))) $(addprefix md/,$(addsuffix .md,$(basename $(notdir $(wildcard ../stl/*.lisp)))))

md/%.md: ../stl/%.lisp documents.awk Makefile
	mkdir -p md
	awk -f documents.awk $< > $@

html/%.html: md/%.md Makefile
	mkdir -p html
	which pandoc
	pandoc -f markdown -t html5 \
	-V title="$(basename $(notdir $*))(1)" \
	-V pagetitle="$(basename $(notdir $*))(1)" \
	-V css=../style.css -V author=$(PANDOC_AUTHORS) --template=default.html < $< > $@

clean:
	-rm -r html md
