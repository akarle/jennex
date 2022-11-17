HTML != for f in *.tmpl; do echo $$(basename $$f tmpl)html; done
SCHEME != find . -name '*.scm'

.PHONY: build
build: $(HTML) rsvp

.PHONY: clean
clean:
	rm -f $(HTML)

.SUFFIXES: .tmpl .html
.tmpl.html:
	(cat header.html; cat $<; cat footer.html) > $@

$(HTML): header.html footer.html Makefile

rsvp: $(SCHEME)
	(cd src; chicken-csc prod.scm -o ../$@)
