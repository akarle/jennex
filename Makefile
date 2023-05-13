HTML != for f in *.tmpl; do echo $$(basename $$f tmpl)html; done
SCHEME != find . -name '*.scm'

.PHONY: build
build: build-html


# Standalone target for Netlify, which doesn't have chicken-csc
.PHONY: build-html
build-html: $(HTML)

.PHONY: clean
clean:
	rm -f $(HTML)

.SUFFIXES: .tmpl .html
.tmpl.html:
	(cat header.html; cat $<; cat footer.html) > $@

$(HTML): header.html footer.html Makefile
