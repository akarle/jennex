HTML != for f in *.tmpl; do echo $$(basename $$f tmpl)html; done

.PHONY: build
build: $(HTML)

.PHONY: clean
clean:
	rm -f $(HTML)

.SUFFIXES: .tmpl .html
.tmpl.html:
	(cat header.html; cat $<; cat footer.html) > $@

$(HTML): header.html footer.html Makefile

