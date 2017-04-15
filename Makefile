make_md = pandoc $(1).md --css css/solarized.css > $(1).html

all: js md

js:
	pulp browserify > web/js/output.js

md:
	$(call make_md,web/index)
	$(call make_md,web/program)
