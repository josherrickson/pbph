LOAD=R_PROFILE=load.R
RCMD=R --vanilla -q -e

interactive-emacs:
	@$(LOAD) emacs -nw -f R

interactive:
	@$(LOAD) R -q --no-save

.devtools:
	@$(RCMD) "devtools:::$(FUNC)($(DEVTOOLSARG))"

DEVTOOLSARG=
dependencies: FUNC=install_deps
dependencies: DEVTOOLSARG=dependencies=TRUE
test: FUNC=test
check: FUNC=check
document: FUNC=document
vignette: FUNC=build_vignettes
clean-vignette: FUNC=clean_vignettes
build: FUNC=build
dependencies test check document vignette clean-vignette build: .devtools

clean: clean-vignette
	git clean -Xfd

knit-README:
	@$(LOAD) R -q --no-save -e "knitr::knit('README.Rmd', 'README.md')"
