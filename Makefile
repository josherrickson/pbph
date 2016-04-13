LOAD=R_PROFILE=load.R
RCMD=R --vanilla -q -e

interactive-emacs:
	@$(LOAD) emacs -nw -f R

interactive:
	@$(LOAD) R -q --no-save

.devtools:
	@$(RCMD) "devtools:::$(FUNC)($(DEVTOOLSARG))"

dependencies: FUNC=install_deps
dependencies: DEVTOOLSARG=dependencies=TRUE
test: FUNC=test
check: FUNC=check
document vignette: FUNC=document
vignette: DEVTOOLSARG=roclets='vignette'
clean: FUNC=clean_vignettes
build: FUNC=build
dependencies test check document vignette clean build: .devtools


knit-README:
	@$(LOAD) R -q --no-save -e "knitr::knit('README.Rmd', 'README.md')"
