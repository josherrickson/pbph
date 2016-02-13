LOAD=R_PROFILE=load.R
RCMD=R --vanilla -q -e

interactive-emacs:
	@$(LOAD) emacs -nw -f R

interactive:
	@$(LOAD) R -q --no-save

.devtools:
	@$(RCMD) "library(devtools); devtools:::$(FUNC)()"

dependencies: FUNC=install_deps
test: FUNC=test
check: FUNC=check
document: FUNC=document
build: FUNC=build
dependencies test check document build: .devtools

clean:
	git clean -Xfd
