LOAD=R_PROFILE=load.R
RCMD=R --vanilla -q -e
DEVTOOLS=library(devtools)
# DEVFUN needs to be followed by a devtools function like test() or check()
DEVFUN=devtools:::

# These two interactive commands aren't working right with documentation.
# To see documentation live; run R then source("load.R")
interactive-emacs:
	@$(LOAD) emacs -nw -f R

interactive:
	@$(LOAD) R -q --no-save

test:
	@$(RCMD) "$(DEVTOOLS); $(DEVFUN)test()"

check:
	@$(RCMD) "$(DEVTOOLS); $(DEVFUN)check()"

document:
	@$(RCMD) "$(DEVTOOLS); $(DEVFUN)document()"

build:
	@$(RCMD) "$(DEVTOOLS); $(DEVFUN)build()"
