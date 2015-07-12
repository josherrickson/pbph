interactive:
	R_PROFILE=load.R R -q --no-save

interactive-emacs:
	R_PROFILE=load.R emacs -nw -f R

test:
	R --vanilla -q -e "library(devtools); devtools:::test()"

check:
	R --vanilla -q -e "library(devtools); devtools:::check()"

document:
	R --vanilla -q -e "library(devtools); devtools:::document()"
