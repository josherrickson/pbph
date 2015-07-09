interactive:
	R_PROFILE=load.R R -q --no-save

interactive-emacs:
	R_PROFILE=load.R emacs -nw -f R

test:
	R -q -e "library(devtools); devtools:::test()"
