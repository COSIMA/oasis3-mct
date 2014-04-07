
.PHONY: default

default:
	echo "include $(shell pwd)/util/make_dir/make.nci" > make.inc
	cd util/make_dir && make -j 4 -f TopMakefileOasis3
