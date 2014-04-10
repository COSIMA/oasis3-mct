
.PHONY: nci

nci:
	echo "include $(shell pwd)/util/make_dir/make.nci" > util/make_dir/make.inc
	cd util/make_dir && make -j 4 -f TopMakefileOasis3
