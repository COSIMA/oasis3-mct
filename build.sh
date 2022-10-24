#!/bin/bash

PROGNAME="$( basename ${BASH_SOURCE[0]} )"
PROJDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
PLATFORM="ubuntu"

if hostname --fqdn | grep gadi.nci.org.au$ > /dev/null
then
	# TODO: On gadi, should we execute this here or is it the callers
	#       responsibility?
	module purge
	module load intel-compiler/2019.5.281
	module load netcdf/4.7.4
	module prepend-path PKG_CONFIG_PATH "${NETCDF_BASE}/lib/Intel/pkgconfig/"
	module load openmpi/4.0.2
	PLATFORM="nci"
fi

# See doc/oasis3mct_UserGuide.pdf:
#
# compiles all OASIS3-MCT libraries mct, scrip and psmile:
# make -f TopMakefileOasis3
#
# removes all OASIS3-MCT compiled sources and librairies:
# make realclean -f TopMakefileOasis3

cd ${PROJDIR}/util/make_dir && \
echo "include ${PROJDIR}/util/make_dir/make.${PLATFORM}" > make.inc && \
OASIS_HOME=${PROJDIR} make -f TopMakefileOasis3 realclean && \
OASIS_HOME=${PROJDIR} make -j$(nproc) -f TopMakefileOasis3
