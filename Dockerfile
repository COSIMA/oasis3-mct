FROM spack/ubuntu-jammy:v0.19.0

# Install individually to allow build caching
RUN spack -d install gcc@12.2.0
RUN spack -d install intel-oneapi-compilers@2021.1.2
RUN spack -d install openmpi@4.0.2
RUN spack -d install netcdf-fortran@4.5.2 ^netcdf-c@4.7.4

# Link netcdf-fortran libs to somewhere the compiler will find them
RUN ln -s /opt/spack/linux*/gcc*/netcdf-fortran*/include/* /usr/include/

RUN git clone https://github.com/ACCESS-NRI/oasis3-mct.git
WORKDIR "./oasis3-mct"

RUN spack load\
    gcc\
    intel-oneapi-compilers\
    openmpi\
    netcdf-fortran\
    && ./build.sh
