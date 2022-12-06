FROM ubuntu:jammy

# Spack dependencies
RUN apt-get update && apt-get install -y\
    git\
    python3\
    build-essential\
    curl\
    gfortran

# Install spack
RUN git clone https://github.com/spack/spack.git /root/spack --branch v0.19.0 --single-branch
RUN ln -s /root/spack/bin/spack /bin

# MCT build dependencies
# Install individually to allow build caching
RUN spack -d install gcc@12.2.0
RUN spack -d install intel-oneapi-compilers@2021.1.2
RUN spack -d install openmpi@4.0.2
RUN spack -d install netcdf-fortran@4.5.2 ^netcdf-c@4.7.4

# Link netcdf-fortran libs to somewhere the compiler will find them
RUN ln -s /root/spack/opt/spack/linux*/gcc*/netcdf-fortran*/include/* /usr/include/

RUN git clone https://github.com/ACCESS-NRI/oasis3-mct.git /root/oasis3-mct
WORKDIR "/root/oasis3-mct"

COPY entrypoint.sh ./
ENTRYPOINT ["./entrypoint.sh"]
