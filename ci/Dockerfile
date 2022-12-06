FROM ubuntu:jammy

# Install spack dependencies
RUN apt-get update && apt-get install -y\
    git\
    python3\
    build-essential\
    curl\
    gfortran

# Install spack
RUN git clone https://github.com/spack/spack.git /root/spack --branch v0.19.0 --single-branch
RUN ln -s /root/spack/bin/spack /bin

# Set up ACCESS spack package repo
RUN git clone https://github.com/ACCESS-NRI/spack_packages.git /root/spack_packages
COPY repos.yaml /root/spack/etc/spack/repos.yaml

# Install dependencies separately for caching
RUN spack install --only dependencies access.nri.oasis3-mct

ENTRYPOINT ["/bin/bash", "-c", "spack -v install --only package access.nri.oasis3-mct"]
