FROM fedora:37

RUN dnf -y update && \
    dnf -y install cmake gcc-g++ gcc-fortran make git && \
    dnf clean all

COPY . /ccpp/

ENTRYPOINT [ '/bin/bash' ]