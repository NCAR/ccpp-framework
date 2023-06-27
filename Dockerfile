FROM fedora:37

RUN dnf -y update && \
    dnf -y install cmake gcc-g++ gcc-fortran make git python3 && \
    dnf clean all

RUN python3 -m venv ./ccpp-env && \
    source ./ccpp-env/bin/activate && \
    pip3 install -y pytest black flake8 pylint

COPY . /ccpp/

ENTRYPOINT [ '/bin/bash' ]