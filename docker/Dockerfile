#> docker image build -t nqoki709/monolis_build:latest .
#> docker push nqoki709/monolis_build

FROM ubuntu:20.04
ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update \
 && apt-get -y install build-essential cmake gfortran git \
 && apt-get -y install libmetis-dev libmumps-dev libopenmpi-dev \
 && apt-get -y install doxygen graphviz \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
