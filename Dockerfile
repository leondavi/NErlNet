FROM ubuntu:latest

RUN apt-get update && \
    apt-get install -y make gcc g++ libncurses-dev libssl-dev git wget \
    cmake python3-pip iproute2 zip unzip

# Install Erlang/OTP 28.0
RUN git clone https://github.com/erlang/otp.git  && \
    cd otp  && \
    git fetch --all --tags  && \
    git checkout tags/OTP-28.0 -b otp-28.0 && \
    ./configure  && \
    make -j2 && \
    make install && \
    cd - && \
    rm -rf otp

ENV RUNNING_IN_DOCKER=true \
    PIP_BREAK_SYSTEM_PACKAGES=1
WORKDIR /nerlnet
COPY . /nerlnet/
