FROM ubuntu:latest

RUN apt-get update && \
    apt-get install -y make gcc g++ libncurses-dev libssl-dev git wget \
    erlang cmake python3-pip iproute2 zip unzip \
    && rm -rf /var/lib/apt/lists/*
    
ENV RUNNING_IN_DOCKER=true \
    PIP_BREAK_SYSTEM_PACKAGES=1
WORKDIR /nerlnet
COPY . /nerlnet/
