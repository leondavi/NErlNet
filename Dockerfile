FROM ubuntu:latest

RUN apt-get update && \
    apt-get install -y make gcc g++ libncurses-dev libssl-dev git wget \
    python3-venv erlang cmake python3-pip iproute2 zip unzip

ENV RUNNING_IN_DOCKER=true
WORKDIR /nerlnet
COPY . /nerlnet/
