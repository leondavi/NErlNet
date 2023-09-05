FROM ubuntu:latest

RUN apt-get update && \
    apt-get install -y make gcc g++ libncurses-dev libssl-dev git wget \
    erlang cmake python3-pip

ENV RUNNING_IN_DOCKER=true
WORKDIR /nerlnet
COPY . /nerlnet/
