FROM ubuntu:latest

RUN apt-get update && \
    apt-get install -y make gcc g++ libncurses-dev libssl-dev git wget \
    erlang cmake

ENV RUNNING_IN_DOCKER=true
WORKDIR /nerlnet
COPY . /nerlnet/
