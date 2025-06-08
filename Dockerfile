FROM ubuntu:latest

RUN apt-get update && \
    apt-get install -y make gcc g++ libncurses-dev libssl-dev git wget \
    cmake python3-pip iproute2 zip unzip \
    && rm -rf /var/lib/apt/lists/*

RUN wget https://packages.erlang-solutions.com/ubuntu/erlang_solutions.asc && \
    sudo apt-key add erlang_solutions.asc && \
    echo "deb https://packages.erlang-solutions.com/ubuntu focal contrib" | sudo tee /etc/apt/sources.list.d/erlang-solutions.list && \
    apt-get update && \
    sudo apt install -y esl-erlang
    
ENV RUNNING_IN_DOCKER=true \
    PIP_BREAK_SYSTEM_PACKAGES=1
WORKDIR /nerlnet
COPY . /nerlnet/
