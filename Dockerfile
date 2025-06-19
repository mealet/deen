# Minimum Required LLVM Version (1.18.8): https://github.com/llvm/llvm-project/releases/download/llvmorg-18.1.8/clang+llvm-18.1.8-x86_64-linux-gnu-ubuntu-18.04.tar.xz

FROM ubuntu:22.04

# Installing dependencies
RUN apt update && apt -y upgrade
RUN apt install -y \
  git \
  cmake \
  ninja-build \
  g++ \
  python3 \
  zlib1g-dev \
  libncurses5 \
  libncurses5-dev \
  libxml2-dev \
  libedit-dev \
  swig \
  curl \
  build-essential \
  wget \
  xz-utils \
  libc6

# Installing LLVM
WORKDIR /llvm
RUN wget https://github.com/llvm/llvm-project/releases/download/llvmorg-18.1.8/clang+llvm-18.1.8-x86_64-linux-gnu-ubuntu-18.04.tar.xz && \
  tar -xf clang+llvm-18.1.8-x86_64-linux-gnu-ubuntu-18.04.tar.xz && \
  mv clang+llvm-18.1.8-x86_64-linux-gnu-ubuntu-18.04/* . && \
  rm clang+llvm-18.1.8-x86_64-linux-gnu-ubuntu-18.04.tar.xz

ENV LLVM_SYS_180_PREFIX=/llvm
ENV PATH="${PATH}:/llvm:/llvm/bin"

# Verify LLVM
RUN llvm-config --version

# Installing Rust
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y && \
  . "$HOME/.cargo/env"


ENV PATH="${PATH}:$HOME/.cargo:$HOME/.cargo/bin"

# Setting up project
WORKDIR /compiler
COPY . .

CMD ["cargo", "run", "--release"]
