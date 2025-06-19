# Minimum Required LLVM Version (1.18.8): https://github.com/llvm/llvm-project/releases/download/llvmorg-18.1.8/clang+llvm-18.1.8-x86_64-linux-gnu-ubuntu-18.04.tar.xz

FROM ubuntu:22.04 as builder

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
  libffi-dev \
  libz-dev \
  libxml2-dev \
  libedit-dev \
  libc6 \
  swig \
  curl \
  build-essential \
  wget \
  xz-utils \
  && rm -rf /var/lib/apt/lists/*

# Installing LLVM
WORKDIR /llvm
RUN wget https://github.com/llvm/llvm-project/releases/download/llvmorg-18.1.8/clang+llvm-18.1.8-x86_64-linux-gnu-ubuntu-18.04.tar.xz && \
  tar -xf clang+llvm-18.1.8-x86_64-linux-gnu-ubuntu-18.04.tar.xz && \
  mv clang+llvm-18.1.8-x86_64-linux-gnu-ubuntu-18.04/* . && \
  rm clang+llvm-18.1.8-x86_64-linux-gnu-ubuntu-18.04.tar.xz

ENV LLVM_SYS_180_PREFIX=/llvm
ENV PATH="${PATH}:/llvm:/llvm/bin"

# Installing Rust
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
ENV PATH="/root/.cargo/bin:${PATH}"

# Setting up project
WORKDIR /compiler
COPY . .

RUN cargo build --release

FROM ubuntu:24.04

WORKDIR /runner
COPY ./source.dn .

COPY --from=builder /compiler/target/release/deen /usr/local/bin

RUN apt update && apt install -y clang
CMD ["deen", "source.dn", "output", "&&", "./output"]
