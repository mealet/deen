FROM archlinux:base-devel

# Dependencies and languages installation
RUN pacman -Syu --noconfirm
RUN pacman -S --noconfirm \
  rustup \
  git \
  cmake \
  clang \
  lld \
  && rustup default stable

# LLVM Installation 
RUN pacman -S --noconfirm llvm18
ENV LLVM_SYS_180_PREFIX=/usr/lib/llvm18

# Caching up dependencies
WORKDIR /compiler
COPY Cargo.toml Cargo.lock ./

RUN mkdir src && echo "fn main() {}" > src/main.rs && \
  cargo build --release && \
  rm -rf src

# Copying sources
COPY . .
CMD ["cargo", "run", "--release"]
