FROM ubuntu:24.04 AS nerlnet_base

ARG DEBIAN_FRONTEND=noninteractive
ARG TARGETARCH

ENV RUNNING_IN_DOCKER=true \
    PIP_BREAK_SYSTEM_PACKAGES=1 \
    NERLNET_TORCH_VERSION=2.1.2 \
    NERLNET_TORCH_VARIANT=cpu \
    REPO_ROOT=/workspace/NErlNet

RUN set -eux; \
    install_pkgs="\
      ca-certificates \
      curl \
      wget \
      git \
      unzip \
      zip \
      make \
      gcc \
      g++ \
      cmake \
      libncurses-dev \
      libssl-dev \
      iproute2 \
      python3 \
      python3-pip \
      python3-venv \
      virtualenv \
      nodejs \
      npm \
      erlang \
      rebar3"; \
    if ! (apt-get update && apt-get install -y --no-install-recommends $install_pkgs); then \
      echo "Secure apt flow failed, retrying with insecure repository allowances for this host"; \
      apt-get -o Acquire::AllowInsecureRepositories=true -o Acquire::AllowDowngradeToInsecureRepositories=true update; \
      apt-get -o Acquire::AllowInsecureRepositories=true --allow-unauthenticated install -y --no-install-recommends $install_pkgs; \
    fi; \
    rm -rf /var/lib/apt/lists/*

WORKDIR /workspace/NErlNet
COPY . /workspace/NErlNet

RUN chmod +x \
      /workspace/NErlNet/NerlnetInstall.sh \
      /workspace/NErlNet/NerlnetBuild.sh \
      /workspace/NErlNet/tests/NerlnetFullFlowTorchTest.sh \
      /workspace/NErlNet/tests/set_env.sh \
      /workspace/NErlNet/docker/ubuntu-torch-cpu/run_validation.sh

# Install NErlNet runtime assets.
# On amd64 we can use libtorch archives directly; on arm64 we use pip torch wheels later.
RUN if [ "$TARGETARCH" = "amd64" ]; then \
      /workspace/NErlNet/NerlnetInstall.sh --torch >/tmp/nerlnet_install.log 2>&1; \
    else \
      /workspace/NErlNet/NerlnetInstall.sh >/tmp/nerlnet_install.log 2>&1; \
    fi || (cat /tmp/nerlnet_install.log && false)

# Under emulation, local rebar3 bootstrap may fail; pin /usr/local/bin/rebar3 to apt-installed binary.
RUN ln -sf /usr/bin/rebar3 /usr/local/bin/rebar3

# Ensure no host-generated CMake cache leaks into the container build.
RUN rm -rf /workspace/NErlNet/build/release

FROM nerlnet_base AS nerlnet_runtime
ARG TARGETARCH

# Install Python deps + torch cpu wheel used by model generation scripts.
RUN if [ "$TARGETARCH" = "amd64" ]; then \
      /bin/bash -lc "source /workspace/NErlNet/tests/set_env.sh --torch"; \
    else \
      /bin/bash -lc "source /workspace/NErlNet/tests/set_env.sh --torch-version ${NERLNET_TORCH_VERSION}"; \
    fi

# On arm64 use the pip-installed torch package as the C++ bridge root.
RUN if [ "$TARGETARCH" = "arm64" ]; then \
      TORCH_SITE_DIR="$(python3 -c 'import pathlib, torch; print(pathlib.Path(torch.__file__).resolve().parent)')" && \
      mkdir -p /workspace/NErlNet/build && \
      { \
        echo '#!/bin/bash'; \
        echo "export TORCH_VERSION=\"${NERLNET_TORCH_VERSION}\""; \
        echo 'export TORCH_VARIANT="cpu"'; \
        echo 'export TORCH_ARCHIVE_NAME="pip-wheel"'; \
        echo 'export TORCH_ARCHIVE_SHA256="pip-wheel"'; \
        echo "export TORCH_URL=\"pip://torch==${NERLNET_TORCH_VERSION}\""; \
        echo "export TORCH_ROOT=\"${TORCH_SITE_DIR}\""; \
        echo "export TORCH_INCLUDE_MAIN=\"${TORCH_SITE_DIR}/include\""; \
        echo "export TORCH_INCLUDE_API=\"${TORCH_SITE_DIR}/include/torch/csrc/api/include\""; \
        echo "export TORCH_INCLUDE=\"${TORCH_SITE_DIR}/include:${TORCH_SITE_DIR}/include/torch/csrc/api/include\""; \
        echo "export TORCH_LIB_DIR=\"${TORCH_SITE_DIR}/lib\""; \
        echo "export TORCH_CMAKE_PREFIX=\"${TORCH_SITE_DIR}/share/cmake/Torch\""; \
        echo 'export TORCH_CXX_FLAGS="${TORCH_CXX_FLAGS:--D_GLIBCXX_USE_CXX11_ABI=1}"'; \
        echo ''; \
        echo 'case ":${LD_LIBRARY_PATH:-}:" in'; \
        echo "  *\":${TORCH_SITE_DIR}/lib:\"*) ;;"; \
        echo "  *) export LD_LIBRARY_PATH=\"${TORCH_SITE_DIR}/lib:\${LD_LIBRARY_PATH:-}\";;"; \
        echo 'esac'; \
        echo ''; \
        echo 'case ":${CMAKE_PREFIX_PATH:-}:" in'; \
        echo "  *\":${TORCH_SITE_DIR}/share/cmake/Torch:\"*) ;;"; \
        echo "  *) export CMAKE_PREFIX_PATH=\"${TORCH_SITE_DIR}/share/cmake/Torch:\${CMAKE_PREFIX_PATH:-}\";;"; \
        echo 'esac'; \
      } > /workspace/NErlNet/build/torch_env.sh && \
      chmod +x /workspace/NErlNet/build/torch_env.sh; \
    fi

# Build Torch infrastructure (and source_nif dependency).
RUN /workspace/NErlNet/NerlnetBuild.sh --infra torch -j4 >/tmp/nerlnet_build.log 2>&1 || (cat /tmp/nerlnet_build.log && false)

CMD ["/bin/bash", "/workspace/NErlNet/docker/ubuntu-torch-cpu/run_validation.sh"]
