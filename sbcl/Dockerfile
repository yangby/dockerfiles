FROM debian:testing-slim

LABEL maintainer YangBy <yby@yangby.org>

ENV SBCL_HOME "/opt/lib/sbcl"
ENV ASDF_OUTPUT_TRANSLATIONS "/:"
ENV PATH "/opt/bin:${PATH}"

RUN    echo "==> ==> ==> ==> ==> ==> ==> ==> ==> ==> ==> ==> "          \
    && echo "==> Configure SBCL ..."                                    \
    && SBCL_VER=1.3.20                                                  \
    && SBCL_ARCH=x86-64                                                 \
    && SBCL_PLAT=linux                                                  \
    && SBCL_FULLVER=sbcl-${SBCL_VER}-${SBCL_ARCH}-${SBCL_PLAT}          \
    && SBCL_BIN=${SBCL_FULLVER}-binary                                  \
    && SBCL_FILE=${SBCL_BIN}.tar.bz2                                    \
    && SBCL_URL=https://prdownloads.sourceforge.net/sbcl/${SBCL_FILE}   \
    && echo "==> Set environment variables ..."                         \
    && INSTALL_ROOT=/opt                                                \
    && TMP_DIR=$(mktemp -d)                                             \
    && echo "==> Install packages ..."                                  \
    && export DEBIAN_FRONTEND=noninteractive                            \
    && apt-get -y update                                                \
    && apt-get -y upgrade                                               \
    && apt-get -y install                                               \
        --no-install-recommends --no-install-suggests                   \
        sudo openssl curl make bzip2 ca-certificates                    \
    && echo "==> Create temporary directories ..."                      \
    && mkdir ${TMP_DIR}/pkgs                                            \
    && curl -L -o "${TMP_DIR}/pkgs/${SBCL_FILE}"                        \
        "${SBCL_URL}"                                                   \
    && mkdir ${TMP_DIR}/sbcl                                            \
    && bzip2 -cd "${TMP_DIR}/pkgs/${SBCL_FILE}"                         \
        | tar xf - -C "${TMP_DIR}"                                      \
    && cd ${TMP_DIR}/${SBCL_FULLVER}                                    \
    && SBCL_HOME="${SBCL_HOME}" INSTALL_ROOT="${INSTALL_ROOT}"          \
        sh "install.sh"                                                 \
    && echo "==> Remove unnessary packages in runtime ..."              \
    && apt-get -y remove --purge                                        \
        openssl curl make bzip2 ca-certificates                         \
    && apt-get -y autoremove --purge                                    \
    && apt-get -y autoclean                                             \
    && apt-get -y clean                                                 \
    && echo "==> Remove temporary directories ..."                      \
    && rm -rf ${TMP_DIR}                                                \
    && rm -rf /var/lib/apt/lists/*                                      \
    && echo "==> Add a standard user ..."                               \
    && addgroup --gid 1000 stdusr                                       \
    && useradd --create-home --shell /bin/bash                          \
        --uid 1000 --gid 1000 stdusr                                    \
    && echo "==> Grant privileges to the standard user ..."             \
    && echo "stdusr ALL=(ALL) NOPASSWD:ALL" > /etc/sudoers.d/custom     \
    && chmod 400 /etc/sudoers.d/custom                                  \
    && echo "==> ==> ==> ==> ==> ==> ==> ==> ==> ==> ==> ==> "

USER stdusr
WORKDIR /mnt/work
ENTRYPOINT ["sbcl"]
