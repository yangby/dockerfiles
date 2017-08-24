FROM debian:testing-slim

LABEL maintainer YangBy <yby@yangby.org>

ENV PATH "/opt/bin:${PATH}"

RUN    echo "==> ==> ==> ==> ==> ==> ==> ==> ==> ==> ==> ==> "          \
    && echo "==> Configure PYTHON ..."                                  \
    && PYTHON_VER=3.6.2                                                 \
    && PYTHON_FULLVER=Python-${PYTHON_VER}                              \
    && PYTHON_FILE=${PYTHON_FULLVER}.tgz                                \
    && PYTHON_URL=https://www.python.org/ftp/python/${PYTHON_VER}/${PYTHON_FILE} \
    && echo "==> Set environment variables ..."                         \
    && INSTALL_ROOT=/opt                                                \
    && TMP_DIR=$(mktemp -d)                                             \
    && export GNUPGHOME="$(mktemp -d)"                                  \
    && echo "==> Install packages ..."                                  \
    && export DEBIAN_FRONTEND=noninteractive                            \
    && apt-get -y update                                                \
    && apt-get -y upgrade                                               \
    && apt-get -y install                                               \
        --no-install-recommends --no-install-suggests                   \
        sudo openssl curl ca-certificates gnupg                         \
        build-essential pkg-config                                      \
        cpio file                                                       \
        libexpat1-dev libffi-dev libmpdec-dev                           \
        libexpat1     libffi6    libmpdec2                              \
        libgdbm-dev libsqlite3-dev libssl-dev                           \
        libgdbm3    libsqlite3-0   libssl1.1                            \
        libncurses5-dev libncursesw5-dev                                \
        libncurses5     libncursesw5                                    \
        libbz2-dev liblzma-dev zlib1g-dev                               \
        libbz2-1.0 liblzma5    zlib1g                                   \
        tcl8.6-dev tk8.6-dev libreadline-dev                            \
        libtcl8.6  libtk8.6  libreadline7                               \
    && echo "==> Create a temporary directory ..."                      \
    && mkdir ${TMP_DIR}/pkgs                                            \
    && echo "==> Install Python ..."                                    \
    && curl -L -o "${TMP_DIR}/pkgs/${PYTHON_FILE}"                      \
        "${PYTHON_URL}"                                                 \
    && curl -L -o "${TMP_DIR}/pkgs/${PYTHON_FILE}.asc"                  \
        "${PYTHON_URL}.asc"                                             \
    && curl -L -o "${TMP_DIR}/pkgs/pubkeys.txt"                         \
        "https://www.python.org/static/files/pubkeys.txt"               \
    && gpg --import "${TMP_DIR}/pkgs/pubkeys.txt"                       \
    && gpg --verify "${TMP_DIR}/pkgs/${PYTHON_FILE}.asc"                \
    && mkdir ${TMP_DIR}/python                                          \
    && tar -xzf "${TMP_DIR}/pkgs/${PYTHON_FILE}" -C ${TMP_DIR}          \
    && cd ${TMP_DIR}/${PYTHON_FULLVER}                                  \
    && ./configure --prefix=${INSTALL_ROOT}                             \
        --build="$(dpkg-architecture --query DEB_BUILD_GNU_TYPE)"       \
        --enable-shared                                                 \
        --disable-profiling                                             \
        --enable-optimizations                                          \
        --enable-loadable-sqlite-extensions                             \
        --disable-ipv6                                                  \
        --without-pydebug                                               \
        --without-assertions                                            \
        --without-lto                                                   \
        --with-system-expat                                             \
        --with-system-ffi                                               \
        --with-system-libmpdec                                          \
        --with-threads                                                  \
        --without-doc-strings                                           \
        --with-fpectl                                                   \
        --without-computed-gotos                                        \
        --without-ensurepip                                             \
    && make install                                                     \
    && echo "PATH=\"${INSTALL_ROOT}/bin:\${PATH}\""                     \
        >> /etc/profile.d/custom.sh                                     \
    && . /etc/profile.d/custom.sh                                       \
    && echo "${INSTALL_ROOT}/lib"                                       \
        >> /etc/ld.so.conf.d/custom.conf                                \
    && ldconfig                                                         \
    && echo "==> Install pip ..."                                       \
    && curl "https://bootstrap.pypa.io/get-pip.py"                      \
        > ${TMP_DIR}/get-pip.py                                         \
    && python3 ${TMP_DIR}/get-pip.py                                    \
        --disable-pip-version-check                                     \
        --no-cache-dir                                                  \
    && echo "==> Remove unnessary packages in runtime ..."              \
    && apt-get -y remove --purge                                        \
        curl gnupg                                                      \
        build-essential pkg-config                                      \
        cpio file                                                       \
        libexpat1-dev libffi-dev libmpdec-dev                           \
        libgdbm-dev libsqlite3-dev libssl-dev                           \
        libncurses5-dev libncursesw5-dev                                \
        libbz2-dev liblzma-dev zlib1g-dev                               \
        tcl8.6-dev tk8.6-dev libreadline-dev                            \
    && apt-get -y autoremove --purge                                    \
    && apt-get -y autoclean                                             \
    && apt-get -y clean                                                 \
    && echo "==> Remove temporary directories and files ..."            \
    && rm -rf ${TMP_DIR} ${GNUPGHOME}                                   \
    && rm -rf /var/lib/apt/lists/*                                      \
    && find ${INSTALL_ROOT}/                                            \
        -type d -a \( -name test -o -name tests -o -name __pycache__ \) \
        -exec rm -rf '{}' +                                             \
    && echo "==> Add a standard user ..."                               \
    && addgroup --gid 1000 stdusr                                       \
    && useradd --create-home --shell /bin/bash                          \
        --uid 1000 --gid 1000 stdusr                                    \
    && echo "==> Grant privileges to the standard user ..."             \
    && echo "stdusr ALL=(ALL) NOPASSWD:ALL"                             \
        > /etc/sudoers.d/custom                                         \
    && echo "Defaults secure_path=\"/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:${INSTALL_ROOT}/bin\"" \
        >> /etc/sudoers.d/custom                                        \
    && chmod 400 /etc/sudoers.d/custom                                  \
    && echo "==> ==> ==> ==> ==> ==> ==> ==> ==> ==> ==> ==> "

USER stdusr
WORKDIR /mnt/work
ENTRYPOINT ["python3"]
