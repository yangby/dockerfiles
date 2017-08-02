FROM yangby/sbcl:1.3.20

LABEL maintainer YangBy <yby@yangby.org>

RUN    echo "==> ==> ==> ==> ==> ==> ==> ==> ==> ==> ==> ==> "          \
    && echo "==> Install packages ..."                                  \
    && export DEBIAN_FRONTEND=noninteractive                            \
    && sudo apt-get -y update                                           \
    && sudo apt-get -y upgrade                                          \
    && sudo apt-get -y install                                          \
        --no-install-recommends --no-install-suggests                   \
        openssl curl ca-certificates gnupg                              \
    && echo "==> Set environment variables ..."                         \
    && INSTALL_DIR=${HOME}/quicklisp                                    \
    && TMP_DIR=$(mktemp -d)                                             \
    && export GNUPGHOME="$(mktemp -d)"                                  \
    && echo "==> Download Quicklisp ..."                                \
    && curl "https://beta.quicklisp.org/quicklisp.lisp"                 \
        > ${TMP_DIR}/quicklisp.lisp                                     \
    && curl "https://beta.quicklisp.org/quicklisp.lisp.asc"             \
        > ${TMP_DIR}/quicklisp.lisp.asc                                 \
    && curl "https://beta.quicklisp.org/release-key.txt"                \
        > ${TMP_DIR}/release-key.txt                                    \
    && gpg --import ${TMP_DIR}/release-key.txt                          \
    && gpg --verify ${TMP_DIR}/quicklisp.lisp.asc                       \
        ${TMP_DIR}/quicklisp.lisp                                       \
    && echo "==> Install Quicklisp ..."                                 \
    && sbcl --noinform --noprint --non-interactive                      \
        --load ${TMP_DIR}/quicklisp.lisp                                \
        --eval                                                          \
            "(quicklisp-quickstart:install :path \"${INSTALL_DIR}\")"   \
        --eval "(ql-util:without-prompting (ql:add-to-init-file))"      \
    && echo "==> Updating Lisp Packages ..."                            \
    && sbcl --noinform --noprint --non-interactive                      \
        --eval "(ql:update-client)"                                     \
        --eval "(ql-util:without-prompting (ql:update-all-dists))"      \
    && echo "==> Remove temporary directory and files ..."              \
    && rm -rfv ${TMP_DIR} ${GNUPGHOME}                                  \
    && find "${INSTALL_DIR}/" -name "*.fasl" -exec rm -v {} \;          \
    && echo "==> Remove unnessary packages in runtime ..."              \
    && sudo apt-get -y remove --purge                                   \
        openssl curl ca-certificates gnupg                              \
    && sudo apt-get -y autoremove --purge                               \
    && sudo apt-get -y autoclean                                        \
    && sudo apt-get -y clean                                            \
    && sudo rm -rfv /var/lib/apt/lists/*                                \
    && echo "==> ==> ==> ==> ==> ==> ==> ==> ==> ==> ==> ==> "
