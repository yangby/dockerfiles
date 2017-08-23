FROM yangby/sbcl-quicklisp:latest

LABEL maintainer YangBy <yby@yangby.org>

RUN    echo "==> ==> ==> ==> ==> ==> ==> ==> ==> ==> ==> ==> "          \
    && echo "==> Install packages ..."                                  \
    && export DEBIAN_FRONTEND=noninteractive                            \
    && sudo apt-get -y update                                           \
    && sudo apt-get -y upgrade                                          \
    && sudo apt-get -y install                                          \
        --no-install-recommends --no-install-suggests                   \
        build-essential gcc libev4                                      \
    && echo "==> Set environment variables ..."                         \
    && INSTALL_DIR=${HOME}/quicklisp                                    \
    && echo "==> Installing Lisp Libraries ..."                         \
    && need_libs=""                                                     \
    && need_libs="${need_libs} clack"                                   \
    && need_libs="${need_libs} woo"                                     \
    && need_libs="${need_libs} caveman2"                                \
    && need_libs="${need_libs} envy"                                    \
    && need_libs="${need_libs} djula"                                   \
    && need_libs="${need_libs} datafly"                                 \
    && need_libs="${need_libs} websocket-driver-server"                 \
    && install_cmd="sbcl --noinform --noprint --non-interactive"        \
    && install_cmd="${install_cmd} --eval \"(ql:update-client)\""       \
    && install_cmd="${install_cmd} --eval \"(ql-util:without-prompting (ql:update-all-dists))\"" \
    && for lib in ${need_libs}; do                                      \
        install_cmd="${install_cmd} --eval \"(ql-util:without-prompting (ql:quickload :${lib}))\""; \
        done                                                            \
    && eval ${install_cmd}                                              \
    && echo "==> Remove unnessary packages in runtime ..."              \
    && sudo apt-get -y remove --purge                                   \
        build-essential                                                 \
    && sudo apt-get -y autoremove --purge                               \
    && sudo apt-get -y autoclean                                        \
    && sudo apt-get -y clean                                            \
    && sudo rm -rf /var/lib/apt/lists/*                                 \
    && echo "==> ==> ==> ==> ==> ==> ==> ==> ==> ==> ==> ==> "

COPY webapp /mnt/work
RUN sudo chown -R stdusr:stdusr /mnt/work

EXPOSE 8000

ENV SERVER_PORT 8000
ENV SERVER_ROOT /mnt/work

CMD ["--noinform", "--load", "run.lisp", "--quit"]
ENTRYPOINT ["sbcl"]
