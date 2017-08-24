FROM yangby/python3:latest

LABEL maintainer YangBy <yby@yangby.org>

RUN    echo "==> ==> ==> ==> ==> ==> ==> ==> ==> ==> ==> ==> "          \
    && echo "==> Install packages ..."                                  \
    && export DEBIAN_FRONTEND=noninteractive                            \
    && sudo apt-get -y update                                           \
    && sudo apt-get -y upgrade                                          \
    && sudo apt-get -y install                                          \
        --no-install-recommends --no-install-suggests                   \
        build-essential                                                 \
    && echo "==> Installing Python Libraries ..."                       \
    && need_libs=""                                                     \
    && need_libs="${need_libs} sanic"                                   \
    && need_libs="${need_libs} gunicorn"                                \
    && sudo pip3 install                                                \
        --disable-pip-version-check                                     \
        --no-cache-dir                                                  \
        ${need_libs}                                                    \
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

CMD ["--config", "etc/gunicorn.conf", "run:app"]
ENTRYPOINT ["gunicorn"]
