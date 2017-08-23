FROM debian:testing-slim

LABEL maintainer YangBy <yby@yangby.org>

RUN    echo "==> ==> ==> ==> ==> ==> ==> ==> ==> ==> ==> ==> "          \
    && echo "==> Update packages ..."                                   \
    && apt-get -y update                                                \
    && apt-get -y upgrade                                               \
    && echo "==> Install packages ..."                                  \
    && export DEBIAN_FRONTEND=noninteractive                            \
    && apt-get -y install                                               \
        --no-install-recommends --no-install-suggests                   \
        nginx-extras                                                    \
    && echo "==> Remove unnessary packages ..."                         \
    && apt-get -y autoremove --purge                                    \
    && apt-get -y autoclean                                             \
    && apt-get -y clean                                                 \
    && echo "==> Remove temporary files ..."                            \
    && rm -rf /var/lib/apt/lists/*                                      \
    && echo "==> Add a standard user ..."                               \
    && addgroup --gid 1000 stdusr                                       \
    && useradd --uid 1000 --gid 1000 stdusr                             \
    && echo "==> Forward logs to docker log collector ..."              \
    && ln -sf /dev/stdout /var/log/nginx/access.log                     \
    && ln -sf /dev/stderr /var/log/nginx/error.log                      \
    && echo "==> ==> ==> ==> ==> ==> ==> ==> ==> ==> ==> ==> "

EXPOSE 80 443

STOPSIGNAL SIGTERM

CMD ["-g", "daemon off;"]
ENTRYPOINT ["nginx"]
