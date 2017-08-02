# dockerfiles

Dockerfiles for my docker images.

## Dockers

All my docker images can be found in [my page][my-docker-hub-url] at Docker
Hub Registry.

[my-docker-hub-url]: https://hub.docker.com/r/yangby/

### nginx-extras

The [official Nginx docker images][nginx-docker-url] do NOT provide third-party
modules (issues： [#116][nginx-issue-116-url], [#162][nginx-issue-162-url],
[#178][nginx-issue-178-url]).

[nginx-docker-url]: https://github.com/nginxinc/docker-nginx
[nginx-issue-116-url]: https://github.com/nginxinc/docker-nginx/issues/116
[nginx-issue-162-url]: https://github.com/nginxinc/docker-nginx/issues/162
[nginx-issue-178-url]: https://github.com/nginxinc/docker-nginx/issues/178

### sbcl

Whenever you run sbcl, you will see the follow warning message:

```
WARNING:
Couldn't re-execute SBCL with proper personality flags (/proc isn't mounted? setuid?)
Trying to continue anyway.
```

The simplest way is running docker with `--security-opt=seccomp=unconfined`
to avoid it.

### sbcl-quicklisp
