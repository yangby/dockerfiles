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

### sbcl-caveman2

Put your webapp into `/mnt/work` (in docker) and write a `/mnt/work/run.lisp`
(in docker) to start the server.

There is a simple demo in the docker. The demo is created by the follow
commands:

```lisp
(ql:quickload :caveman2)
(caveman2:make-project #P"/path/to/webapp/" :author "AuthorName")
```

Add the `run.lisp` into `/path/to/webapp`:

```lisp
(require "asdf")
(asdf:disable-output-translations)

(defvar *address* "0.0.0.0")
(defvar *port* (parse-integer (asdf::getenv "SERVER_PORT")))
(defvar *server-root*
  (pathname (concatenate
             'string (asdf::getenv "SERVER_ROOT") "/")))
(push *server-root* asdf:*central-registry*)

(require "webapp")
(webapp:start
 :server :woo :address *address* :port *port* :use-thread nil)
```

There are two important environment variables:

* `SERVER_PORT`: default 8000.
* `SERVER_ROOT`: default `/mnt/work`.

Start the docker as follows:

```bash
docker run --rm -d --name caveman2-instance -p 0.0.0.0:80:8000 \
    --security-opt=seccomp=unconfined -v /data/caveman2-webapp:/mnt/work \
    yangby/sbcl-caveman2:latest
```
