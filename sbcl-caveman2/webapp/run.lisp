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
