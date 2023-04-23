(mapc (lambda (load)
	(push load ql:*local-project-directories*))
      '("/usr/local/src/cl-unac/"
	"/usr/local/src/cl-slugify/"
	"/usr/local/src/spinneret/"
	"/usr/local/src/site-content/"))

(ql:quickload :cl-unac.config)

(cl-unac.config:load-from-custom-path
 "/usr/lib/x86_64-linux-gnu/libunac")

(ql:quickload :diasbruno)

(setf spinneret:*always-quote* t
      spinneret:*html-style* :human
      spinneret:*html-lang* "en-US"
      local-time:*default-timezone* local-time:+utc-zone+)
