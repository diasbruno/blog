## Overview

WST, short for Web Server Tools, provides the pieces for a Common Lisp web application. Routes and handlers can share middleware for requests, sessions, and responses. The route and middleware definitions keep the order of those operations visible in the application.

## Route composition

A route can sit inside a resource and share middleware with other endpoints in that resource. This declaration places a GET handler under `/api/health`.

```lisp
(defparameter health-routes
  '(:resource "/api"
    (:route :GET health "/health" health-handler)))
```

The bookmark app linked in the sidebar shows how the pieces fit together in an application.
