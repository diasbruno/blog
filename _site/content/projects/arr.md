## Overview

arr provides background workers for Common Lisp. Its generic interfaces cover scheduling, enqueueing, dequeueing, and dispatching tasks. Applications can supply a worker or storage implementation suited to their workload.

## Worker lifecycle

The background-worker API returns an application value that can be passed to the stop function. This starts two workers and makes shutdown explicit.

```lisp
(defparameter *workers*
  (arr.background-worker:start-application :number-of-workers 2))

(arr.background-worker:stop-application *workers*)
```
