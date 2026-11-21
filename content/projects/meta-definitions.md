## Overview

Meta-definitions describes the slots and annotations of a domain type in Common Lisp. The definition can then drive more than one artifact. A slot can carry a JSON key, a database column name, or a validation rule, and a derived type can select a subset of the original slots.

## A small example

This definition supplies metadata for an account's identifier and generates a Common Lisp class and readers.

```lisp
(meta:define
  (class (account :table-name "accounts"))
  (slots (id :json-key "id" :table-column "account_id")))

(meta:define-class account)
(meta:derive-readers account)
```

A create or update request can reuse the relevant fields from the domain definition, keeping their annotations in one place.
