## Overview

Verona uses S-expressions for a native systems language. Source forms can be manipulated as data, which gives macros a place in the language alongside explicit types, pattern matching, and generic dispatch. I contribute to the project.

<blockquote class="research-warning">
  <p><strong>Warning</strong></p>
  <p>Verona is still in research.</p>
</blockquote>

## A small program

This program uses the base library and returns 42 as its process exit status.

```lisp
(import base)

(base:function main () exit-code
  (+ 40 2))
```

## Design

The compiler targets LLVM and keeps source locations through analysis and native code generation so diagnostics can refer back to the original source. The toolchain can emit native libraries, and C interoperability is part of the language's design.
