<blockquote class="research-warning">
  <p><strong>Warning</strong></p>
  <p>Verona is still in research.</p>
</blockquote>

## A small program

Verona writes types alongside parameter names. This example defines a function over two 32-bit integers, then uses it from the program’s entry point.

```lisp
(import base)

(base:function total
  ((quantity i32) (unit-price i32))
  i32
  (* quantity unit-price))

(base:function main () exit-code
  (total 3 7))
```

The program exits with status 21.

## From source to native code

The compiler can check a program with `verona check` before you build a native executable with `verona compile`. Verona uses LLVM for native code generation. It can also produce libraries for use from C.

S-expressions let macros work directly with the structure of a program. The compiler carries source locations through code generation so diagnostics can point back to the original forms.
