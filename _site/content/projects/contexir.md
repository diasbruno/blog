## Overview

Contexir brings context-oriented programming to Elixir. A layer can add behavior before or after a function, or wrap the next implementation and decide when to continue. Callers can activate layers explicitly or select them with context values.

## A small example

An inventory function can keep its base behavior while a separate layer reports when the count changes.

```elixir
defmodule Inventory do
  use Contexir

  def restock(count, amount, _context), do: count + amount
end

import Contexir.Layer

deflayer ReportRestock do
  refine Inventory do
    defpartial restock(_count, amount, _context), mode: :before do
      IO.puts("Adding #{amount} items")
    end
  end
end
```

Layer activation is local to the BEAM process, so a newly spawned process can choose its own context.
