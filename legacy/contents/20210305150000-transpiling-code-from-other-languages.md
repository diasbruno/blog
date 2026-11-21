Every time I realize I'm going to duplicate code that is already written somewhere else, I feel sad.

Why not start transpiling code from other languages? 

This is not a new idea, but sometimes we need to remember of what we can do. Code is also data.

The recipe is actually quite simple. Find out a good tool to generate/manipulate the AST (abstract syntax tree) of the language and just walk the tree collecting information and generate some code, string or maybe the AST, in the language you are going to use.

One good example to use this approach: enums.

Input:

```python
from enum import Enum

class Color(Enum):
    RED = 1
    GREEN = 2
    BLUE = 3
```

Output:

```typescript
export default class Color {
    static RED = 1;
    static GREEN = 2;
    static BLUE = 3;
}
```

I want to "avoid the problem" of having to change this file to update the enums by hand. So, all I have to do is run the script that (re)generate the entire file.

```python
# yeah...it doesn't need to be pretty :)
import ast

enums = ast.parse(open('./enums.py', 'r').read())

# filter only class definitions
classes = [kl for kl in enums.body if type(kl) == ast.ClassDef]

# make a simple object of name and all available attributes
# NOTE: if there are other declarations in the class,
# you might need to filter the `options`.
def decode_enum_class(name, options):
    return { "name": name,
             "options": [(opt.targets[0].id, opt.value.s) for opt in options]}

# render a single option, `name = value`, in javascript.
def render_option(option):
    return "  static {} = \"{}\";".format(option[0], option[1])

# render an entire python enum class.
def to_js(enum):
    options = "".join([render_option(opt) + "\n"
                       for opt in enum.get("options")])
    return """
export class {} {{
{}}}
    """.format(enum.get("name"), options)

# composition to make the transformation
# (name, body) |> decode_enum_class |> to_js
transform = lambda name, body: to_js(decode_enum_class(name, body))

# dump to the terminal
print("".join([transform(x.name, x.body) for x in classes]))
```

This simple technique is helpful to avoid typos and keep everything in sync without doing much.
