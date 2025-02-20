# Introdução

Em várias linguagens temos operadores booleanos que convertem valores para definir se é um valor que é considerado verdadeiro ou falso.

Em algumas, como python, objetos vazios e arrays vazias podem ser convertidas pra valores booleanos.

```py
 bool({})                             # false
 bool({ 'a': 1 })                     # True
 bool({ 'a': 1 }) == bool({ 'a': 2 }) # True
```

Podemos define uma “interface" (note os “”) como essa em Haskell:

```hs
class Applicative f => Alternative f where
  empty :: f a               -- elemento neutro
  (<|>) :: f a -> f a -> f a -- é uma operação que combina 2 valores
```

Os supostos objetos que implementam essa “interface" deve seguir alguns axiomas:

```hs
empty     <|> empty     = empty
empty     <|> something = something
something <|> empty     = something
```

Caso os 2 objetos sejam não-vazios podemos combinar as “computações”. Essa classe tem como objetivo analisar os casos para que, quando não vazios possam ser combinados de alguma forma.

Poderíamos analisar objetos em python como:

```py
# essa operação para `{}` (objeto), uma das possíveis operações é `update`.
a <|> b = a.update(b)

# `{}` (objeto) é elemento neutro com a operação de `update` 
# pois não influe no resultado
empty                     = {}

empty      <|> empty      = a.update(b) = {}
empty      <|> { 'a': 1 } = a.update(b) = { 'a': 1 }
{ 'a': 1 } <|> empty      = a.update(b) = { 'a': 1 }
{ 'a': 1 } <|> { 'b': 2 } = a.update(b) = { 'a': 1, 'b': 2 }
```

Nesse caso estamos “roubando” pois objeto não segue a risca algumas propriedades (não vamos discutir aqui). Mas podemos fazer completar de modo a mostrar essas propriedades.

Podemos utilizar o type *Optional[X]  (Union[X, None])*.

```py
def toOptional(x):
    return bool(x) and x or None

# nesse caso podemos utilizar a operação `and`,
# se o objeto `a` "existir" usaremos ele, do contrário usaremos `b`
a <|> b = a and b

empty                     = toOptional({})

empty                  <|> empty                  = {}
empty                  <|> toOptional({ 'a': 1 }) = b
toOptional({ 'a': 1 }) <|> empty                  = a
toOptional({ 'a': 1 }) <|> toOptional({ 'b': 2 }) = a
```

Descartamos o segundo item,  pois podemos ter apenas um objeto.

Existem outras propriedades dessa tal alternative, mas o resto você pode estudar.
