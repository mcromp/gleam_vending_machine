# phantom types
 - don't exsist at runtime
 -> they exsist for compile
 -> good for:
  - Id's

 generic:
  type Something(T){ A(T)} -> A takes a generic "T"

 phantom types are unused but the TYPE CALLERS REFINE what the type's type 
##  they're good for validation:
```
 fn (a: Something(Invalid)) -> Something(Valid) // phantom types can restrict apis so you can focus on happy path
 ```



# traits




<!--  switch cases weren't working -->
