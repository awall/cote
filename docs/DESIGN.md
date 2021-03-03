Killer Feature
---------------
Compile time evaluation. CoTE's killer feature is powerful compile-time evaluation. This is general enough that features like generics and inheritance can just be implemented as macros.


Mandatory Features
------------------
* __First class types__ are necessary so that we can create new types from
  other code at compile time.
* __Reflection, available at compile-time__ lets us inspect existing code
  to create useful macros that for instance, create a new type by adding
  or removing fields from another type.
* __Syntax for manipulating ASTs__ will allow far easier development of
  compile-time macros.
* __Computed ASTs that can be 'spliced into the code'__ will be what brings
  everything together in terms of using macros. A macro can be simply a
  function which returns an AST, and 'invoking a macro' is just calling
  this function at compile-time, and splicing its result into the 
  post-macro-expansion AST.