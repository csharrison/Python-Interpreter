The final project for CS173 - An interpreter for Python

Our core difference from ParselTongue is the use of EPS, Environment Passing Style.

Because in Python any expression can introduce a new identifier, we pass the any new environments on to any next expression, just like the store.

The exception case is the CApp case, in which case we use the callers environment to return to, not the callees.


design considerations:
    we need some sort of O(1) iterable in plai-typed to use for our iterables
    Pythons lists are definitely constant time lookup, so we can't use (list) s
    
    Classes are just functions that are object-makers
    
    

Tests we pass:
    we've implemented basic functions, with parseltongue like scope
    we implemented default arguments to procedures
    we have basic string and number comparators
    we have basic binary operations on strings (+) and numbers (+ - * / //)
    
    we implemented boolean operations (and or) via if desugaring
    
