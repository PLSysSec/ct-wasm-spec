<img src="./logo.png"/>

------------
# ct-wasm-spec
This repository, forked from the [WebAssembly 
Spec](https://github.com/WebAssembly/spec), contains the CT-Wasm reference 
interpreter, which also implements a `ct-rewrite` tool (`ct2wasm` from the 
paper). 

### Get the Interpreter
The instructions for building and using the reference interpreter may be found in 
the [interpreter](https://github.com/PLSysSec/ct-wasm-spec/tree/master/interpreter) 
directory. 

The `ct-rewrite` tool can be invoked by passing in the `-strip` flag 
as shown below: 

```lisp
./ctrewrite -strip -i file_to_strip.{wat, wasm} -o file_to_output.{wat, wasm}
```
