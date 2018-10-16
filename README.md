# CT-Wasm Spec
This repository, forked from the [WebAssembly 
Spec](https://github.com/WebAssembly/spec), contains the CT-Wasm reference 
interpreter, which also implements a rewrite tool that strips secrecy labels.


### Interpreter
The instructions for building and using the reference interpreter may be found in 
the [interpreter](https://github.com/PLSysSec/ct-wasm-spec/tree/master/interpreter) 
directory. The produced binary that implements CT-Wasm is called `ct2wasm`. (This name will
be updated to not be confused with the label stripping tool that is simply a flag to this binary.)

### Remove CT-Wasm Annotations
The reference interpreter also implements the label removing tool, and can be invoked 
by passing in the `-strip` flag as shown below: 

```bash
./ct2wasm -strip -i file_to_strip.{wat, wasm} -o file_to_output.{wat, wasm}
```
