This repository contains the CT-WASM reference interpreter (`wasm2ct` in the 
paper) and the ct-rewrite tool (`ct2wasm` in the paper). 

## Get the Tools
### Reference Interpreter
The reference interpreter, and the instructions to build it, may be found in 
the [interpreter](https://github.com/PLSysSec/ct-wasm-spec/tree/master/interpreter) 
directory. 

### Rewrite Tool
After building the reference interpreter, the `ct-rewrite` tool can be invoked 
by passing in the `-strip` flag as shown below: 

```lisp
./ctrewrite -strip -i file_to_strip.{wat, wasm} -o file_to_output.{wat, wasm}
```

## Reproduce Results
Navigate to the `eval/` directory in the [`ct-wasm-ports`](https://github.com/PLSysSec/ct-wasm-ports) 
repository
