# CT-Wasm Spec
This repository, forked from the [WebAssembly 
Spec](https://github.com/WebAssembly/spec), contains the CT-Wasm reference 
interpreter, which also implements a rewrite tool that strips secrecy labels and a simple label inference tool.


### Buildilng

The instructions for building and using the reference interpreter may be found in 
the [interpreter](https://github.com/PLSysSec/ct-wasm-spec/tree/master/interpreter) 
directory. We produce two binaries `ct_wasm_spec` and `ct2wasm`.

#### Interpreter

To use the interpreter:

```bash
./ct_wasm_spec
```

### Remove CT-Wasm Annotations
The label removing tool can be invoked by passing in the `-strip` flag:

```bash
./ct2wasm -strip -i file_to_strip.{wat, wasm} -o file_to_output.{wat, wasm}
```

### Infer CT-Wasm Annotations

The label inference tool is part of the interpreter and can be invoked by passing in the `-r` flag:

```bash
./ct_wasm_spec -r file_to_infer.{wat, wasm} -o file_to_output.{wat, wasm}
```
