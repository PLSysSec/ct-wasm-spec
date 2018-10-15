<img src="./logo.png"/>

------------
This repository, forked from the [WebAssembly
Spec](https://github.com/WebAssembly/spec), contains additions to the
reference interpreter that allow for the encoding of constant-time
computations suitable for crypto.

NOTE: As a reference implementation, no guarantees are provided about the
execution timing of the extensions.

Conforming implementations must ensure that all operations on the s32 and s64
types be implemented in constant time. Otherwise, the typing rules here
presented will ensure the constant timedness of all programs over secrets.


## Resources
### Binary Distribution
For convenience, we provide [binary
releases](https://github.com/PLSysSec/ct-wasm-spec/releases/artifact) for
macOS and 64 bit Linux. Other platforms should work, but are untested and
will need to build from source.

These releases contain 3 binaries:
 - `ct_wasm_spec`: a build of the spec interpreter that also supports primitive secrecy inference through the `-r` flag.
 - `ct_node`: a version of node.js that natively supports the use of CT-Wasm. This version has been measured by dude-ct to provide constant-time guarantees.
 - `ct2wasm`: a build of the spec interpreter that supports secrecy stripping through the `-strip` flag.

#### Chromium Builds
Chromium is notoriously difficult to maintain a fork for, so we provide a prebuilt binary. It uses the same modifications performed in the V8 subdirectory of our node interpreter.

### Source Distribution
CT-Wasm efforts are split across a few different repositories:
 - [`ct-wasm-spec`](https://github.com/PLSysSec/ct-wasm-spec): The spec interpreter
 - [`ct-wasm-node`](https://github.com/PLSysSec/ct-wasm-node): An implementation in Node/V8
 - [`ct-wasm-ports`](https://github.com/PLSysSec/ct-wasm-ports): Algorithm implementations and evaluation scripts

#### Using the source
For building from source, we recommend pulling down
[`ct-wasm-ports`](https://github.com/PLSysSec/ct-wasm-ports) and running
`make tools` from the `eval` directory. This will automatically pull and
build the relevant repositories.

The eval directory also provides a single command to collect all data to
replicate the Evaluation section of the POPL 2019 paper. More details can be
found in the `eval` directory's `README`.


## Summary of spec changes

### New Types
 - `s32`: Secret 32 bit integer
 - `s64`: Secret 64 bit integer

These types come with all integer operations except `div` and `rem` which are
notoriously non-CT and can leak information through partiality.

### New Memory Type
Memories can be either secret or public.

They are declared in text as:
`(memory secret 0 10)`

Secret memories accept and produce secret values but require public indices for stores and loads

```lisp
(module
    (memory secret 1)

    (func $store_example
        (s32.store (i32.const 0) (s32.const 1))))
```

### Declassification
Declassification allows the relabeling of secret data as public. This is inherently unsound but important to operations such as encryption which produce a safely public value out of a secret one.

This is performed by the two operators:
 - `i32.declassify`
 - `i64.declassify`

These operators are only allowed inside **trusted functions**. By default, functions are trusted.
Trust is built into the type of a function like so:

```lisp
(func utrusted (param s32) (result i32)
    ...)
```
