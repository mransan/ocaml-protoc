## Benchmarking 

#### Test Case 01 - Int list and OCaml extensions  

In this benchmark we analyze the various way design the `.proto` message to encode an
list of 32 bit integer:

**Default**

Using the OCaml `int32` and `list` type:
```Javascript
repeated int32 l = 1;
```

**ocaml_type = int_t extension**

Using the `int` and `list` OCaml type: 
```Javascript
repeated int32 l = 1 [(ocaml_type)=int_t];
```

**ocaml_type = int_t / ocaml_container = repeated_field extensions**

Using `int` type and the Runtime container `Pbrt.Repeated_field`:
```Javascript
repeated int32 l = 1 [(ocaml_type) = int_t, (ocaml_container) = repeated_field];
```

**Packed encoding and all ocaml extensions**

```Javascript
repeated int32 l = 1 [(ocaml_type) = int_t, (ocaml_container) = repeated_field];
```

**Results in seconds for Decoding**

Environment configuration:

```
OS: OSX Version 10.9.5
Processor: 1.4 Ghz Intel Core i5
Memory: 4 GB 1600 MHz DDR3
```

Results:

|     Size:     |   int32_list|        int_list|   int_repeated|   int_packed_repeated|
|---------------|-------------|----------------|---------------|----------------------|
|      100      |      0.00001|         0.00001|        0.00001|               0.00001|
|      1000     |      0.00059|         0.00008|        0.00016|               0.00006|
|      50000    |      0.01324|         0.00995|        0.00593|               0.00375|
|      1000000  |      0.34267|         0.23039|        0.08685|               0.05702|
|      10000000 |      3.47812|         2.35051|        0.81929|               0.67300|

**Conclusion**

The results above are completely expected and can be explained:

* Using `int` is more efficient than `int32` in OCaml. This is because int32 is boxed.
* Using the Runtime `Pbrt.Repeated_field` container improves performance over list. This data imperative data structure is optimzed for push back operation.
* Using `packed` encoding improve performance: As explained by the protobuffer documentation, this encoding is more efficient.
