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

|     Size:     |   int32_list|        int_list|   int_repeated|   int_packed_repeated|
|---------------|-------------|----------------|---------------|----------------------|
|     100:      |      0.00001|         0.00001|        0.00001|               0.00001|
|     1000:     |      0.00079|         0.00006|        0.00006|               0.00004|
|     50000:    |      0.01204|         0.00654|        0.00530|               0.00320|
|     1000000:  |      0.39857|         0.24039|        0.07283|               0.06382|
|     10000000: |      3.78239|         2.14404|        0.77094|               0.62178|

**Conclusion**

The results above are completely expected and can be explained:

* Using `int` is more efficient than `int32` in OCaml. This is because int32 is boxed.
* Using the Runtime `Pbrt.Repeated_field` container improves performance over list. This data imperative data structure is optimzed for push back operation.
* Using `packed` encoding improve performance: As explained by the protobuffer documentation, this encoding is more efficient.
