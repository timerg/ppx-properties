# ppx-properties
ppx for record in reasonml


## Usage
```
[@deriving properties]
type my_typ = {
  foo: int,
  bar: string,
  nested: option(my_typ),
};
```

this will auto generate 1 type and 3 functions from the record type
```
type field('_) = 
  | Foo: field(int)
  | Bar: field(string)
  | Nested: field(option(my_type));
  
let get: (field('v), my_type) => 'v;

let set: (field('v), 'v my_type) => my_type;

let update: (field('v), 'v => 'v, my_type) => my_type;
```

## Install
Install this github package by ssh:

1. Add your public key to this project's Deploy key.
2. `yarn add -D ssh://git@github.com/Tsaitung/ppx-properties.git#v0.1.1`
3. add `ppx_properties/ppx` to field 'ppx-flags' in your bsconfig.json


## Credit
This project use GADT. The idea comes from [Astrocoders/lenses-ppx](https://github.com/Astrocoders/lenses-ppx)
