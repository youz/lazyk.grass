# lazyk.grass

Lazy K interpreter in Grass language


## How to build

You need [grasspiler](https://www.npmjs.com/package/@susisu/grasspiler)

```
$ grasspile lazyk.ml -o lazy-k.grass
```


## Usage

```
$ grass lazyk.grass < sample-fizzbuzz.grass
```

## Limitations

- support Unlambda-style syntax only
- don't accept extra characters in the source code, even spaces or line breaks
