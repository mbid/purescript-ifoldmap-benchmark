Benchmarks different implementions of `foldMap` and `ifoldMap`.

`foldMap`, `ifoldMap`, `traverse` and `itraverse` all have separate FFI
implementations here.

```bash
> npm install && bower install
> pulp build --optimise --to main.js && node main.js
```

## `foldMap`
![foldMap Benchmarks](results/sum.png?raw=True)

## `ifoldMap`
![ifoldMap Benchmarks](results/indexed-sum.png?raw=True)
