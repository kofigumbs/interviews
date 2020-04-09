# Quick Start

1. Install [Stack](https://docs.haskellstack.org/en/stable/README/): `curl -sSL https://get.haskellstack.org/ | sh`
2. Run `stack build --exec 'frostl fixtures/moon.stl'`


# Design

I chose Haskell because the I find it to be a pleasant language for parsing problems.
I made an effort to annotate any particularly Haskell-y idiosyncrasies.

Since I computed the metrics incrementally within the parser, there shouldn't be any glaring memory leaks.
For instance, I never have multiple parsed facets in memory.
Of course, this being Haskell, there are always hidden memory leaks.
If this solution didn't work quickly enough for millions of triangles, I'd start looking into decisions around lazy vs. strict.
Parsec includes modules for each of major string types, so I imagine there is low-hanging fruit by switching to Text or ByteString.
I've also only used lazy data types and hand-rolled the math.
I know that Haskell libraries exist to make these structures more performant (for instance, the `vector` package).
I didn't include them in this version because I wasn't sure how many dependencies I was allowed.


# Run Times & Outputs

### `simple.stl`

```
$ time stack exec frostl fixtures/simple.stl
Number of Triangles: 2
Surface Area: 1.4142135623730956
Bounding Box: { x: 0.0, y: 0.0, z: 0.0 }, { x: 0.0, y: 0.0, z: 1.0 }, { x: 0.0, y: 1.0, z: 0.0 }, { x: 0.0, y: 1.0, z: 1.0 }, { x: 1.0, y: 0.0, z: 0.0 }, { x: 1.0, y: 0.0, z: 1.0 }, { x: 1.0, y: 1.0, z: 0.0 }, { x: 1.0, y: 1.0, z: 1.0 }

real    0m0.274s
user    0m0.222s
sys     0m0.031s
```


### `moon.stl`

```
$ time stack exec frostl fixtures/Moon.stl
Number of Triangles: 116
Surface Area: 7.772634278919952
Bounding Box: { x: 0.0, y: 0.0, z: 0.0 }, { x: 0.0, y: 0.0, z: 3.0 }, { x: 0.0, y: 0.35, z: 0.0 }, { x: 0.0, y: 0.35, z: 3.0 }, { x: 1.62841, y: 0.0, z: 0.0 }, { x: 1.62841, y: 0.0, z: 3.0 }, { x: 1.62841, y: 0.35, z: 0.0 }, { x: 1.62841, y: 0.35, z: 3.0 }

real    0m0.314s
user    0m0.238s
sys     0m0.042s
```


### `rat.stl` (from [TurboSquid](https://www.turbosquid.com/3d-models/sculpture-rat-3ds-free/1127738))

This was the most complex STL file I could find for free online.
It came in the binary format, so I converted it with <https://github.com/cmpolis/convertSTL>.

My numbers do not match the "3D Model Specifications" listed on the web page:
they list "Polygons: 746,788", which is different from my "Number of Triangles" count.
I don't understand the domain well enough to know why.
I did notice that that some of their pictures include a pedstal for the rat, whereas the downloaded file does not.

```
$ time stack exec frostl fixtures/rat.stl
Number of Triangles: 758564
Surface Area: 148.34672035492383
Bounding Box: { x: -3.892809, y: -4.506467, z: -1.923693 }, { x: -3.892809, y: -4.506467, z: 3.075398 }, { x: -3.892809, y: 6.537354, z: -1.923693 }, { x: -3.892809, y: 6.537354, z: 3.075398 }, { x: 2.87902, y: -4.506467, z: -1.923693 }, { x: 2.87902, y: -4.506467, z: 3.075398 }, { x: 2.87902, y: 6.537354, z: -1.923693 }, { x: 2.87902, y: 6.537354, z: 3.075398 }

real    1m0.199s
user    0m57.342s
sys     0m2.549s
```
