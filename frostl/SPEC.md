# STL File Analysis

## STL Files

An STL (stereolithography) file is a standard file format for defining 3D models that will be printed. STL files describe only the surface geometry of a three-dimensional object without any representation of color, texture or other common CAD model attributes. STL files have both an ASCII and binary representation but for this challenge we'll only work with the ASCII version.

### Format

```
solid name
  facet normal ni nj nk
      outer loop
          vertex v1x v1y v1z
          vertex v2x v2y v2z
          vertex v3x v3y v3z
      endloop
  endfacet
endsolid name
```

For example here is a very simple 2 triangle STL.

```
solid simple
  facet normal 0 0 0
      outer loop
          vertex 0 0 0
          vertex 1 0 0
          vertex 1 1 1
      endloop
  endfacet
  facet normal 0 0 0
      outer loop
          vertex 0 0 0
          vertex 0 1 1
          vertex 1 1 1
      endloop
  endfacet
endsolid simple
```

## Challenge

For this challenge we'd like you to implement an STL parser that can do some basic analysis of a model. It should be able to report these metrics

* The number of triangles in the model
* The surface area of the model
* The bounding box of the model (the 8 points defining a cube (or 3D rectangle) with smallest volume that completely contains the shape), see: https://en.wikipedia.org/wiki/Minimum_bounding_box

## Assumptions

In order to reduce the complexity of the challenge you can safely assume that the input STL files are all properly formed and all of the triangles are non overlapping.

## Example output

```
Number of Triangles: 2
Surface Area: 1.4142
Bounding Box: {x: 0, y: 0, z: 0 }, {x: 1, y: 1, z: 1 } ...
```

## Instructions

You may develop your solution in any language that you like. However along with your solution please provide a README that includes instructions for running your code, an explanation of the design decisions you made, and potential performance improvements you might make so your solution could handle a model with millions of triangles.

## What We're Looking For

While this challenge is about working with 3D models we're not trying to test your math knowledge or your ability to think about 3D space. You should be able to find the basic forumlas needed for the calculations with a bit of Googling. However, if you find yourself spending too much time on those bits (more than 30 minutes or so) feel free to just leave comments where you would put the calculation in your code. We're much more concerned with how understandable and extensible your code is rather than your ability to understand math formulas.
