# Vending Machine

This project is a simple exercise in implementing the classic low-level design task of building a vending machine.
<br> The primary objective is to learn, practice, experiment, and play with the [ gleam language ](https://gleam.**run**/)

This is not intended to serve as an example of good code quality in any way.

# Build structure
The gleam file was compiled by the gleam using
```
gleam build
```
Then the build was bundled using esbuild:
```
esbuild ./build/dev/javascript/vending_machine_gleam/vending_machine_gleam.mjs --bundle --outfile=out.js
```

then the out.js code was copy and pasted into the `index.html` file for a static build