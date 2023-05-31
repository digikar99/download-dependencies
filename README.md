# download-dependencies

> This is an experimental project.

Starting with a root project containing a `.dependencies` file, recursively downloads all the dependencies into `*dependencies-home*`. What this means is that for every dependency downloaded into `*dependencies-home*` directory, it checks whether the (sub)dependency contains a `.dependencies` file, and accordingly downloads the dependencies of those (sub)dependencies.

An example is the `.dependencies` file at [dense-arrays](https://github.com/digikar99/dense-arrays):

```
("abstract-arrays" :git "https://github.com/digikar99/abstract-arrays")
("extensible-optimizing-coerce" :git "https://github.com/digikar99/extensible-optimizing-coerce")
```

Processing this downloads the abstract-arrays repository as well as the extensible-optimizing-coerce repository into `*dependencies-home*`. However, both of these have a `.dependencies` file of their own:


The one at [abtract-arrays](https://github.com/digikar99/abstract-arrays):

```
("polymorphic-functions" :git "https://github.com/digikar99/polymorphic-functions")
("extensible-compound-types" :git "https://github.com/digikar99/extensible-compound-types")
```

The one at [extensible-optimizing-coerce](https://github.com/digikar99/extensible-optimizing-coerce):

```
("extensible-compound-types" :git "https://github.com/digikar99/extensible-compound-types")
```

The recursion continues until a directory has no `.dependencies` file. Eventually, after the execution of

```lisp
(let ((download-dependencies:*dependencies-home* #P"/path/to/dependencies/"))
  (download-dependencies:ensure-system "dense-arrays"))
```

the `/path/to/dependencies/` would contain:

- abstract-arrays
- cl-form-types
- polymorphic-functions
- extensible-optimizing-coerce
- extensible-compound-types

The default value of `*dependencies-home*` is taken as the `dependencies/` directory in the directory containing the system's asd file.
