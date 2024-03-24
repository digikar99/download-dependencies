# download-dependencies

> This is an experimental project.

Two cases, either

1. You have the system and you only want to download the dependencies,
2. Or, you do not have the system and you want to download the system itself, along with its dependencies.

`download-dependencies` was primarily made for use case 1, but it proved to be trivial to adopt it for use case 2. It exports two symbols

- `*dependencies-home*`: The directory where the system or its dependencies will be downloaded.
- `ensure-system`: The user facing high-level function that downloads the system or its dependencies

## If you want to download the system itself (use case 2)

```lisp
(let ((download-dependencies:*dependencies-home* #P"/path/to/dependencies/"))
  (download-dependencies:ensure-system
   "dense-arrays"
   :source-type :git
   :source "https://github.com/digikar99/dense-arrays"))
```

This emits

```
Cloning into 'dense-arrays'...
Downloading dependencies in /path/to/dependencies/test-dd/
Downloading https://github.com/digikar99/abstract-arrays
Cloning into 'abstract-arrays'...
Downloading https://gitlab.com/digikar/peltadot
Cloning into 'peltadot'...
warning: redirecting to https://gitlab.com/digikar/peltadot.git/
Downloading https://github.com/alex-gutev/cl-environments
Cloning into 'cl-environments'...
Downloading https://github.com/digikar99/compiler-macro-notes
Cloning into 'compiler-macro-notes'...
Downloading https://github.com/digikar99/alternate-asdf-system-connections
Cloning into 'alternate-asdf-system-connections'...
```

## If you want to download only the dependencies (use case 1)

```lisp
(let ((download-dependencies:*dependencies-home* #P"/path/to/dependencies/"))
  (download-dependencies:ensure-system "dense-arrays"))
```

This emits

```
From https://github.com/digikar99/dense-arrays
 * branch            HEAD       -> FETCH_HEAD
Already up to date.
Downloading dependencies in /path/to/dependencies/test-dd/
abstract-arrays is already available. Updating...
From https://github.com/digikar99/abstract-arrays
 * branch            HEAD       -> FETCH_HEAD
Already up to date.
peltadot is already available. Updating...
From https://github.com/digikar99/abstract-arrays
 * branch            HEAD       -> FETCH_HEAD
Already up to date.
peltadot is already available. Updating...
warning: redirecting to https://gitlab.com/digikar/peltadot.git/
From https://gitlab.com/digikar/peltadot
 * branch            HEAD       -> FETCH_HEAD
Already up to date.
cl-environments is already available. Updating...
From https://github.com/alex-gutev/cl-environments
 * branch            HEAD       -> FETCH_HEAD
Already up to date.
compiler-macro-notes is already available. Updating...
From https://github.com/digikar99/compiler-macro-notes
 * branch            HEAD       -> FETCH_HEAD
Already up to date.
alternate-asdf-system-connections is already available. Updating...
From https://github.com/digikar99/alternate-asdf-system-connections
 * branch            HEAD       -> FETCH_HEAD
Already up to date.
```

## What it does

Starting with a root project containing a `.dependencies` file, recursively downloads all the dependencies into `*dependencies-home*`. What this means is that for every dependency downloaded into `*dependencies-home*` directory, it checks whether the (sub)dependency contains a `.dependencies` file, and accordingly downloads the dependencies of those (sub)dependencies. **Conflicts are not be resolved.**

An example is the `.dependencies` file at [dense-arrays](https://github.com/digikar99/dense-arrays):

```
("abstract-arrays" :git "https://github.com/digikar99/abstract-arrays")
("peltadot" :git "https://gitlab.com/digikar/peltadot")
```

Processing this downloads the abstract-arrays repository as well as the extensible-optimizing-coerce repository into `*dependencies-home*`. However, both of these have a `.dependencies` file of their own:


The one at [abtract-arrays](https://github.com/digikar99/abstract-arrays):

```
("peltadot" :git "https://gitlab.com/digikar/peltadot")
```

The one at [peltadot](https://gitlab.com/digikar/peltadot):

```
("cl-environments" :git "https://github.com/alex-gutev/cl-environments")
("compiler-macro-notes" :git "https://github.com/digikar99/compiler-macro-notes")
("alternate-asdf-system-connections" :git "https://github.com/digikar99/alternate-asdf-system-connections")
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

If `:source` and `:source-type` were supplied, it will additionally also contain the `dense-arrays` directory.

The default value of `*dependencies-home*` is taken as the `dependencies/` directory in the directory containing the system's asd file.
