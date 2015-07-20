#delta ![Build Status](https://travis-ci.org/kryoxide/delta.svg?branch=master)

This is a haskell library for monitoring filesystem changes. It is FRP based
but also provides a callback API.

This library currently offers two methods for detecting file changes:

* Polling of directories in given time intervals
* Using the FSEvents - API (OS X only)

## Usage

### FRP interface (using Sodium)
Import the module ```System.Delta``` and then call the function ```deltaDir```
on the path you want to monitor. The value you get back is an instance of the
class ```FileWatcher``` but it will later depend on your OS.

The generated value offers three ```Event```s:

* ```changedFiles```
* ```newFiles```
* ```deletedFiles```

### Callback interface

The function ```deltaDirWithCallbacks``` gives you an instance of the
datatype ```CallbackWatcher``` that wraps a ```FileWatcher```.

You can add an arbitrary number of callbacks to instances of that type with:

* ```withChangedCallback```
* ```withNewCallback```
* ```withDeletedCallback```

These operations give you back a ```CallbackId``` which lets you delete
callbacks from the ```CallbackWatcher``` using ```unregisterCallback```.

## Command line interface

### delta-run

This package includes a handy little executable called ```delta-run``` which
allows you to run a command on your terminal each time a file in the directory
you are watching changes.

This is especially handy if you want to recompile some code each time it
changes, but there are a lot of other useful applications.

  Usage: delta-run [-i|--interval INTERVAL] FILE CMD

  Available options:
    -h,--help                Show this help text
    -i,--interval INTERVAL   Run at most every n seconds
    FILE                     The directory that is watched
    CMD                      The command to run

### delta-cli (demo only)

This cabal package also ships an executable called ```delta-cli``` that currently
watches a directory that you give as an argument

    delta-cli /path/to/my/directory

outputs:

    new:     /path/to/my/directory/file
    changed: /path/to/my/directory/file
    new:     /path/to/my/directory/file2
    del:     /path/to/my/directory/file

## Hosting

This package is [hosted on hackage](http://hackage.haskell.org/package/delta)
