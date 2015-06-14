#delta

This is a haskell library for monitoring filesystem changes. It is FRP based
but also provides a callback API.

In the future the library will offer OS-specific methods of detecting changes
but for now it uses periodic recursive directory traversal.

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

You can add callbacks to instances of that type with:

* ```withChangedCallback```
* ```withNewCallback```
* ```withDeletedCallback```
