#delta

This is a haskell library for monitoring filesystem changes. It is FRP based
but also provides a callback API.

In the future the library will offer OS-specific methods of detecting changes
but for now it uses periodic recursive directory traversal.
