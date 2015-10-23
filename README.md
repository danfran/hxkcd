# HXkcd

HXkcd is a simple client that reproduces the behaviour of the original website [Xkcd] and gives you the possibility to move back and forth across the comic images.

![Screenshot](screenshot.png)

The project is written in [Haskell] and follows the principles of [Functional Reactive Programming] using the library [Reactive-Banana].

## Compile and Run

Before to compile and run the application, install a local sandbox:

```
cd hxkcd
cabal sandbox init
```

Due to the nature of some libraries like wx/wxcore (that relay underneath on wxWidgets/wxHaskell) and reactive-banana-wx
 check your current version of cabal with:
 
```
cabal --version
```
 
If the version is ```< 1.22.4.0``` then install a local cabal version with:

```
cabal install cabal-1.22.4.0 # (or 1.22.6.0)
```

Once this is done run:

```
.cabal-sandbox/bin/cabal configure
.cabal-sandbox/bin/cabal build
```

This might be not the most elegant solution at the moment (there are better alternatives like [Stack] for example) but it is the only one that I tried to make the build process working.

[Xkcd]:http://xkcd.com/
[Haskell]:https://www.haskell.org/
[Functional Reactive Programming]:https://en.wikipedia.org/wiki/Functional_reactive_programming
[Reactive-Banana]:https://wiki.haskell.org/Reactive-banana
[Stack]:https://www.stackage.org/

