Snaplet Fay
===========

Snaplet Fay integrates [Snap](http://www.snapframework.com) with
[Fay](http://www.fay-lang.org). Snap is a
[Haskell](http://www.haskell.org) web framework and Fay is a compiler
for a proper subset of Haskell to JavaScript. Snaplet Fay integrates
them nicely with each other allowing automatic (re)compilation of Fay
source files. Snap provides this for both static content and haskell
sources preventing web server restarts and here we add Fay to this
list as well. Now we can seamlessly code Haskell for both front-end
and back-end.

Installation
------------

You will need Haskell, Snap and Fay installed. The simplest way to get
up and running with Haskell is to install
[The Haskell Platform](http://hackage.haskell.org/platform/).

Snap and Fay are available on hackage:

```
cabal install snap fay
```

Clone this repository and install the package:
```
cabal install
````

Example Usage
-------------

Site.hs:
```
import           Snap.Snaplet.Fay

app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
  -- Put Fay .hs files in src/Fay
  -- The True argument will cause Snap Fay to print more information about what it's doing.
  fay' <- nestSnaplet "fay" fay $ initFay "src/Fay" True
  return $ App { _fay = fay' }
```

Application.hs:
```
import Snap.Snaplet.Fay

data App = App { _fay :: Snaplet Fay }

makeLens ''App
```


Development Status
------------------

Snaplet Fay is functioning and will be updated to keep up with
both Snap and Fay.


Contributions
-----------

Fork on!

Any enhancements are welcome.

To run the tests, do:
```
cabal configure -ftest
cabal build
./dist/build/test/test
```
