Snaplet Fay
===========

Snaplet Fay integrates [Snap](http://www.snapframework.com) with
[Fay](http://www.fay-lang.org). Snap is a
[Haskell](http://www.haskell.org) web framework and Fay is a compiler
from a proper subset of Haskell to JavaScript. Snaplet Fay integrates
them nicely with each other allowing automatic (re)compilation of Fay
source files. Snap provides this for both static content and haskell
sources preventing web server restarts and here we add Fay to this
list as well. This lets us write both front and back-end code in Haskell.


Installation
------------

You will need Haskell, Snap and Fay installed. The simplest way to get
up and running with Haskell is to install
[The Haskell Platform](http://hackage.haskell.org/platform/).

Snap and Fay are available on hackage, but use chrisdone's master for now:

```
cabal install snap
git clone https://github.com/faylang/fay.git && cd fay && cabal install
```

Clone this repository and install the package:
```
cabal install
````

Example Usage
-------------

Site.hs:
```
import Snap.Snaplet.Fay

app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
  -- Put Fay .hs files in src/Fay
  -- The verbosity Bool parameter tells Snap Fay how much information it should
  -- print about what it's doing.
  -- CompileOnDemand compiles files when requested.
  -- CompileAll compiles all files and removes js files whose hs source has been deleted.
  fay' <- nestSnaplet "fay" fay $ initFay "src/Fay" True CompileOnDemand
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

Contact
-------

File an issue, e-mail or visit `#fay @ irc.freenode.net`.
