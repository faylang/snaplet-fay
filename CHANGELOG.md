## Changelog

#### 0.3.3.12

* Allow `aeson 0.9.*`

#### 0.3.3.11

* Allow `snap 0.14.*`

#### 0.3.3.10 (2015-02-11)

* Allow `fay 0.23.*`

#### 0.3.3.9 (2015-01-05)

* Allow `fay 0.22.*`

#### 0.3.3.8 (2014-10-11)

* Allow `fay 0.21.*`

#### 0.3.3.7 (2014-05-12)

* Take `HASKELL_PACKAGE_SANDBOX` into account when compiling Fay code

##### Revision 1

* Allow `configurator == 0.3.*`

#### 0.3.3.6 (2014-05-11)

* Allow `mtl 2.2.*`
* Allow `transformers >= 0.4.1 && < 0.5`

#### 0.3.3.5 (2014-04-29)

* Update to `fay 0.20`

#### 0.3.3.4 (2014-01-19)

* Allow aeson 0.7.*

#### 0.3.3.3 (2014-01-17)

* Allow fay 0.19.*

#### 0.3.3.2 (2013-11-07)

* example: Allow fay-text 0.3
* example: Remove bounds on snaplet-fay

#### 0.3.3.1 (2013-11-07)

* Allow fay-jquery 0.5

### 0.3.3 (2013-11-07)

* Format Fay compile errors using Fay's builtin (doesn't break on apostrophes)
* Only treat compile errors as Success in development mode (for in browser error reporting)
* Fixes a bug where error strings would be encoded twice
* Requires fay 0.18
* Update example for fay-jquery 0.4
* Upper bounds for dependencies

### 0.3.2.0

* Expose Snap.Snaplet.Fay.Internal

### 0.3.1.1

* Hackage version now includes the example application
* Updated repository location

### 0.3.1.0

* Add `packages` flag in the config which lets you use other fay packages without using includeDirs
* On errors in development mode a js file is produced that prints the error to the console (Thanks Daniel Bruegmann)

#### 0.3.0.1

* Bump to support snap 0.10 and fay 0.11
