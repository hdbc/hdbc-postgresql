# Changelog

## 2.5.0.1

* Bump time upper bound to 1.14, add round-trip test for 0-padded date.

## 2.5.0.0

* Add explicit call to `finalizeForeignPtr` on connection disconnects. Ensures disconnects happen at the moment close is call, as opposed to when the GC frees the ForeignPtr.

## 2.4.0.0

* Remove custom reference counting logic. Thanks to David Johnson.

### 2.3.2.8

* Compatiblity with Cabal 3.2

### 2.3.2.7

* Compatibility with time 1.9 & GHC 8.8
* Custom setup for cabal v2 & v3

### 2.3.2.6

* Fix postgresql header name collision

#### 2.3.2.5

* Compatibility with time 1.8, GHC 8.2.

#### 2.3.2.4

* Compatibility with time 1.6, base 4.9, and Cabal 1.24 (GHC 8).

#### 2.3.2.3

* Compatibility with time 1.5.
* Fix test compilation.
* Fix warnings.
