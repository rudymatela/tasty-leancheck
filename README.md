tasty-leancheck: LeanCheck support for the Tasty test framework
===============================================================

[![tasty-leancheck's Build Status][build-status]][build-log]

[LeanCheck] support for the [Tasty] test framework.
Tasty and healthy tests.


Installing
----------

    $ cabal install tasty-leancheck


Example
-------

(This example is intentionally similar to [Tasty's official example].)

Here's how your `test.hs` might look like:

```haskell
import Test.Tasty
import Test.Tasty.LeanCheck as LC
import Data.List

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Test properties checked by LeanCheck"
  [ LC.testProperty "sort == sort . reverse" $
      \list -> sort (list :: [Int]) == sort (reverse list)
  , LC.testProperty "Fermat's little theorem" $
      \x -> ((x :: Integer)^7 - x) `mod` 7 == 0
  -- the following two properties do not hold
  , LC.testProperty "Fermat's last theorem" $
      \x y z n ->
        (n :: Integer) >= 3 LC.==> x^n + y^n /= (z^n :: Integer)
  ]
```

And here is the output for the above program:

```sh
$ ./test
Test properties checked by LeanCheck
  sort == sort . reverse:  OK
    +++ OK, passed 200 tests.
  Fermat's little theorem: OK
    +++ OK, passed 200 tests.
  Fermat's last theorem:   FAIL
    *** Failed! Falsifiable (after 71 tests):
    0 0 0 3

1 out of 3 tests failed (0.00s)
```


Options
-------

The tasty-leancheck provider has only one option, `--leancheck-tests`:

	$ ./test --leancheck-tests 10
	Test properties checked by LeanCheck
      sort == sort . reverse:  OK
        +++ OK, passed 10 tests.
      Fermat's little theorem: OK
        +++ OK, passed 10 tests.
      Fermat's last theorem:   OK
        +++ OK, passed 10 tests.

All 3 tests passed (0.00s)


[Tasty's official example]: https://github.com/feuerbach/tasty#example
[Tasty]:     https://github.com/feuerbach/tasty
[LeanCheck]: https://github.com/rudymatela/leancheck

[build-status]: https://travis-ci.org/rudymatela/tasty-leancheck.svg?branch=master
[build-log]:    https://travis-ci.org/rudymatela/tasty-leancheck
