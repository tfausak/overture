<h1 align="center">
  <a href="https://github.com/tfausak/overture">
    Overture
  </a>
</h1>

<p align="center">
  Overture is an alternative to some of Haskell's Prelude.
</p>

<p align="center">
    <a href="https://hackage.haskell.org/package/overture">
        <img alt="" src="https://img.shields.io/hackage/v/overture.svg">
    </a>
    <a href="https://travis-ci.org/tfausak/overture">
        <img alt="" src="https://img.shields.io/travis/tfausak/overture/master.svg">
    </a>
    <a href="http://packdeps.haskellers.com/feed?needle=overture">
        <img alt="" src="https://img.shields.io/hackage-deps/v/overture.svg">
    </a>
</p>


<hr>

-   [Install](#install)
-   [Use](#use)

## Install

To use Overture in a Cabal package, add it to your Cabal file.

~~~
build-depends:
    overture ==0.0.*
~~~

For other use cases, install it with `cabal-install`.

~~~ {.sh}
$ cabal update
$ cabal install 'overture ==0.0.*'
~~~

Overture uses [Semantic Versioning][]. Check out [the change log][] for a
detailed list of changes.

## Use

Overture is designed to be imported unqualified. It does not export anything
that conflicts with the Prelude. To get started, simply import it.

~~~ {.haskell}
import Overture
~~~

Check out [the Haddock documentation][] for more information about the
functions Overture provides.

[semantic versioning]: http://semver.org/spec/v2.0.0.html
[the change log]: CHANGELOG.md
[the haddock documentation]: https://hackage.haskell.org/package/overture
