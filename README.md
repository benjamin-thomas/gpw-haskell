Run tests with

```sh
doctest ./lesson03.hs
# Or
ghcid --command="ghci ./lesson03.hs" --test=':!doctest ./lesson03.hs'
# Or
ghcid --command="ghci ./lesson03.hs" --test=':!doctest *.hs'
```