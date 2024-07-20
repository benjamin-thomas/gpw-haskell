## Run tests with

```sh
doctest ./lesson03.hs
# Or
ghcid --command="ghci ./lesson03.hs" --test=':!doctest ./lesson03.hs'
# Or
ghcid --command="ghci -Wall ./lesson14/q2.hs" --test=':!doctest *.hs **/*.hs'
# Or
ghcid --command="ghci -Wall ./lesson15/*" --test=':!doctest *.hs **/*.hs'
```

Reload in GHCi with:

```ghci
:cmd return $ unlines [":reload", "getAttack killerRobot"]
```

## Install global dependencies with

```sh
cabal install --lib random     # System.Random
cabal install --lib containers # Data.Map
```
