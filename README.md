Run tests with

```sh
doctest ./lesson03.hs
# Or
ghcid --command="ghci ./lesson03.hs" --test=':!doctest ./lesson03.hs'
# Or
ghcid --command="ghci -Wall ./lesson14/q2.hs" --test=':!doctest **/*.hs'
# Or
ghcid --command="ghci -Wall ./lesson15/*" --test=':!doctest **/*.hs'
```

Reload in GHCi with:

```ghci
:cmd return $ unlines [":reload", "getAttack killerRobot"]
```