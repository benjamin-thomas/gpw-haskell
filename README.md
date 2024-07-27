## ghcid-less workflow

1. In tmux pane 0, launch: `ghci ./my_work_in_progress.hs`
2. In tmux pane 1, launch: `./manage/reload_repl_on_change`

## Run tests with

```sh
doctest ./lesson03.hs
# Or
ghcid --command="ghci ./lesson03.hs" --test=':!doctest ./lesson03.hs'
# Or
ghcid --command="ghci -Wall ./lesson14/q2.hs" --test=':!doctest *.hs **/*.hs'
# Or
ghcid --command="ghci -Wall ./lesson15/*" --test=':!doctest *.hs **/*.hs'
# Or
echo ./lesson22/01_interact_non_lazy.hs | entr -c runghc -Wall /_
# Or (with linting, into a lesson folder)
ghcid --lint="hlint --color=always" -c 'ghci ./02_*.hs' -T ':!doctest ./02_*.hs'
```

### Color output quirks

If not seeing color output, you may need to run something like:

```sh
ghcid -c 'ghci -Wall -ferror-spans -fdiagnostics-color=always ./lesson22/01_interact_non_lazy.hs'
```

> Source: https://github.com/ndmitchell/ghcid/blob/master/README.md#i-only-see-source-spans-or-colors-on-errorswarnings-after-the-first-load

Color output does show after a refresh, so doing so may not be worth the effort.

---

Reload in GHCi with:

```ghci
:cmd return $ unlines [":reload", "getAttack killerRobot"]
```

## Install global dependencies with

```sh
cabal install hlint
cabal install --lib random      # System.Random
cabal install --lib containers  # Data.Map
cabal install --lib text        # Data.Text
cabal install --lib bytestring  # Data.ByteString
```
