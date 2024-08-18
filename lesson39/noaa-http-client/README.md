Initialized with:

```
cabal init --simple -m noaa-http-client
```

Request token here:

https://www.ncei.noaa.gov/cdo-web/token

Then use the git-ignored `.env` dir to load it, like so:

```
[noaa-http-client]$ envdir .env.example/ env | grep TOKEN
TOKEN=hello-123
```

Then run the app, like so:

```
envdir .env cabal run
```

Watch run with on of:

```
rg --files | entr -c bash -c 'envdir .env cabal run'
rg --files | entr -c bash -c 'envdir .env cabal -v0 run'
```