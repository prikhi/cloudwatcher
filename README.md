# cloudwatcher

[![cloudwatcher Build Status](https://travis-ci.org/prikhi/cloudwatcher.svg?branch=master)](https://travis-ci.org/prikhi/cloudwatcher)


A TUI for finding & reviewing 500 errors in AWS cloudwatch.

Requires [`stack`][get-stack]:

```sh
stack run
```

[get-stack]: https://docs.haskellstack.org/en/stable/README/


## Install

You can install the CLI exe by running `stack install`. This lets you call the
executable directly instead of through stack:

```sh
stack install
export PATH="${HOME}/.local/bin/:${PATH}"
cloudwatcher
```

## Usage

AWS authentication details are read from `~/.aws/{config,credentials}`.

Navigate the TUI with vim or arrow keys(+ `Home`, `PageUp`, etc), `Enter` to
move to the next pane, `Esc` to move to the previous pane, & `q` to quit.

By default, only the last 15 minutes of 500 errors are shown. You can change
this by passing the number of minutes to search when you launch cloudwatcher:

```sh
# Show any 500s in the last hour
cloudwatcher 60
```


## Build

You can build the project with stack:

```sh
stack build
```

For development, you can enable fast builds with file-watching,
documentation-building, & test-running:

```sh
stack test --haddock --fast --file-watch --pedantic
```

To build & open the documentation, run:

```sh
stack haddock --open cloudwatcher
```


## LICENSE

BSD-3
