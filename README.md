
# purescript-road-to-react

The book [Road to React](https://www.roadtoreact.com/) is an introductory book
to React in JavaScript.  In early 2020, the book was updated to use
[React Hooks](https://reactjs.org/docs/hooks-intro.html).

The first half of the book iteratively develops a React-based frontend.  Each
chapter gradually introduces more and more functionality from React.  As the
first half of the book comes to an end, you've written a basic frontend that
demonstrates a good deal of the functionality of React.

This repo translates the on-going example from into PureScript, using the
[purescript-react-basic-hooks](https://github.com/spicydonuts/purescript-react-basic-hooks)
library.

## The Code

The original JavaScript code can be found on CodeSandbox
[here](https://codesandbox.io/s/github/the-road-to-learn-react/hacker-stories/tree/hs/Data-Re-Fetching-in-React).

The translated PureScript code can be found [in this repo](./src/App.purs).

## Running

This repo can be built by running the following commands:

```console
# Enter into a Nix shell with all the dependencies available.
$ nix-shell
# Within the nix shell, build the PureScript app with spago.
$ spago build
# Run Parcel to handle the CommonJS modules.
$ parcel index.html
```

`parcel` serves the app on http://localhost:1234/.

If you want to do development interactively, you can start `spago` in one
terminal with a command line `spago build --watch`, and then start `parcel` in
another terminal.  `parcel` sometimes gets autoreloading correct, but sometimes
you explicitly have to refresh the browser window to see changes.

If you don't want to use Nix, it should be possible to install `spago`, the
PureScript compiler, and Parcel with NPM/yarn.
