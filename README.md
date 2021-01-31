# Hum: a Haskell Music Manager

A TUI mpd client, currently in beta and under active development.

The plan is to basically reimplement ncmpcpp in haskell (at least featurewise). The reasons are as follows:

-   Haskell is much more fun than C++
-   I find the tag editor of ncmpcpp to be lacking and want to change it
-   Generally, I want to change some stuff with the UI
-   It's faster (try scrolling down the artists in the library in both programs)
-   I wanted to design the program around vim style bindings
-   I don't know C++


## Installation

As this is an MPD client, you will need to install [mpd](https://github.com/MusicPlayerDaemon/MPD) from your favorite package manager.

You can install `hum` directly from [Hackage](https://hackage.com/package/hum), or by cloning the repo and running `cabal install`.

Nix support will be available in the near future, but in the meantime you can run `nix-build release.nix` and then launch hum with `./result/bin/hum` to try it out, but this requires an up to date `nixpkgs-unstable`.

## Features

### Current

-   playback keys
-   Viewing the Queue, Library, and Playlists screens
-   Editing the Queue with vim like cut copy and paste
-   adding and bulk adding songs from the Library and Play  nlists
-   vim style search
-   a help command
-   playlist editing


### Planned

-   better documentation
-   tag info and editing
-   mpdish file browser and more advanced search
-   visualizer because why not
-   having a random mode when you can see what the next song is
-   making the UI nicer
-   more commands
-   more vimmy keybindings (e.g. 10j, perhaps making y and d verbs)
-   doing stuff with album art using uegerzug
-   config file

For a list of all the keybindings and ex style commands, type `:help` and press `RET`.
