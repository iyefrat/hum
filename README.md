# Hum: a Haskell Music Manager

A TUI mpd client, currently in alpha.

The plan is to basically reimplement ncmpcpp in haskell (at least featurewise). The reasons are as follows:

-   Haskell is much more fun than C++
-   I find the tag editor of ncmpcpp to be lacking and want to change it
-   Generally, I want to change some stuff with the UI
-   It&rsquo;s faster (try scrolling down the artists in the library in both programs)
-   I wanted to design the program around vim style bindings
-   I don&rsquo;t know C++


## Installation

not on hackage yet, so you have to clone and run `cabal install`.


## Features


### Current

-   playback keys
-   Viewing the Queue, Library, and Playlists screens
-   Editing the Queue with vim like cut copy and paste
-   adding and bulk adding songs from the Library and Playlists
-   vim style search
-   a help command


### Planned

-   playlist editing
-   tag info and editing
-   mpdish file browser browser and more advanced search
-   visualizer because why not
-   having a random mode when you can see what the next song is
-   making the UI nicer
-   more commands
-   more vimmy keybindings (e.g. 10j, perhaps making y and d verbs)
-   doing stuff with album art using uegerzug
-   config file


## Help

Here are all keybindings currently implemented:


### Change views:

`1` - queue

`2` - library

`3` - playlists


### General Bindings:

`t`       - play/pause toggle

`,`       - previous song

`.`       - next song

`[` and `]` - skip 5 second in either direction

`{` and `}` - skip 30 second in either direction

`hjkl`    - vim movements

`/` and `?` - forwards and backwards search

`n` and `N` - move to next match of search

`:`       - execute commands

`q`       - quit

`s`       - toggle single mode in mpd

`c`       - toggle consume mode in mpd

`x`       - toggle crossfade mode in mpd

`r`       - toggle repeat mode in mpd

`z`       - toggle random mode in mpd


### Queue keybindings:

`SPC` - select song

`y` and `d` - yank and delete the selected songs

`p`   - paste selected song


### Library and Playlists keybindings:

`SPC` - add song/song collection to queue

`RET` - add song/song collection to queue, and start playing the first one


### Commands:

`:help` - gets you this

`:q`    - quits