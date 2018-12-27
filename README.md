# Spotify.el

**Control Spotify app from within Emacs.**

![track-search](./img/playlist-tracks.png)

Spotify.el is a collection of extensions that allows you to control the Spotify
application from within your favorite text editor.

**Note:** This is _very_ alpha software, and it works only in Mac OS X and Linux.

## Features

* Spotify client integration for GNU/Linux (via D-Bus) and OS X (via AppleScript)
* Communicates with the Spotify API via Oauth2
* Displays the current track in mode line
* Create playlists (public or private)
* Browse the Spotify featured playlists, your own playlists, and their tracks
* Search for tracks and playlists that match the given keywords
* Easily control basic Spotify player features like, play/pause, previous,
  next, shuffle, and repeat with the Spotify Remote minor mode

## Installation

First, make sure your system satisfies the given dependencies:

* Emacs 24.4+
* Python 2.7+ (needed for the Oauth2 callback server)

To manually install spotify.el, just clone this project somewhere in your
disk, add that directory in the `load-path`, and require the `spotify` module:

````el
(add-to-list 'load-path "<spotify.el-dir>")
(require 'spotify)

;; Settings
(setq spotify-oauth2-client-secret "<spotify-app-client-secret>")
(setq spotify-oauth2-client-id "<spotify-app-client-id>")
````

In order to get the the client ID and client secret, you need to create
[a Spotify app](https://developer.spotify.com/my-applications), specifying
<http://localhost:8591/> as the redirect URI.

### Creating The Spotify App

Go to [Create an Application](https://developer.spotify.com/my-applications/#!/applications/create)
and give your application a name and a description:

![Creating a Spotify App 1/2](./img/spotify-app-01.png)

At this point, the client ID and the client secret is already available, so set
those values to `spotify-oauth2-client-id` and `spotify-oauth2-client-secret`,
respectively.

Then, scroll down a little bit, type <http://localhost:8591/> as the Redirect
URI for the application, and click **Add**:

![Creating a Spotify App 2/2](./img/spotify-app-02.png)

Finally, scroll to the end of the page and hit **Save**.

## Usage

### Remote Minor Mode

Whenever you enable the `spotify-remote-mode` minor mode you get the following
key bindings:

| Key                | Function                     | Description                       |
|:-------------------|:-----------------------------|:----------------------------------|
| <kbd>M-p M-s</kbd> | `spotify-toggle-shuffle`     | Turn shuffle on/off [1]           |
| <kbd>M-p M-r</kbd> | `spotify-toggle-repeat`      | Turn repeat on/off [1]            |
| <kbd>M-p M-p</kbd> | `spotify-toggle-play`        | Play/pause                        |
| <kbd>M-p M-f</kbd> | `spotify-next-track`         | Next track                        |
| <kbd>M-p M-b</kbd> | `spotify-previous-track`     | Previous track                    |
| <kbd>M-p p m</kbd> | `spotify-my-playlists`       | Show your playlists               |
| <kbd>M-p p f</kbd> | `spotify-featured-playlists` | Show the featured playlists       |
| <kbd>M-p p s</kbd> | `spotify-playlist-search`    | Search for playlists              |
| <kbd>M-p p u</kbd> | `spotify-user-playlists`     | Show playlists for the given user |
| <kbd>M-p p c</kbd> | `spotify-create-playlist`    | Create a new playlist             |
| <kbd>M-p t s</kbd> | `spotify-track-search`       | Search for tracks                 |

The current song being played by the Spotify client is displayed in the mode
line along with the player status (playing, paused). The interval in which the
mode line is updated can be configured via the
`spotify-mode-line-refresh-interval` variable:

````el
;; Updates the mode line every second (set to 0 to disable this feature)
(setq spotify-mode-line-refresh-interval 1)
````

[1] No proper support for this in D-Bus implementation for GNU/Linux

#### Customizing The Mode Line

The information displayed in the mode line can be customized by setting the
desired format in `spotify-mode-line-format`. The following placeholders are
supported:

| Symbol | Description                | Example                        |
|:------:|:---------------------------|:-------------------------------|
| `%u`   | Track URI                  | `spotify:track:<id>`           |
| `%a`   | Artist name (truncated)    | `Pink Floyd`                   |
| `%t`   | Track name (truncated)     | `Us and Them`                  |
| `%n`   | Track #                    | `7`                            |
| `%l`   | Track duration, in minutes | `7:49`                         |
| `%r`   | Player repeat status       | `R`, `-`                       |
| `%s`   | Player shuffle status      | `S`, `-`                       |
| `%p`   | Player playing status      | `Playing`, `Paused`, `Stopped` |

The default format is `"[%p: %a - %t ◷ %l %r%s]"`.

The number of characters to be shown in truncated fields can be configured via
the `spotify-mode-line-truncate-length` variable.

````el
(setq spotify-mode-line-truncate-length 10) ; default: 15
````

The text indicator for each of the following player statuses can be configured
via their corresponding variables:

| Player State  | Variable                               | Default Value |
|:--------------|:---------------------------------------|:-------------:|
| Playing       | `spotify-mode-line-playing-text`       | `"Playing"`   |
| Paused        | `spotify-mode-line-paused-text`        | `"Paused"`    |
| Stopped       | `spotify-mode-line-stopped-text`       | `"Stopped"`   |
| Shuffling On  | `spotify-mode-line-repeating-text`     | `"R"`         |
| Shuffling Off | `spotify-mode-line-not-repeating-text` | `"-"`         |
| Repeating On  | `spotify-mode-line-shuffling-text`     | `"S"`         |
| Repeating Off | `spotify-mode-line-not-shuffling-text` | `"-"`         |

#### Global Remote Mode

This mode can be enabled globally by running
<kbd>M-x global-spotify-remote-mode</kbd>.

### Searching For Tracks

To search for tracks, run <kbd>M-x spotify-track-search</kbd> and type in your
query. The results will be displayed in a separate buffer with the following
key bindings:

| Key              | Description                                                      |
|:-----------------|:-----------------------------------------------------------------|
| <kbd>l</kbd>     | Loads the next page of results (pagination)                      |
| <kbd>g</kbd>     | Clears the results and reloads the first page of results         |
| <kbd>M-RET</kbd> | Plays the track under the cursor in the context of its album [1] |

[1] D-Bus implementation for GNU/Linux do not support passing the context, so
only the track under the cursor will be played

The resulting buffer loads the `spotify-remote-mode` by default.

**Tip:** In order to customize the number of items fetched per page, just change
the variable `spotify-api-search-limit`:

````el
;; Do not use values larger than 50 for better compatibility across endpoints
(setq spotify-api-search-limit 50)
````

### Playing a Spotify URI

To ask the Spotify client to play a resource by URI, run
<kbd>M-x spotify-play-uri</kbd> and enter the resource URI.

### Creating Playlists

To create new playlists, run <kbd>M-x spotify-create-playlist</kbd> and follow
the prompts.

Currently it's not possible to add tracks to a playlist you own, or to remove
tracks from them.

### Searching For Playlists

To return the playlists for the current user, run
<kbd>M-x spotify-my-playlists</kbd>, or
<kbd>M-x spotify-user-playlists</kbd> to list the public playlists for some
given user. To search playlists that match the given search criteria, run
<kbd>M-x spotify-playlist-search CRITERIA</kbd>. Also, run
<kbd>M-x spotify-featured-playlists</kbd> in order to browse the featured
playlists from Spotify en_US.

Change the following variables in order to customize the locale and region for
the featured playlists endpoint:

````el
;; Spanish (Mexico)
(setq spotify-api-locale "es_MX")
(setq spotify-api-country "MX")
````

All these commands will display results in a separate buffer with the following
key bindings:

| Key              | Description                                              |
|:-----------------|:---------------------------------------------------------|
| <kbd>l</kbd>     | Loads the next page of results (pagination)              |
| <kbd>g</kbd>     | Clears the results and reloads the first page of results |
| <kbd>f</kbd>     | Follows the playlist under the cursor                    |
| <kbd>u</kbd>     | Unfollows the playlist under the cursor                  |
| <kbd>t</kbd>     | Lists the tracks of the playlist under the cursor        |
| <kbd>M-RET</kbd> | Plays the playlist under the cursor                      |

Once you opened the list of tracks of a playlist, you get the following key
bindings in the resulting buffer:

| Key              | Description                                                         |
|:-----------------|:--------------------------------------------------------------------|
| <kbd>l</kbd>     | Loads the next page of results (pagination)                         |
| <kbd>g</kbd>     | Clears the results and reloads the first page of results            |
| <kbd>f</kbd>     | Follows the current playlist                                        |
| <kbd>u</kbd>     | Unfollows the current playlist                                      |
| <kbd>M-RET</kbd> | Plays the track under the cursor in the context of the playlist [1] |

Both buffers load the `spotify-remote-mode` by default.

[1] D-Bus implementation for GNU/Linux do not support passing the context, so
only the track under the cursor will be played

## Donate

If this project is useful for you, buy me a beer!

Bitcoin: `bc1qtwyfcj7pssk0krn5wyfaca47caar6nk9yyc4mu`

## License

Copyright (C) Daniel Fernandes Martins

Distributed under the New BSD License. See COPYING for further details.
