# Spotify.el

**Control Spotify app from within Emacs.**

![track-search](./img/track-search.png)

Spotify.el is a collection of extensions that allows you to control the Spotify
application from within your favorite text editor.

**Note:** This is _very_ alpha software, and it works only in Mac OS X.

## Features

* Uses the Spotify API via Oauth2
* List your public and private playlists and its tracks
* Search for tracks and playlists that match the given keywords
* Easily control basic Spotify player features like, play/pause, previous, 
  next, shuffle, and repeat with the Spotify Remote minor mode

### Planned Features

* Add/remove tracks from playlist
* Linux support via D-Bus

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

Or if you use [el-get](https://github.com/dimitri/el-get):

````el
(add-to-list
  'el-get-sources
  '(:name spotify.el
          :type github
          :pkgname "danielfm/spotify.el"
          :description "Control the Spotify app from within Emacs"
          :url "https://github.com/danielfm/spotify.el"
          :after (progn
                  (setq spotify-oauth2-client-secret "<spotify-app-client-secret>")
                  (setq spotify-oauth2-client-id "<spotify-app-client-id>"))))
````

In order to get the the client ID and client secret, you need to create 
[a Spotify app](https://developer.spotify.com/my-applications), specifying
<http://localhost:8591/> as the redirect URI.

## Usage

### Starting A New Session

In order to connect with the Spotify API and refresh the access token,
run <kbd>M-x spotify-connect</kbd>. This will start the Oauth2 authentication
and authorization workflow.

You may be asked to type a password since the tokens are securely stored as an
encrypted file in the local filesystem. After you enter your credentials and
authorizes the app, you should see a greeting message in the echo area.

To disconnect, run <kbd>M-x spotify-disconnect</kbd>.

### Searching For Tracks

To search for tracks, run <kbd>M-x spotify-track-search</kbd> and type in your
query. The results will be displayed in a separate buffer.

Then, just navigate to it and type <kbd>RET</kbd> to play the track under the
cursor, or type <kbd>M-RET</kbd> in order to play the album in which that track
appears.

The resulting buffer loads the `spotify-remote-mode` by default.

### Searching For Playlists

To list your own playlists, run <kbd>M-x spotify-my-playlists</kbd>, or
<kbd>M-x spotify-playlist-search</kbd> if you want to search for any
playlist that matches the given keywords. The results will be displayed
in a separate buffer.

Then, just navigate to it and type <kbd>RET</kbd> to play the playlist under the
cursor. If you want to list the tracks of the playlist under the cursor, just
type <kbd>M-RET</kbd>.

The resulting buffer loads the `spotify-remote-mode` by default.

### Remote Minor Mode

Whenever you enable the `spotify-remote-mode` you get the following key
bindings:

| Key                | Function                 | Description                    |
|:-------------------|:-------------------------|:-------------------------------|
| <kbd>M-p M-i</kbd> | `spotify-player-info`    | Display the track being played |
| <kbd>M-p M-s</kbd> | `spotify-toggle-shuffle` | Turn shuffle on/off            |
| <kbd>M-p M-r</kbd> | `potify-toggle-repeat`   | Turn repeat on/off             |
| <kbd>M-p M-p</kbd> | `spotify-toggle-play`    | Play/pause                     |
| <kbd>M-p M-f</kbd> | `spotify-next-track`     | Next track                     |
| <kbd>M-p M-b</kbd> | `spotify-previous-track` | Previous track                 |

This mode can be enabled globally with `global-spotify-remote-mode`.

## License

Copyright (C) Daniel Fernandes Martins

Distributed under the New BSD License. See COPYING for further details.
