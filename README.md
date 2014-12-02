# Spotify.el

**Control Spotify app from within Emacs.**

![track-search](./img/track-search.png)

Spotify.el is a collection of extensions that allows you to control the Spotify
application from within your favorite text editor.

**Note:** This is _very_ alpha software, and it works only in Mac OS X.

## Features

* Uses the Spotify API via Oauth2
* Track search mode that lists the tracks that match the given keywords
* Toggle Spotify repeating and shuffling with one keystroke

### Planned Features

* Global "remote control" minor mode as an alternative to media keys
* Add track or album to a new (or existing) playlist
* Linux support via D-Bus

## Installation

First, make sure your system satisfies the given dependencies:

* Emacs 24.4+
* Python 2.7+ (needed for the Oauth2 callback server)

To manually install spotify.el, just clone this project somewhere in your
disk, add that directory in the `load-path`, and require the `spotify` module:

````lisp
(add-to-list 'load-path "<spotify.el-dir>")
(require 'spotify)

;; Settings
(setq spotify-oauth2-client-secret "<spotify-app-client-secret>")
(setq spotify-oauth2-client-id "<spotify-app-client-id>")
````

Or if you use [el-get](https://github.com/dimitri/el-get):

````lisp
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

In order to connect with the Spotify API and refresh the access token,
run `M-x spotify-connect`. This will start the Oauth2 authentication and
authorization workflow.

You may be asked to type a password since the tokens are securely stored as an
encrypted file in the local filesystem. After you enter your credentials and
authorizes the app, you should see a greeting message in the echo area.

To search for tracks, run `M-x spotify-track-search` and type in your query.
The results will be displayed in a separate buffer.

To play any of those tracks, just navigate to it and type `RET`, or type
`M-RET` in order to play the album in which that track appears.

This buffer also loads the `spotify-remote-mode` by default.

### Remote Minor Mode

Whenever you enable the `spotify-remote-mode` you get the following key
bindings:

| Key       | Command          |
|:----------|:-----------------|
| `M-p M-s` | Toggle shuffling |
| `M-p M-r` | Toggle repeating |
| `M-p M-p` | Play/pause       |
| `M-p M-.` | Next track       |
| `M-p M-,` | Previous track   |

Feel free to enable this minor mode wherever you want by using `add-hook`.

## License

Copyright (C) Daniel Fernandes Martins

Distributed under the New BSD License. See COPYING for further details.
