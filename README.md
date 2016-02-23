# Spotify.el

**Control Spotify app from within Emacs.**

![track-search](./img/track-search.png)

Spotify.el is a collection of extensions that allows you to control the Spotify
application from within your favorite text editor.

**Note:** This is _very_ alpha software, and it works only in Mac OS X.

## Features

* Communicates with the Spotify API via Oauth2
* List your public and private playlists and its tracks
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
query. The results will be displayed in a separate buffer with the following
key bindings:

| Key              | Description                                                  |
|:-----------------|:-------------------------------------------------------------|
| <kbd>l</kbd>     | Loads the next page of results (pagination)                  |
| <kbd>RET</kbd>   | Plays the track under the cursor in the context of its album |

The resulting buffer loads the `spotify-remote-mode` by default.

**Tip:** In order to customize the number of items fetched per page, just change
the variable `spotify-api-search-limit`:

````el
;; Do not use values larger than 50 for better compatibility across endpoints
(setq spotify-api-search-limit 50)
````

### Searching For Playlists

To list your own playlists, run <kbd>M-x spotify-my-playlists</kbd>, or
<kbd>M-x spotify-playlist-search</kbd> if you want to search for any
playlist that matches the given keywords. The results will be displayed
in a separate buffer with the following key bindings:

| Key              | Description                                           |
|:-----------------|:------------------------------------------------------|
| <kbd>l</kbd>     | Loads the next page of results (pagination)           |
| <kbd>f</kbd>     | Follows the playlist under the cursor                 |
| <kbd>RET</kbd>   | Play the playlist under the cursor from the beginning |
| <kbd>M-RET</kbd> | Lists the tracks of the playlist under the cursor     |

Once you opened the list of tracks of a playlist, you get the following key
bindings in the resulting buffer:

| Key              | Description                                                     |
|:-----------------|:----------------------------------------------------------------|
| <kbd>l</kbd>     | Loads the next page of results (pagination)                     |
| <kbd>f</kbd>     | Follows the current playlist                                    |
| <kbd>RET</kbd>   | Plays the track under the cursor in the context of the playlist |
| <kbd>M-RET</kbd> | Plays the track under the cursor in the context of its album    |

Both buffers load the `spotify-remote-mode` by default.

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

This is particularly useful for those using keyboards without media keys.

This mode can be enabled globally with `global-spotify-remote-mode`.

## License

Copyright (C) Daniel Fernandes Martins

Distributed under the New BSD License. See COPYING for further details.
