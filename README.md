# Smudge

[![MELPA](https://melpa.org/packages/smudge-badge.svg)](https://melpa.org/#/smudge)

**Control Spotify app from within Emacs.**

[![asciicast](https://asciinema.org/a/218654.svg)](https://asciinema.org/a/218654)

Smudge allows you to control the Spotify application from within your favorite text
editor. If you are running on Mac OS X or Linux, you can control the locally running instance. If
you are running on any platform with a network connection (including Windows - and even headless!)
and have a Spotify premium subscription, you can control an instance of Spotify via the Spotify
Connect feature.

## Features

* Spotify client integration for GNU/Linux (via D-Bus) and OS X (via AppleScript)
* Device playback display & selection using the Spotify Connect API (requires premium)
* Communicates with the Spotify API via Oauth2
* Displays the current track in mode line or title bar
* Create playlists (public or private)
* Browse the Spotify featured playlists, your own playlists, and their tracks
* Search for tracks and playlists that match the given keywords
* Easily control basic Spotify player features like, play/pause, previous,
  next, shuffle, and repeat with the Smudge Remote minor mode

## Installation

Smudge requires Emacs 27.1+.

### Vanilla Emacs

`package.el` is the built-in package manager in Emacs.

Smudge is available on the two major package.el community maintained repos MELP AStable and MELPA.

You can install Smudge with the following command:

<kbd>M-x</kbd> package-install <kbd>[RET]</kbd> smudge <kbd>[RET]</kbd>

Or put the following snippet into your Emacs configuration:

```elisp
(use-package smudge
  :bind-keymap ("C-c ." . smudge-command-map)
  :config
  (setq smudge-oauth2-client-secret "..."
        smudge-oauth2-client-id "..."))
```

### Doom Emacs

Add the following to the `packages.el` file:

```elisp
;; Fetch from MELPA
(package! smudge)

;; Fetch from GitHub
(package! smudge
  :recipe (:host github :repo "danielfm/smudge"))
```

Add the following to the `config.el` file:

``` elisp
(use-package! smudge
  :bind-keymap ("C-c ." . smudge-command-map)
  :custom
  (smudge-oauth2-client-secret "...")
  (smudge-oauth2-client-id "...")
  ;; optional: enable transient map for frequent commands
  (smudge-player-use-transient-map t))
```

## Configuration

```elisp
(setq smudge-oauth2-client-secret "<spotify-app-client-secret>")
(setq smudge-oauth2-client-id "<spotify-app-client-id>")
```

In order to get the client ID and client secret, you need to create a
[Spotify app][app-list], specifying <http://localhost:8080/smudge-api-callback>
as the redirect URI (or whichever port you have specified via customize). The
OAuth2 exchange is handled by `simple-httpd`. If you are not already using
this package for something else, you should not need to customize this port.
Otherwise, you'll want to set it to whatever port you are running on.

To use the "Spotify Connect" transport (vs. controlling only your local
instance - though you can also control your local instance as well), set
`smudge-transport` to `'connect` as follows. **This feature requires a Spotify
premium subscription.**

```elisp
(setq smudge-transport 'connect)
```

### Key Bindings

``` elisp
; Set C-c . as the Smudge [prefix]
(define-key smudge-mode-map (kbd "C-c .") 'smudge-command-map)
```

The keymap prefix <kbd>C-c .</kbd> is just a suggestion, following the
conventions suggested for minor modes as defined in the Emacs manual
[Key Binding Conventions][kbd-conv]. Previous versions of this package used
<kbd>M-p</kbd>.

The default bindings provided by the `smudge-command-map` is as follows:

| Key                     | Function                                   | Description                                      |
|:------------------------|:-------------------------------------------|:-------------------------------------------------|
| <kbd>[prefix] d</kbd>   | `smudge-select-device`                     | Select a playback device [2]                     |
| <kbd>[prefix] SPC</kbd> | `smudge-controller-toggle-play`            | Play/pause                                       |
| <kbd>[prefix] s</kbd>   | `smudge-controller-toggle-shuffle`         | Turn shuffle on/off [1]                          |
| <kbd>[prefix] r</kbd>   | `smudge-controller-toggle-repeat`          | Turn repeat on/off [1]                           |
| <kbd>[prefix] n</kbd>   | `smudge-controller-next-track`             | Next track                                       |
| <kbd>[prefix] b</kbd>   | `smudge-controller-previous-track`         | Previous track                                   |
| <kbd>[prefix] v u</kbd> | `smudge-controller-volume-up`              | Increase the volume [2]                          |
| <kbd>[prefix] v d</kbd> | `smudge-controller-volume-down`            | Decrease the volume [2]                          |
| <kbd>[prefix] v m</kbd> | `smudge-controller-volume-mute-unmute`     | Alternate the volume between 0 and 100 [2]       |
| <kbd>[prefix] p m</kbd> | `smudge-my-playlists`                      | Show your playlists                              |
| <kbd>[prefix] p f</kbd> | `smudge-featured-playlists`                | Show the featured playlists                      |
| <kbd>[prefix] p s</kbd> | `smudge-playlist-search`                   | Search for playlists                             |
| <kbd>[prefix] p u</kbd> | `smudge-user-playlists`                    | Show playlists for the given user                |
| <kbd>[prefix] p c</kbd> | `smudge-create-playlist`                   | Create a new playlist                            |
| <kbd>[prefix] t s</kbd> | `smudge-track-search`                      | Search for tracks                                |
| <kbd>[prefix] t r</kbd> | `smudge-recently-played`                   | List of recently played tracks                   |
| <kbd>[prefix] t l</kbd> | `smudge-save-playing-track-to-library`     | Save currently playing track to your Library     |
| <kbd>[prefix] t k</kbd> | `smudge-remove-playing-track-from-library` | Remove currently playing track from your Library |

[1] No proper support for this in D-Bus implementation for GNU/Linux
[2] This feature uses Spotify Connect and requires a premium subscription

Users of the package hydra may find the code below more convenient for managing
Spotify, _although this is isn't officially supported:_

```elisp
;; A hydra for controlling spotify.
(defhydra hydra-spotify (:hint nil)
"
^Search^                  ^Control^               ^Manage^
^^^^^^^^-----------------------------------------------------------------
_t_: Track               _SPC_: Play/Pause        _+_: Volume up
_m_: My Playlists        _n_  : Next Track        _-_: Volume down
_f_: Featured Playlists  _p_  : Previous Track    _x_: Mute
_u_: User Playlists      _r_  : Repeat            _d_: Device
^^                       _s_  : Shuffle           _q_: Quit
"
    ("t" smudge-track-search :exit t)
    ("m" smudge-my-playlists :exit t)
    ("f" smudge-featured-playlists :exit t)
    ("u" smudge-user-playlists :exit t)
    ("SPC" smudge-controller-toggle-play :exit nil)
    ("n" smudge-controller-next-track :exit nil)
    ("p" smudge-controller-previous-track :exit nil)
    ("r" smudge-controller-toggle-repeat :exit nil)
    ("s" smudge-controller-toggle-shuffle :exit nil)
    ("+" smudge-controller-volume-up :exit nil)
    ("-" smudge-controller-volume-down :exit nil)
    ("x" smudge-controller-volume-mute-unmute :exit nil)
    ("d" smudge-select-device :exit nil)
    ("q" quit-window "quit" :color blue))

(bind-key "a" #'hydra-spotify/body some-map)
```

A transient map can be enabled to allow repeating frequent commands
(defined in `smudge-transient-command-map`) without having to repeat the
prefix key for `smudge-command-map`.

```elisp
(setq smudge-player-use-transient-map t)
```

### Creating The Spotify App

Go to [Create an Application][app-create] and give your application a name and
a description:

![Creating a Spotify App 1/3](./img/spotify-app-01.png)

After creating the new app, click the **Edit Settings**, scroll down a little bit,
type <http://localhost:8080/smudge-api-callback> as the Redirect URI for the
application, and click **Add**. Then, hit **Save**.

![Creating a Spotify App 2/3](./img/spotify-app-02.png)

At this point, the client ID and the client secret are available, so set those values to
`smudge-oauth2-client-id` and `smudge-oauth2-client-secret`, respectively.

![Creating a Spotify App 3/3](./img/spotify-app-03.png)

## Usage

### Remote Minor Mode

To display the currently song in the mode line, you can enable the
`global-smudge-remote-mode`. The interval in which the player status is updated
can be configured via the `smudge-player-status-refresh-interval` variable:

```elisp
;; Updates the player status every 10 seconds (default is 5)
;; Note: Set 0 to disable this feature, and avoid values between 1 and 4 when
;; using the 'connect transport.
(setq smudge-player-status-refresh-interval 10)
```
#### Customizing The Player Status

The information displayed in the player status can be customized by setting the
desired format in `smudge-player-status-format`. The following placeholders
are supported:

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
the `smudge-player-status-truncate-length` variable.

```elisp
(setq smudge-player-status-truncate-length 10) ; default: 15
```

The text indicator for each of the following player statuses can be configured
via their corresponding variables:

| Player State  | Variable                                  | Default Value |
|:--------------|:------------------------------------------|:-------------:|
| Playing       | `smudge-player-status-playing-text`       | `"Playing"`   |
| Paused        | `smudge-player-status-paused-text`        | `"Paused"`    |
| Stopped       | `smudge-player-status-stopped-text`       | `"Stopped"`   |
| Repeating On  | `smudge-player-status-repeating-text`     | `"R"`         |
| Repeating Off | `smudge-player-status-not-repeating-text` | `"-"`         |
| Shuffling On  | `smudge-player-status-shuffling-text`     | `"S"`         |
| Shuffling Off | `smudge-player-status-not-shuffling-text` | `"-"`         |

#### Global Remote Mode

This mode can be enabled globally by running
<kbd>M-x global-smudge-remote-mode</kbd>.

### Searching For Tracks

To search for tracks, run <kbd>M-x smudge-track-search</kbd> and type in your
query. The results will be displayed in a separate buffer with the following
key bindings:

| Key              | Description                                                        |
|:-----------------|:-------------------------------------------------------------------|
| <kbd>a</kbd>     | Adds track to a playlist                                           |
| <kbd>l</kbd>     | Loads the next page of results (pagination)                        |
| <kbd>g</kbd>     | Clears the results and reloads the first page of results           |
| <kbd>k</kbd>     | Adds track(s) under the cursor (or inside the region) to the queue |
| <kbd>M-RET</kbd> | Plays the track under the cursor in the context of its album [1]   |

[1] D-Bus implementation for GNU/Linux do not support passing the context, so
only the track under the cursor will be played

The resulting buffer loads the `global-smudge-remote-mode` by default.

**Tip:** In order to customize the number of items fetched per page, just change
the variable `smudge-api-search-limit`:

```elisp
;; Do not use values larger than 50 for better compatibility across endpoints
(setq smudge-api-search-limit 50)
```

### Playing a Spotify URI

To ask Smudge to play a resource by URI, run
<kbd>M-x smudge-play-uri</kbd> and enter the resource URI.

### Creating Playlists

To create new playlists, run <kbd>M-x smudge-create-playlist</kbd> and follow
the prompts.

Currently it's not possible to add tracks to a playlist you own, or to remove
tracks from them.

### Searching For Playlists

To return the playlists for the current user, run
<kbd>M-x smudge-my-playlists</kbd>, or
<kbd>M-x smudge-user-playlists</kbd> to list the public playlists for some
given user. To search playlists that match the given search criteria, run
<kbd>M-x smudge-playlist-search CRITERIA</kbd>. Also, run
<kbd>M-x smudge-featured-playlists</kbd> in order to browse the featured
playlists from Spotify en_US.

Change the following variables in order to customize the locale and region for
the featured playlists endpoint:

```elisp
;; Spanish (Mexico)
(setq smudge-api-locale "es_MX")
(setq smudge-api-country "MX")
```

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

Once you open the list of tracks of a playlist, you get the following key
bindings in the resulting buffer:

| Key              | Description                                                         |
|:-----------------|:--------------------------------------------------------------------|
| <kbd>a</kbd>     | Adds track to a playlist                                            |
| <kbd>r</kbd>     | Removes track from current playlist                                 |
| <kbd>l</kbd>     | Loads the next page of results (pagination)                         |
| <kbd>g</kbd>     | Clears the results and reloads the first page of results            |
| <kbd>f</kbd>     | Follows the current playlist                                        |
| <kbd>u</kbd>     | Unfollows the current playlist                                      |
| <kbd>k</kbd>     | Adds track(s) under the cursor (or inside the region) to the queue  |
| <kbd>M-RET</kbd> | Plays the track under the cursor in the context of the playlist [1] |

Both buffers load the `global-smudge-remote-mode` by default.

[1] D-Bus implementation for GNU/Linux do not support passing the context, so
only the track under the cursor will be played

## Selecting a Device for Playback

<kbd>M-x smudge-select-device</kbd> will display a list of devices available for playback in a separate buffer.

Note: use of this feature requires a Spotify premium subscription.

Once you open the list of devices, you get the following key bindings in the resulting buffer:

| Key            | Description                                       |
|:---------------|:--------------------------------------------------|
| <kbd>RET</kbd> | Transfer playback to the device under the cursor. |
| <kbd>g</kbd>   | Reloads the list of devices                       |

## Specifying the Player Status Location

By default, the player status (playing, paused, track name, time, shuffle, repeat, etc.) are shown
in the modeline. If you want to display the status in the title bar when using a graphical display,
you can set the following:

```elisp
(setq smudge-status-location 'title-bar)
```

Valid values include `'title-bar`, `'modeline` and `nil`, where nil turns off the display of the
player status completely. If the value is set to `title-bar` but you are not using a graphical
display, the player status will be displayed in the mode line instead.

If you want to customize the separator between the existing title bar text and the player status,
you can set the following, i.e.:

```elisp
(setq smudge-title-bar-separator "----")
```

Otherwise, it defaults to 4 spaces.

## License

Copyright (C) Daniel Fernandes Martins

Distributed under the GPL v3 License. See COPYING for further details.

[app-list]: https://developer.spotify.com/dashboard
[app-create]: https://developer.spotify.com/dashboard/create
[kbd-conv]: https://www.gnu.org/software/emacs/manual/html_node/elisp/Key-Binding-Conventions.html
