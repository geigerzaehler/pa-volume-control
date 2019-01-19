# pa-volume-control

Command line volume control for PulseAudio. Heavily inspired by
[`pavolume`][pavolume].

`pa-volume-control` is a command line tool that allows you to change the volume
of the PulseAudio default sink. In addition the tool uses the DBus desktop
notification service to show the changed volume.

## Getting started

Download the [latest release binary][releases] from Github. Now you can try the
following commands to control your volume

    pa-volume-control volup
    pa-volume-control volup --no-limit
    pa-volume-control voldown
    pa-volume-control mutetoggle
    pa-volume-control source-mute-toggle

The `--no-limit` flag allows you to increase the volume beyond 100%, possibly
introducing clipping.

[releases]: https://github.com/geigerzaehler/pa-volume-control/releases

## Using with awesome

If you are using [awesome](http://awesome.naquadah.org/), you can use the
following key bindings to control `pa-volume-control`:

    awful.key({         }, "XF86AudioLowerVolume", function() awful.util.spawn("pa-volume-control volup") end),
    awful.key({         }, "XF86AudioRaiseVolume", function() awful.util.spawn("pa-volume-control voldown") end),
    awful.key({ "Shift" }, "XF86AudioRaiseVolume", function() awful.util.spawn("pa-volmue-control volup --nolimit") end),
    awful.key({         }, "XF86AudioMute",        function() awful.util.spawn("pa-volume-control mutetoggle") end),
    awful.key({         }, "XF86AudioMicMute",     function() awful.util.spawn("pa-volume-control source-mute-toggle") end),

## Differences from `pavolume`

- Volume notifications are replaced instead of stacked so you don’t end up with
  a bunch of volume notifications.
- Controls the volume of the output configured as the default instead of the
  first one.
- Can toggle mute state of the default source
- Only a subset of the command is supported
- Configuration file is not supported


## License

BSD-3-Clause

[pavolume]: https://github.com/sseemayer/pavolume
[stack]: https://docs.haskellstack.org/en/stable/README/#how-to-install
