* Write proper readme
* Use `interpolate` for string interpolation
* Better error messages. For example when parsing the `pacmd` output we
  should throw an error with an informative message whenever a step fails.
  - Inform when could not connect to dbus
  - Inform when notification service does not exist
  - Inform when pacmd not available or fails
* Use strict 'Data.Text.IO' instead of lazy IO everywhere.
* The `pacmd` mock currently changes all lines starting with
  `set-sink-mute`. It should only change the one for the given sink. We
  should then also test against this.
* Set XDG environment variables for tests so that we can check that we write to
  the cache.
