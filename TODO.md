* Better error messages. For example when parsing the `pacmd` output we
  should throw an error with an informative message whenever a step fails.
  - Inform when could not connect to dbus
  - Inform when notification service does not exist
  - Inform when pacmd not available or fails
* Use strict 'Data.Text.IO' instead of lazy IO everywhere.
* Use Template Haskell string interpolation in tests
