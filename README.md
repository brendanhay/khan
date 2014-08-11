# Khan

## Table of Contents

* [Contribute](#contribute)
* [Developing](#developing)
    - [Metadata Sync](#khan-metadata-sync)
    - [Metadata Server](#khan-metadata-server)
* [Licence](#licence)


## Contribute

For any problems, comments or feedback please create an issue [here on GitHub](github.com/brendanhay/khan/issues).


## Developing

When developing features which are planned to run solely on EC2 instances it is
typically difficult to test the behaviour of logic that is reliant on the underlying
metadata lookup mechanisms that utilise `instance-data` from `http://169.254.169.254`.

To that end, two separate utilities are provided to emulate `instance-data` and
make it easy to test your features locally.

### khan-metadata-sync

The synchronisation utility needs to be deployed onto the EC2 instance you wish
to emulate. (it is part of the resulting debian package by default.)

Running `khan-metadata-sync` will crawl the local `instance-data` and will result
in a compressed artifact named `metadata.tar.gz` in your `cwd`.

The internal structure of this artifact mirrors the url layout that is available
in the `instance-data`, and can be used by `khan-metadata-server`.

### khan-metadata-server

A Ruby web server is provided under `khan-metadata-server`.
(locally a symlink can be found under `bin` in the project root.)

This server needs to be run with `sudo` and the name of the network interface to
shape traffic for. On `OSX` the services can be found by running `networksetup -listallnetworkservices`.

> Typically, if you are connected to the network on WLAN, the name of the interface on `OSX` would be `Wi-Fi`.

The server will then remove any DNS servers for the selected interface and inspect
all lookups - modifying the packets to ensure that any requests to `http://instance-data`
are proxied to `localhost`.


## Licence

khan is released under the [Mozilla Public License Version 2.0](http://www.mozilla.org/MPL/)
