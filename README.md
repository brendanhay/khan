# Khan

## Table of Contents

* [Contribute](#contribute)
* [Development](#development)
    - [Library](#library)
    - [CLI](#cli)
    - [Metadata Sync](#khan-metadata-sync)
    - [Metadata Server](#khan-metadata-server)
* [Licence](#licence)


## Contribute

For any problems, comments or feedback please create an issue [here on GitHub](github.com/brendanhay/khan/issues).


## Development

### Library

The core types and functions are located under `khan`. This provides an abstraction
over typical command line usage and a thin layer over Amazon related functionality
in the form of model logic.

### CLI

`khan-cli` contains the monolithic interfaces that define top-level commands and subcommands.

This should slowly be reduced to functionality that is related to provisioning
with other functionality (such as specifics to development) being moved to separate binaries.

### Metadata

When developing features which are planned to run solely on EC2 instances it is
typically difficult to test the behaviour of logic that is reliant on the underlying
metadata lookup mechanisms that utilise `instance-data` from `http://169.254.169.254`.

To that end, two separate utilities are provided to emulate `instance-data` and
make it easy to test your features locally.

#### khan-metadata-sync

The synchronisation utility needs to be deployed onto the EC2 instance you wish
to emulate. (it is part of the resulting debian package by default.)

Running `khan-metadata-sync` will crawl the local `instance-data` and will result
in a compressed artifact named `metadata.tar.gz` in your `cwd`.

The internal structure of this artifact mirrors the url layout that is available
in the `instance-data`, and can be used by `khan-metadata-server`.

#### khan-metadata-server

A Ruby web server is provided under `khan-metadata-server`.

> For convenience symlinks for the built binaries can be found under `bin` in the project root.

This server needs to be run with `sudo` and the name of the network interface to
shape traffic for. On `OSX` the services can be found by running `networksetup -listallnetworkservices`.

The server will then remove any DNS servers for the selected interface and inspect
all lookups - modifying the packets to ensure that any requests to `http://instance-data`
are proxied to `localhost`.

#### Example

The compressed `metadata.tar.gz` artifact from running `khan-metadata-sync` needs to
be extracted into `khan-metadata-server/www`.

```
→ rm -rf khan-metadata-server/www/latest
→ tar -C khan-metadata-server/www -xvf metadata.tar.gz
x ./
x ./latest/
x ./latest/meta-data/
x ./latest/meta-data/network/
x ./latest/meta-data/network/interfaces/
...
```

Typically, if you are connected to the network on WLAN, the name of the interface on `OSX` would be `Wi-Fi`.

```
→ ./bin/khan-metadata-server Wi-Fi
Password: *****
Adding 169.254.169.254 to lo0
Redirecting http://instance-data to localhost
I, [2014-08-11T10:47:48.247533 #86615]  INFO -- : Starting RubyDNS server (v0.8.5)...
I, [2014-08-11T10:47:48.247621 #86615]  INFO -- : Listening on tcp:0.0.0.0:53
I, [2014-08-11T10:47:48.247784 #86615]  INFO -- : Listening on udp:0.0.0.0:53
Serving metadata on http://169.254.169.254/latest
Thin web server (v1.6.2 codename Doc Brown)
Maximum connections set to 1024
Listening on 169.254.169.254:80, CTRL+C to stop
```


## Licence

khan is released under the [Mozilla Public License Version 2.0](http://www.mozilla.org/MPL/)
