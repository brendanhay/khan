# Khan

## Table of Contents

* [Terminology and Concepts](#terminology-and-concepts)
* [Contribute](#contribute)
* [Licence](#licence)


## Terminology and Concepts

Nomenclature is mapped directly to the AWS usage where possible to avoid any confusion.

I've re-iterated the key terms here in an attempt to provide a comprehensive overview.

### Immutability

The key tenant behind Khan's approach to infrastructure provisioning is immutability,
with opinion a close second.

Although as you will see we immediately throw the first tenant to the wolves as
necessitated by the following architecture:

<p align="center">
  <img src="http://brendanhay.github.io/khan/img/immutability.png" alt="immutability">
</p>

### Persistent vs Ephemeral

Terminology and workflow is used to outline a layer of separation between the
stateful and stateless infrastructure services.

#### Persistent

Traditional services and applications that are problematic to shoehorn into a
continuously deployed container workflow.

These services require thoughtful (read: manual) coordination and are not part
of an Auto Scaling Group.

Examples:

* Jenkins Master
* MySQL/PostgreSQL Master
* Nagios

> Diagram

#### Ephemeral

In order to support wide distribution, availability, and redundancy
Khan utilises Auto Scaling Groups and a service discovery mechanisim to route
traffic to the desiginated set of service instances.

> The design constraints for these services are provided [here](#constraints).

Examples:

* Web Applications
* Backend Services
* Jenkins Slaves

> Diagram

### Environments


### Versioning


### Service Discovery


### Key Pair

Khan creates and saves private key information locally (under `~/.khan` by default) and
uses these keys to launch and connect to persistent and ephemeral instances.

Read the [AWS documentation](http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-key-pairs.html)
for more information.

### Role

### Group

###


### Application

### Instance

### Host


## Contribute

For any problems, comments or feedback please create an issue [here on GitHub](github.com/brendanhay/khan/issues).


## Licence

khan is released under the [Mozilla Public License Version 2.0](http://www.mozilla.org/MPL/)
