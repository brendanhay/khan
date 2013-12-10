n# Khan

## Table of Contents

* [Terminology and Concepts](#terminology-and-concepts)
* [Contribute](#contribute)
* [Licence](#licence)


## Terminology and Concepts

An overview of the key terms here in an attempt to provide a somewhat comprehensive
introduction, with AWS nomenclature mapped directly to usage where possible to avoid
any confusion.

### Immutability

The key tenant behind Khan's approach to infrastructure provisioning is immutability,
with opinion a close second.

Khan provides a workflow centered around immutable build artifacts (in the form of AMIs)
which are continuously deployed into `[1..n]` segregated environments.

These versioned artifacts are then launched into an environment which dicates what
traffic and service instances are visible, ensuring that entire (or subsets thereof)
environments are consistent and reproducible.


### Persistent vs Ephemeral

Terminology and workflow is described to outline a layer of separation between the
stateful and stateless infrastructure services.

<p align="center">
  <img src="http://brendanhay.github.io/khan/img/immutability.png" alt="immutability">
</p>

The lower persistent half of the diagram is comprised of services that don't (easily or sanely)
fit with a continuous deployment paradigm due to requirements such as exclusive durable storage,
or sensitive orchestration scenarios over which fine grained control is required.

This style of service has good pre-existing tooling for both Orchestration and
Configuration Management - we attempt to leverage these to avoid reinventing the
wheel where possible.

#### Persistent

Traditional services and applications that are problematic to shoehorn into a
continuously deployed container workflow.

These services require thoughtful (read: manual) coordination and are not part
of an Auto Scaling Group.

Examples:

* Jenkins Master
* MySQL/PostgreSQL Master
* Nagios


#### Ephemeral

In order to support wide distribution, availability, and redundancy
Khan utilises Auto Scaling Groups and a service discovery mechanisim to route
traffic to the desiginated set of service instances.

> The design constraints for these services are provided [here](#constraints).

Examples:

* Web Applications
* Backend Services
* Jenkins Slaves


### Environments

### Versioning

### Service Discovery

### Key Pairs

### Role

### Group

### Application

### Instance

### Host


## Contribute

For any problems, comments or feedback please create an issue [here on GitHub](github.com/brendanhay/khan/issues).


## Licence

khan is released under the [Mozilla Public License Version 2.0](http://www.mozilla.org/MPL/)
