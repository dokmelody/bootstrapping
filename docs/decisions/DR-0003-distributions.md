# Context

DokMelody can be used for different problem domains, and it can run in many different run-time environments.

# Decision

Favour fork of DokMelody, and call them distributions.

The upstream DokMelody project will favour meta-programming and flexibility, but every distribution is a distinct project, with:

* a specific usage-scenario/domain 
* a potentially distinct community of developers
* a distinct name and marketing strategy
* maybe an explicit reference to the upstream DokMelody distribution it is forking and specializing

# Consequences

Pros:

* avoiding pointless discussions on things that must be managed in different ways
* possibility to explore different technical solutions

Cons:

* possible confusion about too much different DokMelody distributions

