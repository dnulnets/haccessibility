# Accessability API
A graphQL- and REST-based interface for accessability information on geographical locations. This is a simple prototype for the Swedish IoT Hub for Accessability.

It is written in Haskell and uses the morpheus graphQL resolver and the Yesod web framework together with Persistent to handle a postgreSQL database. When finished it will contain docker images and kubernetes setup of the system.

## Geographical location

A basic geographical location in this prototype contains a unique name, a description, its geodetic position (in WGS84), the level of accessability (1-5), what source (Manual, Automatic) that sets the state of the location (Unknown, Online, Offline).

More information is supposed to be added to the location based on the type of location, such as store, restaurant, public space, etc. (**Not done yet**)

## Cyber Security

Currently no scyber security is built in, it will be added later but kept in mind when developing. It will be a TLS connection and an API-key type setup.
