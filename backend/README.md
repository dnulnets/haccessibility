# Accessibility API and portal provider
A graphQL- and REST-based API for accessibility information on geographical locations. This is a prototype for the Swedish IoT Hub for Accessibility - Case 3. The server also publishes the portal single page application that can be accessed from a web browser or a mobile phone browser.

A geographical location in this prototype contains a unique name, a description, its geodetic position (in WGS84), the level of accessability (1-5), what source (Manual, Automatic) that sets the state of the location (Unknown, Online, Offline). It also contains a set of attributes that can be compared with the users required attributes to determine what point is accessible.

It is written in Haskell and uses the morpheus graphQL resolver and the Yesod web framework together with Persistent to handle a PostgreSQL database with the postgis extender for spatial and geographic objects.

## Build
To be able to build the portal you need the following (version is just indicative, and others might work):

1. stack 2.5.1
2. libpq-dev

Install stack according to its [Homepage](https://docs.haskellstack.org/en/stable/README/).

After this you can build the backend either using the makefile in the root of the git repository or just write stack build in the backend directory.

```
stack build
```
or

```
make build-backend
```
## Environment variables
The server is configured by setting the following environment variables:

```
export HAPI_KEY=/home/tomas/haskell/haccessability/deployment/tls.key
export HAPI_CERTIFICATE=/home/tomas/haskell/haccessability/deployment/tls.pem
export HAPI_DATABASE=postgresql://uid:password@172.17.0.2:5432/heat
export HAPI_JWT_SESSION_LENGTH=3600
export HAPI_JWT_SECRET="<changeme>"
export HAPI_PASSWORD_COST=10
```

The **HAPI_KEY** and **HAPI_CERTIFICATE** points to the certificate and key used for the TLS setup.

The **HAPI_DATABASE** contains the URL to the PostGIS database.

The **HAPI_JWT_SESSION_LENGTH** contains the session timeout value.

The **HAPI_JWT_SEECRET** contains the secret used to create the token.

The **HAPI_PASSWORD_COST** contains the bcrypt cost factor for the password hashing.
