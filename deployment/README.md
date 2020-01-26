# Accessibility API and portal docker build
## Database image - Dockerfile.build-db
Contains the setup of the database image. The image is named haccdb and tagged with 1, 1.0 and latest.

Created by running makefile image-build-db from the root directory.
## Haskell Build environment image (slow build) - Dockerfile.haskell-build-env
Contains the setup of the build image that is used to build the haskell service quickly. The image is named haccbuild and tagged with 1, 1.0 and latest.

Created by running makefile image-haskell-build-env from the root directory.

This takes time and generates a large image due to the setup from the FP-complete stack environment, but this environment is reused when using the Dockerfile.build.
## Purescript Build environment - Dockerfile.purescript-build-env
Contains the setup of the build image that is used to build the purescript portal quickly. The image is named paccbuild and tagged with 1, 1.0 and latest.

Created by running makefile image-purescript-build-env from the root directory.
## Server image (uses the haskell and purescript build environment image) - Dockerfile.build-server
Contains the setup of the server image. The image is named haccsvc and tagged with 1, 1.0 and latest. It uses the haccbuild:latest and paccbuild:latest images to build the server, so they must be generated in advance.

Created by running makefile image-build-server from the root directory.

The server is configured by setting the following environment variables, e.g. you can use the **--env-file** switch on the **docker run** command.

```
HAPI_KEY=/home/tomas/haskell/haccessability/deployment/tls.key
HAPI_CERTIFICATE=/home/tomas/haskell/haccessability/deployment/tls.pem
HAPI_DATABASE=postgresql://uid:password@172.17.0.2:5432/heat
HAPI_JWT_SESSION_LENGTH=3600
HAPI_PASSWORD_COST=10
```

The **HAPI_KEY** and **HAPI_CERTIFICATE** points to the certificate and key used for the TLS setup.

The **HAPI_DATABASE** contains the URL to the PostGIS database.

The **HAPI_JWT_SESSION_LENGTH** contains the session timeout value.

The **HAPI_PASSWORD_COST** contains the bcrypt cost factor for the password hashing.

