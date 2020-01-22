# Accessibility API and portal docker build
Note that this is work in progress and do not currently contain the portal.
## Database image - Dockerfile.db
Contains the setup of the database image. The image is named haccdb and tagged with 1, 1.0 and latest.
Created by running makefile image-db from the root directory.
## Haskell Build environment image (slow build) - Dockerfile.haskell-build-env
Contains the setup of the build image that is used to build the haskell service quickly. The image is named haccbuild and tagged with 1, 1.0 and latest.

Created by running makefile image-haskell-build-env from the root directory.

This takes time due to the setup from the FP-complete stack environment, but this environment is reused when using the Dockerfile.build.
## Purescript Build environment (slow build) - Dockerfile.purescript-build-env
Contains the setup of the build image that is used to build the purescript portal quickly. The image is named paccbuild and tagged with 1, 1.0 and latest.

Created by running makefile image-purescript-build-env from the root directory.

## Server image (uses the haskell build environment image) - Dockerfile.build
Contains the setup of the server image. The image is named haccsvc and tagged with 1, 1.0 and latest. It does not currently contain the portal.

Created by running makefile image-build-server from the root directory.
