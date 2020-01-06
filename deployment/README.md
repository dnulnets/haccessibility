# Accessibility API and portal docker build
Note that this is work in progress and do not currently contain the portal.
## Database image - Dockerfile.db
Contains the setup of the database image. The image is named haccdb and tagged with 1, 1.0 and latest.
Created by running makefile image-db from the root directory.
## Build environemnt image (slow build) - Dockerfile.build-env
Contains the setup of the build image, that is the image that is used to build the service quickly. The image is named haccbuild and tagged with 1, 1.0 and latest.

Created by running makefile image-build-env from the root directory.

This takes time due to the setup from the FP-complete stack environment, but this environment is reused when using the Dockerfile.build.
## Service image (slow build) - Dockerfile.svc
Contains the setup of the service image. The image is named haccsvc and tagged with 1, 1.0 and latest.

Created by running makefile image-svc from the root directory.

This takes time due to the multistage setup starting from FP-completets stack environment causing a long time just setting it up.
## Service image (using build environment image) - Dockerfile.build
Contains the setup of the service image. The image is named haccsvc and tagged with 1, 1.0 and latest.

Created by running makefile image-build-svc from the root directory.

This takes a shorter time than the other service image because it uses the haccbuild image from above in the multistage setup.
