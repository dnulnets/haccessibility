# Accessability API Docker build
## Database image - Dockerfile.db
Contains the setup of the database image. The image is named haccdb and tagged with 1, 1.0 and latest.
Created by running makefile image-db from the root directory.
## Service image (slow build) - Dockerfile.svc
Contains the setup of the service image. The image is named haccsvc and tagged with 1, 1.0 and latest.
Created by running makefile image-svc from the root directory. This takes time due to the multistage setup starting from FP-completets stack environment causing a long time just setting it up.
## Build environemnt image (slow build) - Dockerfile.build-env
Contains the setup of the build image. The image is named haccbuild and tagged with 1, 1.0 and latest.
Created by running makefile image-build-env from the root directory. This takes time due to the setup from the FP-complete stack environment, but this environment is reused when using the Dockerfile.build.
## Service image (using build environment image) - Dockerfile.build
Contains the setup of the service image. The image is named haccsvc and tagged with 1, 1.0 and latest.
Created by running makefile image-build-svc from the root directory. This takes a short time because it uses the build-env image from above.
