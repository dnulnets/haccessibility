# Accessability database and portal
This is a prototype for the Swedish IoT Hub for Accessability that allows you to manually or automatically through IoT accessability enabled devices enter information, such as availability, on geographical positions.

## Technology stack
The backend is developed in Haskell and uses the Yesod webframework and Morpheus graphQL library.

The portal is developed in Purescript and uses the declarative type-safe UI library Halogen.

The plan is to be able to build docker images for the database and backend server to allow for easy deployment and production.

Please see README.md files in the subdirectories for more information.
