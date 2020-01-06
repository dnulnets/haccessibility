# Accessibility database and portal
This is a prototype for the Swedish IoT Hub for Accessibility that allows you to manually or automatically through IoT accessibility enabled devices enter information, such as availability, on geographical positions.

## Technology stack
The backend is developed in Haskell and uses the Persistence, Yesod webframework and Morpheus graphQL library. The database is PostreSQL.

The portal is developed in Purescript and uses the declarative type-safe UI library Halogen.

The plan is to be able to build docker images for the database and backend server to allow for easy deployment and production.

Please see README.md files in the subdirectories for more information.

### Haskell
A statically typed, concurrent, lazy, purely functional programming language that today is used outside academia and used by several companies for production with an extensive package eco system.

See [Haskell homepgae](https://www.haskell.org/)

See [Haskell in industry](https://wiki.haskell.org/Haskell_in_industry)

### Purescript
A statically typed, functional programming language with an extensive package eco system that compiles to JavaScript much the same way as typescript does.

See [Purescript homepage](http://www.purescript.org/)
