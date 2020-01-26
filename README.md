# Accessibility API and portal
This is a prototype, among many other prototypes and solutions, for the Swedish IoT Hub for Accessibility. This prototype allows you to manually or automatically through IoT accessibility enabled devices enter information, such as availability, on geographical positions.

The wiki for this prototype is [here](https://github.com/dnulnets/haccessibility/wiki)

The github location of the Swedish IoT Hub Source code is [here](https://github.com/orgs/iot-for-tillgenglighet/dashboard).

## Purpose and goal

The project is based on Agenda 2030 and the goal of sustainable cities and communities and to provide universal access to safe, inclusive and accessible green spaces and public places, especially for women and children, the elderly and people with disabilities. The purpose is to provide information that promotes mobility and accessibility to public spaces. The project thus aims to achieve its goal of contributing to increased social interactions, increased quality of life and increased mental well-being for the project´s target groups.

## Backend, portal and deployment information
Information on the [backend](backend/README.md), [portal](portal/README.md) and [deployment](deployment/README.md) can be found in the readme files for each part.

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
