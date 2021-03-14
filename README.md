# Accessibility API and portal
This is a prototype for the Swedish IoT Hub for Accessibility - Case 3. This prototype allows you to manually or automatically through IoT accessibility enabled devices enter information on the accessibility of geographical positions, points of interests. It also determines the level of accessibility based on personal settings and attributes on the geographical position.

<img src="https://github.com/dnulnets/haccessibility/blob/master/doc/webapp.png" width="200" />

The wiki for this prototype: [Accessibility Wiki](https://github.com/dnulnets/haccessibility/wiki)

The readme for the backend: [Haskell Backend](https://github.com/dnulnets/haccessibility/blob/master/backend/README.md)

The readme for the portal: [Purescript frontend](https://github.com/dnulnets/haccessibility/blob/master/portal/README.md)

The github location of the Swedish IoT Hub for accesibility: [here](https://github.com/orgs/iot-for-tillgenglighet/dashboard).

## Purpose and goal
The project is based on Agenda 2030 and the goal of sustainable cities and communities and to provide universal access to safe, inclusive and accessible green spaces and public places, especially for women and children, the elderly and people with disabilities. The purpose is to provide information that promotes mobility and accessibility to public spaces. The project thus aims to achieve its goal of contributing to increased social interactions, increased quality of life and increased mental well-being for the project´s target groups.

## Technology stack
The backend is developed in Haskell and uses Persistence, Yesod webframework and Morpheus graphQL library. The database is PostreSQL.

The portal is developed in Purescript and uses the declarative type-safe UI library Halogen.

The entire solution is run on a kubernetes cluster, containing ingress, services, stateful and stateless deployments and loadbalacing.

Please see README.md files in the subdirectories for more information.

### Haskell
A statically typed, concurrent, lazy, purely functional programming language that today is used outside academia and used by several companies for production with an extensive package eco system.

See [Haskell homepgae](https://www.haskell.org/)

See [Haskell in industry](https://wiki.haskell.org/Haskell_in_industry)

### Purescript
A statically typed, functional programming language with an extensive package eco system that compiles to JavaScript much the same way as typescript does.

See [Purescript homepage](http://www.purescript.org/)
