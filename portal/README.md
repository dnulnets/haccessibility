# Accessibility Portal
An accessibility portal for mobile and PC browsers that allows you to enter accessibility information on geographical positions. This is a prototype for the Swedish IoT Hub for Accessibility.

It is written in Purescript and the declarative, type-safe UI library Halogen. It uses the backend to store and retrieve information through its REST-interface. The backend also serves as the provider of the single page application to the browser.

## Build
To be able to build the portal you need the following (version is just indicative):

1. node.js 12.4.0
2. npm 6.13.4
3. purs 0.13.5
4. spago 0.13.0
5. parcel 1.12.4

Install node.js according to its [Homepage](https://nodejs.org/en/) and set up npm to use a local global store and install purescript, spago and parcel. Note that purescript requires libtinfo5 to be installed as well.

```
npm install -g purescript
npm install -g spago
npm install -g parcel
```

After this you can build the portal either using the makefile in the root of the git repository or just write spago build in the portal directory. I would recommend using the makefile when building to be able to run the backend. Otherwise while programming just use spago build in the directory to verify your code. 

```
spago build
spage bundle-app
parcel build index.html -d ../backend/static index.html
```
or

```
make build-portal
```
