# Accessibility Portal
An accessibility portal for mobile and PC browsers that allows you to enter accessibility information on geographical positions. This is a prototype for the Swedish IoT Hub for Accessibility.

It is written in Purescript and the declarative, type-safe UI library Halogen. It uses the backend to store and retrieve information through its REST-interface. The backend also serves as the provider of the single page application to the browser.

## Build
To be able to build the portal you need the following (version is just indicative):

1. node.js 12.18.3
2. npm 6.14.6
3. purs 0.13.8
4. spago 0.16.0
5. parcel 1.12.4
6. ol 6.4.3

Install node.js according to its [Homepage](https://nodejs.org/en/) and set up npm to use a local global store and install purescript, spago and parcel. Note that purescript requires libtinfo5 to be installed.

```
apt-get install libtinfo5 # Or similar
npm install -g purescript
npm install -g spago
npm install -g parcel
npm install ol
```

After this you can build the portal using the makefile in the root of the git repository or just write the following commands in the portal directory. **./generate.sh** generates the **Version.purs** file that contains the build timestamp.

In the root of the git repository.

```
make build-portal
```

Or in the portal directory.

```
./generate.sh
spago build
spage bundle-app
rm -fR ../backend/static
parcel build index.html -d ../backend/static index.html
```
