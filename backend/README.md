# Accessibility API and portal provider
A graphQL- and REST-based API for accessibility information on geographical locations. This is a prototype for the Swedish IoT Hub for Accessibility. The server also publishes the portal single page application that can be accessed from a web browser or a mobile phone browser.

A geographical location in this prototype contains a unique name, a description, its geodetic position (in WGS84), the level of accessability (1-5), what source (Manual, Automatic) that sets the state of the location (Unknown, Online, Offline).

It is written in Haskell and uses the morpheus graphQL resolver and the Yesod web framework together with Persistent to handle a PostgreSQL database with the postgis extender for spatial and geographic objects.

## Build
To be able to build the portal you need the following (version is just indicative):

1. stack 2.1.3

Install stack according to its [Homepage](https://docs.haskellstack.org/en/stable/README/).

After this you can build the backend either using the makefile in the root of the git repository or just write stack build in the backend directory. I would recommend using the makefile when building to be able to run the backend. Otherwise while programming just use stack build in the directory to verify your code.

```
stack build
```
or

```
make build-backend
```
## Environment variables
The server is configured by setting the following environment variables:

```
export HAPI_KEY=/home/tomas/haskell/haccessability/deployment/tls.key
export HAPI_CERTIFICATE=/home/tomas/haskell/haccessability/deployment/tls.pem
export HAPI_DATABASE=postgresql://uid:password@172.17.0.2:5432/heat
export HAPI_JWT_SESSION_LENGTH=3600
export HAPI_PASSWORD_COST=10
```

The **HAPI_KEY** and **HAPI_CERTIFICATE** points to the certificate and key used for the TLS setup.

The **HAPI_DATABASE** contains the URL to the PostGIS database.

The **HAPI_JWT_SESSION_LENGTH** contains the session timeout value.

The **HAPI_PASSWORD_COST** contains the bcrypt cost factor for the password hashing.

## Portal
The portal is located at **/index.html** and can be accessed from a PC or mobile browser.

## REST interface
The REST interface is located at **/api** and content type is **application/json**.

### Fetch an item - GET /api/item/<item key>
Returns with the item as JSON.
  
### Update an item - PUT /api/item/<item_key>
The PUT body contains the fields on the item that is to be updated. The item_key points to the item.

### Create an item - POST /api/item
The POST body contains the item as JSON and the item is returned and the item_key is added.

### Fetch a list of items - POST /api/items
The POST body contains the selection filters and a list of items is returned as JSON.

## graphQL schema and interface
The graphQL interface is located at **/gql** and is in GraphQL format, the content type must be **application/json**.

The following types are defined in the graphQL schema:

```
enum ItemSource { Manual, Automatic}
enum ItemLevel { L0, L1, L2, L3, L4, L5}
enum ItemState { Unknown, Online, Offline }

type Item {
  itemID:ID
  itemName:String!
  itemDescription:String!
  itemSource:ItemSource!
  itemState:ItemState!
  itemLevel:ItemLevel!
  itemLatitude:Float!
  itemLongitude:Float!
  itemDistance:Float!
}
```

### The Query Type
```
queryItem(
  queryItemName:String!
  ):Item
  
queryItems(
  queryItemsLongitude:Float!
  queryItemsLatitude:Float!
  queryItemsDistance:Float!
  queryItemsLimit:Int!
  ):[Item!]!
```
#### queryItem Example

```
query FetchThemAll { queryItem (
  queryItemName : "Sundsvall Centralstation")
  {
    itemID
    itemName
    itemDescription
    itemSource
    itemState
    itemLevel
    itemLongitude
    itemLatitude
    itemDistance
  }
}
```
#### queryItems Example

```
query FetchThemAll {
  queryItems (
    queryItemsLongitudeMin : 10.1
    queryItemsLongitudeMax : 20.1
    queryItemsLatitudeMin : 50.1
    queryItemsLatitudeMax : 80.1)
  {
    itemID
    itemName
    itemDescription
    itemLongitude
    itemLatitude
  }
}
```

### The mutation type
```
createItem(
  createItemName:String!
  createItemDescription:String!
  createItemSource:ItemSource!
  createItemState:ItemState!
  createItemLevel:ItemLevel!
  createItemLongitude:Float!
  createItemLatitude:Float!
  ):Item!
  
deleteItem(
  deleteItemName:String!
  ):Item
  
updateItem(
  updateItemID:ID!
  updateItemName:String
  updateItemDescription:String
  updateItemSource:ItemSource
  updateItemState:ItemState
  updateItemLevel:ItemLevel
  updateItemLongitude:Float
  updateItemLatitude:Float
  ):Item
```
#### deleteItem Example
```
mutation DeleteItem {
  deleteItem (
    deleteItemName : "NP8 Arena"
  )
}
```
#### createItem Example
```
mutation CreateANewOne { createItem ( 
  createItemName : "NP8 Arena"
  createItemDescription : "The central soccer stadium"
  createItemSource : Manual
  createItemLevel : L3
  createItemState : Online
  createItemLongitude : 25.3156
  createItemLatitude : 62.3369)
  {
    itemID
    itemName
    itemDescription
    itemSource
    itemState
    itemLevel
    itemLongitude
    itemLatitude
  }
}
```
#### updateItem Example
```
mutation UpdateAnItem { updateItem ( 
  updateItemID : "8"
  updateItemName : "Statoil")
  {
    itemID
    itemName
    itemDescription
    itemSource
    itemState
    itemLevel
    itemLongitude
    itemLatitude
  }
}
```
