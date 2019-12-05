# Accessability API
A graphQL- and REST-based interface for accessability information on geographical locations. This is a simple prototype for the Swedish IoT Hub for Accessability.

It is written in Haskell and uses the morpheus graphQL resolver and the Yesod web framework together with Persistent to handle a postgreSQL database. When finished it will contain docker images and kubernetes setup of the system.

## Geographical location

A basic geographical location in this prototype contains a unique name, a description, its geodetic position (in WGS84), the level of accessability (1-5), what source (Manual, Automatic) that sets the state of the location (Unknown, Online, Offline).

More information is supposed to be added to the location based on the type of location, such as store, restaurant, public space, etc. (**Not done yet**)

## graphQL schema
The graphQL interface is located at **/gql** and is in GarphQL format, the content type must be **application/json**.

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
}
```

### The Query Type
```
queryItem(
  queryItemName:String!
  ):Item
  
queryItems(
  queryItemsLongitudeMin:Float!
  queryItemsLatitudeMin:Float!
  queryItemsLatitudeMin:Float!
  queryItemsLongitudeMax:Float!
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
