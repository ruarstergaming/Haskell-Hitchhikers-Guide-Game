module Actions where

import World


data Commands = Go Directions | Get Object | Do Action | Quit | Inv

actions :: String -> Maybe Action
actions "go"      = Just go
actions "get"     = Just get
actions "drop"    = Just put
actions "pour"    = Just pour
actions "examine" = Just examine
actions "drink"   = Just drink
actions "open"    = Just open
actions _         = Prelude.Nothing

commands :: String -> Maybe Command
commands "quit"      = Just quit
commands "inventory" = Just inv
commands _           = Prelude.Nothing



{- Given a direction and a room to move from, return the room id in
   that direction, if it exists.

e.g. try these at the ghci prompt

*Main> move "north" bedroom
Just "kitchen"

*Main> move "north" kitchen
Nothing
-}

move :: String -> Room -> Maybe String
move dir rm = findExit dir (exits rm)

findExit :: String -> [Exit] -> Maybe String
findExit dir [] = Prelude.Nothing
findExit dir (e:es) = (\e es -> if exit_dir e == dir then Just (room e) else findExit dir es) e es
{- Return True if the object appears in the room. -}

objectHere :: String  -> Room -> Bool
objectHere o rm = objectInList o (objects rm)

{- Return True if object appears in list of objects-}
objectInList :: String -> [Object] -> Bool
objectInList _ [] = False
objectInList o (ob:obs)
      |o == obj_name ob = True
      |otherwise = objectInList o obs


{- Given an object id and a room description, return a new room description
   without that object -}

removeObject :: String -> Room -> Room
removeObject o rm = rm{objects = filter (\ob -> obj_name ob /= o)  (objects rm)}

{- Given an object and a room description, return a new room description
   with that object added -}

addObject :: Object -> Room -> Room
addObject o rm = rm{objects = objects rm ++ [o]}

{- Given an object id and a list of objects, return the object data. Note
   that you can assume the object is in the list (i.e. that you have
   checked with 'objectHere') -}

findObj :: String -> [Object] -> Object
findObj o (d:ds) = (\ob obs -> if obj_name ob == o then ob else findObj o obs) d ds

{- Use 'findObj' to find an object in a room description -}

objectData :: String -> Room -> Maybe Object
objectData o rm
      |objectHere o rm = Just (findObj o (objects rm))
      |otherwise = Nothing

{- Given a game state and a room id, replace the old room information with
   new data. If the room id does not already exist, add it. -}

updateRoom :: GameData -> String -> Room -> GameData
updateRoom gd rmid rmdata
   |roomExists rmid gameworld = gd{ world = updateGameWorld rmid rmdata (world gd) }
   |otherwise = gd{ world = world gd ++ [(rmid,rmdata)]}

{- Checks that a room of a given name exists within the game world -}
roomExists ::  String -> [(String,Room)]-> Bool
roomExists _ [] = False
roomExists rm (room:rooms) = (\(name,_) ros -> (name == rm) || roomExists rm ros) room rooms

{- updates a room of a given name with updated information, which is stored in a list to be stored in the game world -}
updateGameWorld :: String -> Room -> [(String,Room)] -> [(String,Room)]
updateGameWorld rmid rm gw = filter (\(a,_) -> a /= rmid) gw ++ [(rmid,rm)]


{- Given a game state and an object id, find the object in the current
   room and add it to the player's inventory -}

addInv :: GameData -> String -> GameData
addInv gd obj = do
   let room =  getRoomData gd
   let object = removeMaybe (objectData obj room)
   let newInventoryState = gd{inventory = inventory gd ++ [object]}
   let newRoom = removeObject obj room

   updateRoom newInventoryState (location_id gd) newRoom

{- Given a game state and an object id, remove the object from the
   inventory. Hint: use filter to check if something should still be in
   the inventory. -}

removeInv :: GameData -> String -> GameData
removeInv gd obj = gd{inventory = filter (\o -> obj_name o /= obj ) (inventory gd)}

{- Does the inventory in the game state contain the given object? -}

carrying :: GameData -> String -> Bool
carrying gd obj = objectInList obj (inventory gd)

{-
Define the "go" action. Given a direction and a game state, update the game
state with the new location. If there is no exit that way, report an error.
Remember Actions return a 2-tuple of GameData and String. The String is
a message reported to the player.

e.g.
*Main> go "north" initState
(kitchen,"OK")

-}

go :: Action
go dir state = do
      let newRoom = move dir (getRoomData state)

      if newRoom == Nothing then
         (state,"You can't move that way!")
      else
         (state {location_id =  removeMaybe newRoom } ,removeMaybe newRoom ++ ", OK")

--removes maybe from a variable (only used if not nothing)
removeMaybe :: Maybe a ->  a
removeMaybe (Just a) = a
removeMaybe Nothing = error "No value"

{- Remove an item from the current room, and put it in the player's inventory.
   This should only work if the object is in the current room. Use 'objectHere'
   and 'removeObject' to remove the object, and 'updateRoom' to replace the
   room in the game state with the new room which doesn't contain the object.

   Hints: you will need to take care to update things in the right order here!
    * create a new state with the updated inventory (use 'objectData')
    * create a new room without the object (use 'removeObject')
    * update the game state with this new room in the current location
      (use 'location_id' to find where the player is)
-}

get :: Action
get obj state
   |objectHere obj (getRoomData state) = (addInv state obj,"Picked up "++obj)
   |otherwise = (state,"That object isn't here!")

{- Remove an item from the player's inventory, and put it in the current room.
   Similar to 'get' but in reverse - find the object in the inventory, create
   a new room with the object in, update the game world with the new room.
-}

put :: Action
put obj state
   |carrying state obj = do
      let object = head ( filter (\o -> obj == obj_name o) (inventory state)) --head is safe as it is checked that the object is present
      let newRoom = addObject object (getRoomData state)
      let newInv = removeInv state obj
      (updateRoom newInv (location_id state) newRoom,"Placed " ++ obj ++ " in the " ++ location_id state )
   |otherwise = (state,"Can't put that down")


{- Don't update the state, just return a message giving the full description
   of the object. As long as it's either in the room or the player's 
   inventory! -}

examine :: Action
examine obj state
   |objectHere obj (getRoomData state) = (state, obj_desc (removeMaybe( objectData obj (getRoomData state))))
   |carrying state obj = (state, obj_desc (head (filter (\o -> obj_name o == obj) (inventory state)))) --head is safe as this only happens if there is an element in the list
   |otherwise = (state,"That object isn't here!")


{- Pour the coffee. Obviously, this should only work if the player is carrying
   both the pot and the mug. This should update the status of the "mug"
   object in the player's inventory to be a new object, a "full mug".
-}

pour :: Action
pour obj state
   |carrying state "mug" && carrying state "coffee" = (state{poured = True, inventory =
      map (\o -> if obj_name o == "mug" then fullmug else o) (inventory state) },"Poured a mug of coffee") --updates mug to a full mug
   |otherwise = (state,"You need to be carrying a mug and coffee jug to do that!")

{- Drink the coffee. This should only work if the player has a full coffee 
   mug! Doing this is required to be allowed to open the door. Once it is
   done, also update the 'caffeinated' flag in the game state.

   Also, put the empty coffee mug back in the inventory!
-}

drink :: Action
drink obj state
   |carrying state "mug" && poured state =
      (state{caffeinated = True, inventory = map (\o -> if obj_name o == "mug" then mug else o) (inventory state)},"Drank the coffee.")
   |otherwise = (state, "you need to have a full coffee mug to drink!")

{- Open the door. Only allowed if the player has had coffee! 
   This should change the description of the hall to say that the door is open,
   and add an exit out to the street.

   Use 'updateRoom' once you have made a new description. You can use 
   'openedhall' and 'openedexits' from World.hs for this.
-}

open :: Action
open obj state
   |caffeinated state && location_id state == "hall" && carrying state "key" && carrying state "wallet" && carrying state "mask" && carrying state "matriculation" = do
      let room = getRoomData state
      let updatedHall = room{room_desc = openedhall, exits = openedexits }
      (updateRoom state "hall" updatedHall, "The door has been opened.")
   |otherwise = (state, "You need to have drank coffee and be in the hall to open the door\nYou also need to have a key to open the door, a mask & matriculation card to go to class, and a wallet to buy lunch")


{-Given a file path, returns the GameData stored in that file
(currently does not work)-}
load :: String -> IO GameData
load file = do
         s <- readFile file
         putStrLn s
         return (read s::GameData)

{-Given a GameData and file path, stores the data in the GameData in the given location to be loaded again later-}
save :: GameData -> String -> IO()
save state file = do
   writeFile file (show state)


{- Don't update the game state, just list what the player is carrying -}

inv :: Command
inv state = (state, showInv (inventory state))
   where showInv [] = "You aren't carrying anything"
         showInv xs = "You are carrying:\n" ++ showInv' xs
         showInv' [x] = obj_longname x
         showInv' (x:xs) = obj_longname x ++ "\n" ++ showInv' xs

quit :: Command
quit state = (state { finished = True }, "Bye bye")

