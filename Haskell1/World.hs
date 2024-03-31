module World where

data Directions = North | South | East | West | Out 
  deriving Show

data Objects   = Mug | FullMug | CoffeePot | Key | Mask | Wallet | Matric | MazeMap
   deriving Show
 
data Rooms = Bedroom | Kitchen | Hall | Street | Garden | Maze0 | Maze1 | Maze2 | Maze3 | Maze4 | Maze5 | Maze6 | Mazec | Pantry | LivingRoom
   deriving Show

data Commands = Go Directions | Get Objects | Drop Objects | Examine Objects 

data Object = Obj { obj_name :: String,
                    obj_longname :: String,
                    obj_desc :: String }
   deriving (Eq,Read)

instance Show Object where
   show obj = obj_longname obj

data Exit = Exit { exit_dir :: String,
                   exit_desc :: String,
                   room :: String }
   deriving (Eq,Read)

data Room = Room { room_desc :: String,
                   exits :: [Exit],
                   objects :: [Object] }
   deriving (Eq,Read)

data GameData = GameData { location_id :: String, -- where player is
                           world :: [(String, Room)],
                           inventory :: [Object], -- objects player has
                           poured :: Bool, -- coffee is poured
                           caffeinated :: Bool, -- coffee is drunk
                           finished :: Bool -- set to True at the end
                         } deriving (Show,Read)

won :: GameData -> Bool
won gd = location_id gd == "street"

instance Show Room where
    show (Room desc exits objs) = desc ++ "\n" ++ concatMap exit_desc exits ++
                                  showInv objs
       where showInv [] = ""
             showInv xs = "\n\nYou can see: " ++ showInv' xs
             showInv' [x] = show x
             showInv' (x:xs) = show x ++ ", " ++ showInv' xs
                                  

--instance Show GameData where
--    show gd = show (getRoomData gd)

-- Things which do something to an object and update the game state
type Action  = String -> GameData -> (GameData, String)

-- Things which just update the game state
type Command = GameData -> (GameData, String)

mug, fullmug, coffeepot, key, mask, wallet, matric :: Object
mug       = Obj "mug" "a coffee mug" "A coffee mug"
fullmug   = Obj "mug" "a full coffee mug" "A coffee mug containing freshly brewed coffee"
coffeepot = Obj "coffee" "a pot of coffee" "A pot containing freshly brewed coffee"
key       = Obj "key" "A metal key" "A key to the front door, why was it in the garden?"
mask      = Obj "mask" "A cloth mask" "A mask to prevent diseases"
wallet    = Obj "wallet" "A leather wallet" "A wallet with money in it"
matric    = Obj "matriculation" "A plastic card" "Matriculation card to get into buildings"
mazemap   = Obj "map" "A small worn piece of paper" "A map of the backrooms. A world if you enter you may not leave. A key is drawn at the center of it. To get to the center go north, north, east, south, west, north, south"

bedroom, kitchen, hall, street, living, garden :: Room

bedroom = Room "You are in your bedroom."
               [Exit "north" "To the north is a kitchen. " "kitchen"]
               [wallet]

kitchen = Room "You are in the kitchen."
               [Exit "south" "To the south is your bedroom. " "bedroom",
                Exit "west" "To the west is a hallway. " "hall",
                Exit "east" "To the east is a pantry. " "pantry"]
               [coffeepot]

pantry = Room "You are in the pantry."
               [Exit "west" "To the west is a kitchen. " "kitchen"]
               [mug]

hall = Room "You are in the hallway. The front door is closed. "
            [Exit "east" "To the east is a kitchen. " "kitchen",
             Exit "west" "To the west is a living room. " "living",
             Exit "south" "To the south is a garden. " "garden"]
            []
living = Room "You are in the living room." 
         [Exit "east" "To the east is a hallway. " "hall",
          Exit "south" "To the south is ???" "maze0"]
         [mask,matric]

garden = Room "You are in the garden."
         [Exit "north" "To the north is a hallway. " "hall"]
         [mazemap]

-- Maze rooms all lead back to each original if strayed from the path.

maze0 = Room "You have entered the beginning of the backrooms, you have noclipped out of reality, good luck :)"
         [Exit "south" "To the south is the living room, you could clip back to reality if you're quick. " "living",
          Exit "north" "To the north is a hallway. " "maze1",
          Exit "east" "To the east is a hallway with a dark handprint on it. " "maze0"]
         []        

maze1 = Room "You are in a yellow hallway"
         [Exit "north" "To the north is a yellow hallway. " "maze2",
          Exit "east" "To the east is a blue hallway. " "maze0",
          Exit "south" "To the south there is no longer a hallway. " "maze0"]
         [] 

maze2 = Room "You are in a yellow hallway"
         [Exit "north" "To the north is a green hallway. " "maze0",
          Exit "east" "To the east is a hallway of mirrors. " "maze3",
          Exit "south" "To the south there is maybe a hallway, if you're lucky. " "maze0"]
         [] 

maze3 = Room "You are in a hallway?"
         [Exit "north" "To the north is a hallway. " "maze0",
          Exit "south" "To the south is a @^1!? hallway." "maze4"]
         [] 

maze4 = Room "a hallway are You in"
         [Exit "north" "To the north is a yellow hallway. " "maze0",
          Exit "east" "To the east is a hallway. " "maze0",
          Exit "west" "To the west there is a hallway FINALLY. " "maze5"]
         [] 

maze5 = Room "You are in the best hallway"
         [Exit "north" "To the north is a spiral hallway. " "maze6",
          Exit "east" "To the east is a ladder. " "maze0",
          Exit "south" "To the south there is maybe a hallway? " "maze0"]
         [] 

maze6 = Room "You are in a yellow hallway"
         [Exit "north" "To the north is a hallway with a shape at the end of it. " "maze0",
          Exit "east" "To the east is a hallway. " "maze0",
          Exit "south" "To the south there is the way you came. It is n o r m a l. " "mazec"]
         [] 

mazec = Room "You have reached the center of the backrooms, congratulations, now leave."
         [Exit "north" "To the north is a doorway to seemingly nowhere. " "living"]
         [key] 

-- New data about the hall for when we open the door

openedhall = "You are in the hallway. The front door is open. "
openedexits = [Exit "east" "To the east is a kitchen. " "kitchen",
               Exit "out" "You can go outside. " "street"]

street = Room "You have made it out of the house."
              [Exit "in" "You can go back inside if you like. " "hall"]
              []

gameworld = [("bedroom", bedroom),
             ("kitchen", kitchen),
             ("hall", hall),
             ("street", street),
             ("living", living),
             ("garden",garden),
             ("pantry",pantry),
             ("maze0",maze0),
             ("maze1",maze1),
             ("maze2",maze2),
             ("maze3",maze3),
             ("maze4",maze4),
             ("maze5",maze5),
             ("maze6",maze6),
             ("mazec",mazec)]

initState :: GameData
initState = GameData "bedroom" gameworld [] False False False

{- Return the room the player is currently in. -}

getRoomData :: GameData -> Room
getRoomData gd = maybe undefined id (lookup (location_id gd) (world gd))
