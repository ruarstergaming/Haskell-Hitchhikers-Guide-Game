To you can play the game by entering this into the command line:

### To build:
	cabal build


### To run:
	./adventure

You can also alternatively put this into the command line:

### To build:
	ghci Adventure.hs

### To run (this is in ghci):
	main

### There are several ways to interact with the game by typing the format: ACTION SPECIFIC

go - takes you places
get - adds the specified item to your inventory
drop - drops the specficied item from your inventory into the room you're in
pour - pours the liquid into a mug if you have one
examine - gives a description of an item in your inventory
open - opens whatever you are trying to open if you have a key
inventory - lists all the items in your inventory
quit - quits the program

### Here are the names of the items of the game:

Coffee Mug - "mug"
Coffee Pot - "coffee"
Key - "key"
Face Mask - "mask"
Wallet - "wallet"
Matriculation Card - "matriculation"
Maze Map - "map"

### To beat the game enter these commands:

get wallet
go north
get coffee
go east
get mug
pour coffee
drink coffee
go west
drop coffee
go west
go south
get map
exmaine map
go north
go west
get mask
get matriculation
go south
go north
go north
go east
go south
go west
go north
go south
get key
go north
go east
open door
go out