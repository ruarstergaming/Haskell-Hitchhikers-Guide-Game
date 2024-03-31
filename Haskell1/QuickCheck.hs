{-# LANGUAGE TemplateHaskell #-}
module QuickCheck where
import Test.QuickCheck
import Test.QuickCheck.All
import Actions
import World


instance Arbitrary Directions where
    arbitrary = oneof [return North,
                       return South,
                       return East,
                       return West]

instance Arbitrary Object where
    arbitrary = oneof [return mug,
                       return fullmug,
                       return coffeepot,
                       return key,
                       return mask,
                       return wallet,
                       return matric,
                       return mazemap]

instance Arbitrary Room where
    arbitrary = oneof [return bedroom,
                       return kitchen,
                       return hall,
                       return street,
                       return garden,
                       return pantry]

instance Arbitrary GameData where
    arbitrary = oneof [return initState]



--prop_remove :: [Object] -> Bool 
--prop_remove xs = findObj xs 


-- Check addObject return a room containing the object, and that objectHere returns true if the object is there
prop_addObject :: Object -> Room -> Bool
prop_addObject o rm = objectHere (obj_name o) (addObject o rm) 



-- Check removeObject return a room without the given object (adds object to room to ensure it's actually in the room)
prop_removeObject :: Object -> Room -> Bool
prop_removeObject o rm = not (objectHere (obj_name o) (removeObject (obj_name o) (addObject o rm)))


return []
runTests = $quickCheckAll