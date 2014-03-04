import Control.Monad
import Data.Map

type Name = String
type Value = Integer
type Phase = Integer
type Addr = (Name, String)
type ModIdAt = (Name, Phase)
type IdAt = (String, Phase)
type Store = Map Addr Value
type Visited = Map ModIdAt (Map String Addr) 
type Resolver = Name -> Module
type State = (ModIdAt, Store, Visited, Resolver)
type Code = State -> State
data Module = Module Name [IdAt] [IdAt] [(Code, Phase)] 

define :: IdAt -> Value -> State -> State
define (id,phase) val ((name, phase'), store, visited, resolver) = 
  ((name, phase'), insert (name, id) val store, visited', resolver)  
  where visited' = 
          case (resolver name) of
            Module name requires provides code ->
              if elem (id, phase) provides then
                insertWith union (name, phase) (fromList [(id, (name, id))]) visited
              else
                visited

resolver name = Module "A" [] [("x", 0)] []

instance Show (a -> b) where
  show x = "FUNC"

main = do
  print $ define ("x",0) 5 (("A", 0), empty, empty, resolver)