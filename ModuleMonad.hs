import Control.Monad
import Data.Map

type Identifier = (String, String)
type Namespace = (Map Identifier Integer)
  
data Module a =
  Module (Namespace -> Either String (a, Namespace))

instance Monad Module where
  (Module m) >>= f = Module (\namespace -> 
                              case m namespace of
                                Left msg -> Left msg
                                Right (val, namespace') ->
                                  let (Module m') = f val in
                                    m' namespace')
  return x = Module (\namespace -> Right (x, namespace))
        
run (Module m) = m empty
  
  
define id val = Module (\namespace -> Right ((), insert id val namespace))
set = define
require mod = Module (\namespace -> 
                       case run mod of
                         Left msg -> Left msg
                         Right (val, namespace') ->
                           Right ((), union namespace' namespace))
ref id = Module (\namespace -> 
                  case Data.Map.lookup id namespace of 
                    Just x -> Right (x, namespace)
                    Nothing -> Left ("Unbound variable " ++ id))

a = do
  define "x" 3
  return ()
b = do
  require a
  set "x" 5
  define "y" 4
  return ()

c = do
  require a
  x <- ref "x"
  define "z" x
  return ()
  
d = do
  require b
  require c
  z <- ref "z"
  return z
  
  
main = do
  print $ run d