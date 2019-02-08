foo :: (b -> c) -> (a -> b) -> (a -> c)
foo f g = f . g
          
