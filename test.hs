data Foo a = Foo1
         | Foo2 a

data Bar = Bar1
         | Bar2 (Foo Int)


blah :: Bar -> Int
blah Bar1 = 1
blah (Bar2 x@(a)) = 2
     
