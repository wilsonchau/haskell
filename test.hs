initials :: String -> String -> String  
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."  
    where (f:_) = firstname  
          (l:_) = lastname  

initials' :: String -> String -> String  
initials' firstname lastname = [f] ++ ". " ++ [l] ++ "."  
    where f = head firstname  
          l = head lastname  

quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted  

quicksort' :: (Ord a) => [a] -> [a]  
quicksort' [] = []  
quicksort' [x] = [x]  
quicksort' (x:xs) =   
    let smallerSorted = quicksort' [a | a <- xs, a <= x]  
        biggerSorted = quicksort' [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted  
