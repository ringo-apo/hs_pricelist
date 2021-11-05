module Main where

func3 pl unum uname uprice = do

    if pl == []
    then []
    else do
        let (data1, data2, data3) = head pl 

        if data1 /= unum
        then [head pl] ++ (func3 (tail pl) unum uname uprice)
        else [(unum, uname, uprice)] ++ (func3 (tail pl) unum uname uprice) 

func2 x y = do

    if x == []
    then []
    else do
        let (data1, data2, data3) = head x

        if data1 /= y
        then [head x] ++ (func2 (tail x) y)
        else (func2 (tail x) y)

func1 x = do

    print "Menu: P:ListPrint I:Insert D:Delete U:Update Other:End"
    menu <- getLine

    case menu of
        "P" -> do
            print x 
            func1 x

        "I" -> do
            let (b, c, d) = unzip3 x
            let e = (maximum b) + 1 
            print e 

            print "Input Name"
            name <- getLine

            print "Input price"
            priceS <- getLine
            let price = read priceS :: Int

            let x2 = x ++ [(e, name, price)]
            func1 x2

        "D" -> do
            print "Input Delete number"
            dnumS <- getLine
            let dnum = read dnumS :: Int

            let x2 = func2 x dnum
            func1 x2

        "U" -> do
            print "Input Updata number"
            unumS <- getLine
            let unum = read unumS :: Int

            print "Updata name"
            uname <- getLine

            print "Updata price"
            upriceS <- getLine
            let uprice = read upriceS :: Int

            let x2 = func3 x unum uname uprice
            func1 x2

        _ -> do
            print "end"   

main :: IO ()
main = do

    let pricelist = [(1, "Apple", 100)]
    print pricelist

    func1 pricelist

