module Order 
    exposing 
        ( Order
        , empty
        , fromList
        , toList
        , remove
        , getOrderOf
        , addFirst
        , addBefore
        , addAfter
        , isBefore
        , isAfter
        )


{-| A type to arrange things an order

# Order

@docs Order

# Basics

@docs empty, isEmpty, fromList, toList, length, reverse, member

# Modifying Orders

@docs remove, addFirst, addBefore, addAfter, isBefore, isAfter


isEmpty, length, reverse,

# Predicate

@docs allPass, anyPass

-}

{-|An arrangement of things in an order. An Order is kind of like a list, except there cant be duplicate elements. And, its kind of like a Set, except an Orders elements go from first to last.
-}
type Order a 
    = Order_ (List a)

{-| Create an empty order

    Order.empty == Order.fromList []

-}
empty : Order a
empty =
    Order_ []


{-| Check if an Order is empty
    
    Order.isEmpty Order.empty == True
-}
isEmpty : Order a -> Bool
isEmpty order =
    case order of
        Order_ list ->
            List.isEmpty list


{-| Get the length of an Order

    nobleGases = Order.fromList
        [ helium
        , neon
        , argon
        , krypton
        , xenon
        , radon
        ]

    Order.length nobleGases == 6
-}
length : Order a -> Int
length order =
    case order of
        Order_ list ->
            List.length list


{-| Reverse an order

    bestCoffeeDrinks ==
        Order.fromList
            [ cortado
            , latte
            , coldbrew
            , coffee
            , mocha
            ]

    worstCoffeeDrinks : Order Coffee
    worstCoffeeDrinks =
        Order.reverse bestCoffeeDrinks

-}
reverse : Order a -> Order a
reverse order =
    case order of
        Order_ list ->
            Order_ (List.reverse list)


{-| Check if an Order contains a member

    Order.member "Grover Cleveland" usPresidents == True

-}
member : a -> Order a -> Bool
member el order =
    case order of
        Order_ list ->
            List.member el list


{-| Create an `Order` from a `List` -}
fromList : List a -> Order a
fromList list =
    let
        check : a -> List a -> List a
        check el newList =
            if List.member el newList then
                newList
            else
                el :: newList            
    in        
        Order_ (List.foldr check [] list)

            
{-| Turn an `Order` into a `List`

[ 'u' ] ==  Order.toList <| Order.fromList [ 'u', 'u' ]

-}
toList : Order a -> List a
toList order =
    case order of
        Order_ list ->
            list


{-| Remove an element from an Order -}
remove : a -> Order a -> Order a
remove element order =
    case order of
        Order_ list ->
            Order_ (filterFor element list)


{-| Add an element to an order, making it the first one -}
addFirst : a -> Order a -> Order a
addFirst element order =
    case order of
        Order_ list ->
            if List.member element list then
                Order_ (element :: (filterFor element list))
            else
                Order_ (element :: list)


{-| Add an element to an order before another element 

    addBefore `b` `c` (Order.fromList [ `a`, `c`, `d` ])
        == Order.fromList [ `a`, `b`, `c`, `d` ]

-}
addBefore : a -> a -> Order a -> Order a
addBefore el newEl order =
    case order of
        Order_ list ->
            if newEl /= el then
                let
                    check : a -> List a -> List a
                    check thisEl newList =
                        if thisEl == el then
                            newEl :: thisEl :: newList
                        else
                            thisEl :: newList
                        
                in
                    list
                        |> List.foldr check []
                        |> fromList

            else
                order


{-| Add an element to an order before another element 

    addAfter `c` `b` (Order.fromList [ `a`, `b`, `d` ])
        == Order.fromList [ `a`, `b`, `c`, `d` ]

-}
addAfter : a -> a -> Order a -> Order a
addAfter el newEl order =
    case order of
        Order_ list ->
            if newEl /= el then
                let
                    check : a -> List a -> List a
                    check thisEl newList =
                        if thisEl == el then
                            el :: newEl :: newList
                        else
                            thisEl :: newList
                        
                in
                    list
                        |> List.foldr check []
                        |> fromList
            else
                order



{-| Check if an element is before another in order, if it is in the order at all.

    germanStates = Order.fromList [ "Bavaria", "Brandenberg" ]

    "Bavaria" |> Order.isBefore "Brandenberg" germanStates == Just True

    "Bavaria" |> Order.isBefore "New York City" germanStates == Nothing
-}
isBefore : a -> a -> Order a -> Maybe Bool
isBefore after first order =
    case order of
        Order_ list ->
            case (getOrderHelper after list, getOrderHelper first list) of
                (Just afterIndex, Just firstIndex) ->
                    if firstIndex < afterIndex then
                        Just True
                    else
                        Just False

                
                _ ->
                    Nothing

{-| Check if an element is after another in order, if it is in the order at all.-}
isAfter : a -> a -> Order a -> Maybe Bool
isAfter first after order =
    isBefore after first order


{-| If the element is in the order, return what place it is in-}
getOrderOf : a -> Order a -> Maybe Int
getOrderOf el order =
    case order of
        Order_ list ->
            getOrderHelper el list






-- HELPERS --


getOrderHelper : a -> List a -> Maybe Int
getOrderHelper el list =
    getOrderRecusrive el (List.indexedMap (flip (,)) list, Nothing)
        |> Tuple.second
            



getOrderRecusrive : a -> (List (a, Int), Maybe Int) -> (List (a, Int), Maybe Int)
getOrderRecusrive el (list, maybeIndex) =
    case list of
        x :: xs ->
            let
                (xEl, index) = x                    
            in                
                if xEl == el then
                    ([], Just index)
                else
                    getOrderRecusrive el (xs, maybeIndex )

        [] ->
            ([], Nothing)





filterFor : a -> List a -> List a
filterFor element list =
    List.filter ((/=) element) list




