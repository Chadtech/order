module Order
    exposing
        ( Order
        , empty
        , isEmpty
        , fromList
        , toList
        , remove
        , reverse
        , member
        , length
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

@docs empty, isEmpty, fromList, toList, length, reverse, member, getOrderOf

# Modifying Orders

@docs remove, addFirst, addBefore, addAfter, isBefore, isAfter


-}


{-| An arrangement of things in an order. An Order is kind of like a list, except there cant be duplicate elements. And, its kind of like a Set, except an Orders elements go from first to last.
-}
type Order a
    = Order (List a)


{-| Create an empty order

    Order.empty == Order.fromList []

-}
empty : Order a
empty =
    Order []


{-| Check if an Order is empty

    Order.isEmpty Order.empty == True
-}
isEmpty : Order a -> Bool
isEmpty (Order list) =
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
length (Order list) =
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
reverse (Order list) =
    Order (List.reverse list)


{-| Check if an Order contains a member

    Order.member "Grover Cleveland" usPresidents == True

-}
member : a -> Order a -> Bool
member el (Order list) =
    List.member el list


{-| Create an `Order` from a `List`
-}
fromList : List a -> Order a
fromList list =
    Order (List.foldr consIfNotMember [] list)


{-| Turn an `Order` into a `List`

    [ 'u' ] == Order.toList <| Order.fromList [ 'u', 'u' ]

-}
toList : Order a -> List a
toList (Order list) =
    list


{-| Remove an element from an Order
-}
remove : a -> Order a -> Order a
remove element (Order list) =
    Order (filterFor element list)


{-| Add an element to an order, making it the first one
-}
addFirst : a -> Order a -> Order a
addFirst element (Order list) =
    Order (element :: (filterFor element list))



{-| Add an element to an order before another element

    addBefore `c` `b` (Order.fromList [ `a`, `c`, `d` ])
        == Order.fromList [ `a`, `b`, `c`, `d` ]

-}
addBefore : a -> a -> Order a -> Order a
addBefore el newEl (Order list) =
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
        Order list


{-| Add an element to an order before another element

    addAfter `b` `c` (Order.fromList [ `a`, `b`, `d` ])
        == Order.fromList [ `a`, `b`, `c`, `d` ]

-}
addAfter : a -> a -> Order a -> Order a
addAfter el newEl (Order list) =
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
        Order list


{-| Check if an element is before another in order, if it is in the order at all.

    germanStates = Order.fromList [ "Bavaria", "Brandenberg" ]

    ("Bavaria" |> Order.isBefore "Brandenberg") germanStates == Just True

    ("Bavaria" |> Order.isBefore "New York City") germanStates == Nothing
-}
isBefore : a -> a -> Order a -> Maybe Bool
isBefore after first (Order list) =
    case ( getOrderHelper after list, getOrderHelper first list ) of
        ( Just afterIndex, Just firstIndex ) ->
            if firstIndex < afterIndex then
                Just True
            else
                Just False

        _ ->
            Nothing


{-| Check if an element is after another in order, if it is in the order at all.
-}
isAfter : a -> a -> Order a -> Maybe Bool
isAfter first after order =
    isBefore after first order


{-| If the element is in the order, return what place it is in
-}
getOrderOf : a -> Order a -> Maybe Int
getOrderOf el (Order list) =
    getOrderHelper el list



-- HELPERS --


consIfNotMember : a -> List a -> List a
consIfNotMember el list =
    if List.member el list then
        list
    else
        el :: list


getOrderHelper : a -> List a -> Maybe Int
getOrderHelper el list =
    getOrderRecursive el ( List.indexedMap (flip (,)) list, Nothing )
        |> Tuple.second


getOrderRecursive : a -> ( List ( a, Int ), Maybe Int ) -> ( List ( a, Int ), Maybe Int )
getOrderRecursive el ( list, maybeIndex ) =
    case list of
        x :: xs ->
            let
                ( xEl, index ) =
                    x
            in
                if xEl == el then
                    ( [], Just index )
                else
                    getOrderRecursive el ( xs, maybeIndex )

        [] ->
            ( [], Nothing )


filterFor : a -> List a -> List a
filterFor element list =
    List.filter ((/=) element) list
