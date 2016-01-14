module Main (..) where

import Html exposing (..)


type E
    = X
    | One
    | Zero
    | Negate E
    | Sum E E
    | Product E E
    | Exp E


eval : Float -> E -> Float
eval x =
    let
        ev expr =
            case expr of
                X ->
                    x

                One ->
                    1

                Zero ->
                    0

                Negate e ->
                    -(ev e)

                Sum e e' ->
                    ev e + ev e'

                Product e e' ->
                    ev e * ev e'

                Exp e ->
                    Basics.e ^ (ev e)
    in
        ev


diff : E -> E
diff expr =
    case expr of
        X ->
            One

        One ->
            Zero

        Zero ->
            Zero

        Negate e ->
            Negate (diff e)

        Sum e e' ->
            Sum (diff e) (diff e')

        Product e e' ->
            Sum (Product e (diff e')) (Product (diff e) e')

        Exp e ->
            Product (Exp e) (diff e)


diffEval : Float -> E -> ( Float, Float )
diffEval x =
    let
        ev expr =
            case expr of
                X ->
                    ( x, 1 )

                One ->
                    ( 1, 0 )

                Zero ->
                    ( 0, 0 )

                Negate e ->
                    let
                        ( ex, ed ) = ev e
                    in
                        ( -ex, -ed )

                Sum e e' ->
                    let
                        ( ex, ed ) = ev e

                        ( ex', ed' ) = ev e'
                    in
                        ( ex + ex', ed + ed' )

                Product e e' ->
                    let
                        ( ex, ed ) = ev e

                        ( ex', ed' ) = ev e'
                    in
                        ( ex * ex', ex * ed' + ed * ex' )

                Exp e ->
                    let
                        ( ex, ed ) = ev e
                    in
                        ( Basics.e ^ ex, Basics.e ^ ex * ed )
    in
        ev


iterate : number -> (a -> a) -> a -> a
iterate n f =
    if n == 1 then
        f
    else
        f << iterate (n - 1) f


main : Html
main =
    let
        f x = Exp (Sum x (Negate One))

        smallExpression = iterate 3 f X

        exprs =
            [ Sum X One
            , Product X (Sum One (Sum One X))
            , Exp X
            , f X
            , smallExpression
            , iterate 1000 f X
            ]

        item e =
            let
                subitem p = li [] [ toString p |> text ]
            in
                li
                    []
                    [ ul
                        []
                        [ subitem <| e
                        , subitem <| eval 3 e
                        --, subitem <| diff e
                        --, subitem <| eval 3 (diff e)
                        , subitem <| diffEval 1.00001 e
                        ]
                    ]
    in
        ul [] <| List.map item exprs
