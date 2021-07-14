module RemoveDuplicateCodeTest exposing (all)

import RemoveDuplicateCode
import Review.Test
import Test exposing (Test, describe, only, test)


all : Test
all =
    describe "RemoveDuplicateCode"
        [ test "distinguish local types" <|
            \_ ->
                [ """module A exposing (..)
                
type MyType = A | B | C | D | E | F | G | H


myType value = 
    case value of 
        A -> 
            0
        
        B -> 
            1

        C -> 
            0
        
        D -> 
            1
            
        E -> 
            0
        
        F -> 
            1
            
        G -> 
            0
        
        H -> 
            1
""" |> String.replace "\u{000D}" ""
                , """module B exposing (..)
                      
type MyType = A | B | C | D | E | F | G | H


myType value = 
    case value of 
        A -> 
            0
        
        B -> 
            1
        
        C -> 
            0
        
        D -> 
            1
          
        E -> 
            0
        
        F -> 
            1
          
        G -> 
            0
        
        H -> 
            1
""" |> String.replace "\u{000D}" ""
                , """module C exposing (..)
                      
type MyType = A | B | C | D | E | F | G | H


myType value = 
    case value of 
        A -> 
            0
        
        B -> 
            1
        
        C -> 
            0
        
        D -> 
            1
          
        E -> 
            0
        
        F -> 
            1
          
        G -> 
            0
        
        H -> 
            1
    """ |> String.replace "\u{000D}" ""
                ]
                    |> Review.Test.runOnModules (RemoveDuplicateCode.rule { ignore = [], threshold = 1 })
                    |> Review.Test.expectNoErrors
        , test "distinguish local types 2" <|
            \_ ->
                [ """module A exposing (..)

errorMessage : Bool -> Result String b -> Element msg
errorMessage submitAttempted result =
    case ( submitAttempted, result ) of
        ( True, Err error ) ->
            DesignSystem.Input.errorMessage error

        _ ->
            Element.none

""" |> String.replace "\u{000D}" ""
                , """module B exposing (..)

errorMessage : Bool -> Result String b -> Element msg
errorMessage submitAttempted result =
    case ( submitAttempted, result ) of
        ( True, Err error ) ->
            DesignSystem.Input.errorMessage error

        _ ->
            Element.none

""" |> String.replace "\u{000D}" ""
                , """module C exposing (..)

errorMessage : Bool -> Result String b -> Element msg
errorMessage submitAttempted result =
    case ( submitAttempted, result ) of
        ( True, Err error ) ->
            DesignSystem.Input.errorMessage error

        _ ->
            Element.none

""" |> String.replace "\u{000D}" ""
                ]
                    |> Review.Test.runOnModules (RemoveDuplicateCode.rule { ignore = [], threshold = 1 })
                    |> Review.Test.expectErrorsForModules
                        [ ( "C"
                          , [ Review.Test.error
                                { message = """Found code that is repeated too often (3 times) and can instead be combined into a single function.

Here are all the places it's used:

C 3:1 to 10:25
B 3:1 to 10:25
A 3:1 to 10:25""" |> String.replace "\u{000D}" ""
                                , details =
                                    [ "It's okay to duplicate short snippets several times or duplicate larger chunks 2-3 times. But here it looks like this code is repeated too often and it would be better to have a single function for it."
                                    , "Debug info: This error has 25 complexity."
                                    ]
                                , under = """errorMessage : Bool -> Result String b -> Element msg
errorMessage submitAttempted result =
    case ( submitAttempted, result ) of
        ( True, Err error ) ->
            DesignSystem.Input.errorMessage error

        _ ->
            Element.none""" |> String.replace "\u{000D}" ""
                                }
                            ]
                          )
                        ]
        , only <|
            test "Complexity test" <|
                \_ ->
                    """module A exposing (..)

a = a.field <= 5

b = a.field <= 5

c = a.field <= 5

d = a.field <= 5
"""
                        |> String.replace "\u{000D}" ""
                        |> Review.Test.run (RemoveDuplicateCode.rule { ignore = [], threshold = 1 })
                        |> Review.Test.expectErrors
                            [ Review.Test.error
                                { message = """Found code that is repeated too often (4 times) and can instead be combined into a single function.

Here are all the places it's used:

A 9:5 to 9:17
A 7:5 to 7:17
A 5:5 to 5:17
A 3:5 to 3:17""" |> String.replace "\u{000D}" ""
                                , details =
                                    [ "It's okay to duplicate short snippets several times or duplicate larger chunks 2-3 times. But here it looks like this code is repeated too often and it would be better to have a single function for it."
                                    , "Debug info: This error has 8 complexity."
                                    ]
                                , under = "a.field <= 5"
                                }
                                |> Review.Test.atExactly { start = { row = 9, column = 5 }, end = { row = 9, column = 17 } }
                            ]
        ]
