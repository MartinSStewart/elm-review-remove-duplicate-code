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
"""
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
"""
                ]
                    |> Review.Test.runOnModules (RemoveDuplicateCode.rule { ignore = [], threshold = 200 })
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

        """
                , """module B exposing (..)

errorMessage : Bool -> Result String b -> Element msg
errorMessage submitAttempted result =
    case ( submitAttempted, result ) of
        ( True, Err error ) ->
            DesignSystem.Input.errorMessage error

        _ ->
            Element.none

        """
                ]
                    |> Review.Test.runOnModules (RemoveDuplicateCode.rule { ignore = [], threshold = 200 })
                    |> Review.Test.expectErrorsForModules
                        [ ( "B"
                          , [ Review.Test.error
                                { message = """Found code that is repeated too often (2 times) and can instead be combined into a single function.

Here are all the places it's used:

B 3:1 to 10:25
A 3:1 to 10:25"""
                                , details = [ "It's okay to duplicate short snippets several times or duplicate larger chunks 2-3 times. But here it looks like this code is repeated too often and it would be better to have a single function for it." ]
                                , under = """errorMessage : Bool -> Result String b -> Element msg
errorMessage submitAttempted result =
    case ( submitAttempted, result ) of
        ( True, Err error ) ->
            DesignSystem.Input.errorMessage error

        _ ->
            Element.none"""
                                }
                            ]
                          )
                        ]
        ]
