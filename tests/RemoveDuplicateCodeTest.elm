module RemoveDuplicateCodeTest exposing (all)

import RemoveDuplicateCode
import Review.Test
import Test exposing (Test, describe, test)


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
                    |> Review.Test.runOnModules RemoveDuplicateCode.rule
                    |> Review.Test.expectNoErrors
        ]
