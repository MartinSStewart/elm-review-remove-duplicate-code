module RemoveDuplicateCode exposing (rule)

{-|

@docs rule

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Expression(..), Function, LetDeclaration(..))
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.Syntax.Range exposing (Range)
import List.Nonempty exposing (Nonempty)
import Review.ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Error, ModuleRuleSchema, Rule)


{-| Reports... REPLACEME

    config =
        [ RemoveDuplicateCode.rule
        ]


## Fail

    a =
        "REPLACEME example to replace"


## Success

    a =
        "REPLACEME example to replace"


## When (not) to enable this rule

This rule is useful when REPLACEME.
This rule is not useful when REPLACEME.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template MartinSStewart/elm-review-remove-duplicate-code/example --rules RemoveDuplicateCode
```

-}
rule : Rule
rule =
    Rule.newProjectRuleSchema "RemoveDuplicateCode" { moduleKeys = Dict.empty }
        |> Rule.withModuleVisitor moduleVisitor
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule =
                Rule.initContextCreator fromProjectToModule
                    |> Rule.withModuleNameLookupTable
            , fromModuleToProject =
                Rule.initContextCreator fromModuleToProject
                    |> Rule.withModuleKey
                    |> Rule.withMetadata
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.fromProjectRuleSchema


moduleVisitor : ModuleRuleSchema {} ModuleContext -> ModuleRuleSchema { moduleSchemaState | hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor =
    Rule.withDeclarationListVisitor declarationVisitor


declarationVisitor : List (Node Declaration) -> ModuleContext -> ( List (Error {}), ModuleContext )
declarationVisitor declarations context =
    ( []
    , { context
        | hashedExpressions =
            List.foldl
                (hashDeclaration context.lookupTable)
                context.hashedExpressions
                declarations
      }
    )


hashDeclaration : ModuleNameLookupTable -> Node Declaration -> Dict String (Nonempty HashData) -> Dict String (Nonempty HashData)
hashDeclaration lookupTable (Node range declaration) hashDict =
    case declaration of
        FunctionDeclaration function ->
            hashFunction lookupTable range function hashDict |> .hashDict

        _ ->
            hashDict


hashFunction : ModuleNameLookupTable -> Range -> Function -> Dict String (Nonempty HashData) -> { hash : String, hashDict : Dict String (Nonempty HashData) }
hashFunction lookupTable range function hashDict =
    let
        implementation =
            Node.value function.declaration

        result =
            hashExpression lookupTable implementation.expression 1 hashDict

        newHash =
            hashText (Node.value implementation.name) result.hash
    in
    { hash = newHash
    , hashDict =
        insertHash
            newHash
            { depth = 0, range = range }
            result.hashDict
    }


hashText : String -> String -> String
hashText text hash =
    Debug.todo ""


escapeChar =
    "‱"


hashExpression :
    ModuleNameLookupTable
    -> Node Expression
    -> Int
    -> Dict String (Nonempty HashData)
    -> { hash : String, hashDict : Dict String (Nonempty HashData) }
hashExpression lookupTable (Node range expression) depth hashDict =
    let
        hashStuff : String -> List (Node Expression) -> { hash : String, hashDict : Dict String (Nonempty HashData) }
        hashStuff hash nodes =
            let
                finalResult =
                    List.foldl
                        (\node state ->
                            let
                                result =
                                    hashExpression lookupTable node (depth + 1) state.hashDict
                            in
                            { hash = hashText state.hash result.hash
                            , hashDict = result.hashDict
                            }
                        )
                        { hash = escapeChar ++ hash, hashDict = hashDict }
                        nodes
            in
            { hash = finalResult.hash
            , hashDict = insertHash finalResult.hash { depth = depth, range = range } finalResult.hashDict
            }
    in
    case expression of
        UnitExpr ->
            { hash = "0" ++ escapeChar, hashDict = hashDict }

        Application nodes ->
            hashStuff ("1" ++ escapeChar) nodes

        OperatorApplication string _ left right ->
            hashStuff ("2" ++ escapeChar ++ string) [ left, right ]

        FunctionOrValue moduleName name ->
            let
                fullName =
                    Review.ModuleNameLookupTable.moduleNameAt lookupTable range
                        |> Maybe.withDefault moduleName
                        |> (\a -> a ++ [ name ])
                        |> String.join "."
            in
            { hash = "3" ++ escapeChar ++ fullName
            , hashDict = hashDict
            }

        IfBlock condition ifTrue ifFalse ->
            hashStuff ("4" ++ escapeChar) [ condition, ifTrue, ifFalse ]

        PrefixOperator string ->
            { hash = "5" ++ escapeChar ++ string, hashDict = hashDict }

        Operator string ->
            { hash = "6" ++ escapeChar ++ string, hashDict = hashDict }

        Integer int ->
            { hash = "7" ++ escapeChar ++ String.fromInt int, hashDict = hashDict }

        Hex int ->
            { hash = "8" ++ escapeChar ++ String.fromInt int, hashDict = hashDict }

        Floatable float ->
            { hash = "9" ++ escapeChar ++ String.fromFloat float, hashDict = hashDict }

        Negation node ->
            hashStuff ("10" ++ escapeChar) [ node ]

        Literal string ->
            { hash = "11" ++ escapeChar ++ string, hashDict = hashDict }

        CharLiteral char ->
            { hash = "12" ++ escapeChar ++ String.fromChar char, hashDict = hashDict }

        TupledExpression nodes ->
            hashStuff ("13" ++ escapeChar) nodes

        ParenthesizedExpression node ->
            hashExpression lookupTable node (depth + 1) hashDict

        LetExpression letBlock ->
            hashStuff
                ("14" ++ escapeChar)
                (letBlock.expression
                    :: List.foldl
                        (\(Node range_ letDeclaration) -> letDeclaration lookupTable range_)
                        hashDict
                        letBlock.declarations
                )

        CaseExpression caseBlock ->
            Debug.todo ""

        LambdaExpression lambda ->
            Debug.todo ""

        RecordExpr nodes ->
            Debug.todo ""

        ListExpr nodes ->
            hashStuff ("18" ++ escapeChar) nodes

        RecordAccess value (Node _ accessor) ->
            hashStuff ("19" ++ escapeChar ++ accessor) [ value ]

        RecordAccessFunction string ->
            { hash = "20" ++ escapeChar ++ string, hashDict = hashDict }

        RecordUpdateExpression (Node _ record) nodes ->
            Debug.todo ""

        GLSLExpression string ->
            { hash = "22" ++ escapeChar ++ string, hashDict = hashDict }


hashLetDeclaration :
    ModuleNameLookupTable
    -> Node LetDeclaration
    -> Int
    -> Dict String (Nonempty HashData)
    -> { hash : String, hashDict : Dict String (Nonempty HashData) }
hashLetDeclaration lookupTable (Node range letDeclaration) depth hashDict =
    case letDeclaration of
        LetFunction letFunction ->
            hashFunction lookupTable range letFunction hashDict

        LetDestructuring pattern expression ->
            let
                result =
                    hashExpression lookupTable expression (depth + 1) hashDict

                newHash =
                    hashText (hashPattern lookupTable pattern) result.hash
            in
            { hash = newHash, hashDict = insertHash newHash { depth = depth, range = range } result.hashDict }


hashPattern : ModuleNameLookupTable -> Node Pattern -> String
hashPattern lookupTable (Node range pattern) =
    let
        hashHelper id nodes =
            List.foldl (\node hash -> hashText hash (hashPattern lookupTable node)) (escapeChar ++ id ++ escapeChar) nodes
    in
    case pattern of
        UnitPattern ->
            "23" ++ escapeChar

        AllPattern ->
            "24" ++ escapeChar

        CharPattern char ->
            "25" ++ escapeChar ++ String.fromChar char

        StringPattern string ->
            "26" ++ escapeChar ++ string

        IntPattern int ->
            "27" ++ escapeChar ++ String.fromInt int

        HexPattern int ->
            "28" ++ escapeChar ++ String.fromInt int

        FloatPattern float ->
            "29" ++ escapeChar ++ String.fromFloat float

        TuplePattern nodes ->
            hashHelper "30" nodes

        RecordPattern fields ->
            "31" ++ escapeChar ++ (List.map Node.value fields |> List.sort |> String.join " ")

        UnConsPattern a b ->
            hashHelper "32" [ a, b ]

        ListPattern nodes ->
            hashHelper "33" nodes

        VarPattern var ->
            "34" ++ escapeChar ++ var

        NamedPattern qualifiedNameRef nodes ->
            let
                name =
                    Review.ModuleNameLookupTable.moduleNameAt lookupTable range
                        |> Maybe.withDefault qualifiedNameRef.moduleName
                        |> (\a -> a ++ [ qualifiedNameRef.name ])
                        |> String.join "."
            in
            hashHelper ("35" ++ escapeChar ++ name) nodes

        AsPattern node (Node _ text) ->
            hashHelper ("36" ++ escapeChar ++ text) [ node ]

        ParenthesizedPattern node ->
            hashPattern lookupTable node



--mergeHashes : Dict String (Nonempty HashData) -> Dict String (Nonempty HashData) -> Dict String (Nonempty HashData)
--mergeHashes dict0 dict1 =
--    Dict.merge
--        (\key a -> )
--        dict0
--        dict1
--        Dict.empty


insertHash : String -> HashData -> Dict String (Nonempty HashData) -> Dict String (Nonempty HashData)
insertHash hash data dict =
    Dict.update hash
        (\maybe ->
            case maybe of
                Just value ->
                    List.Nonempty.cons data value |> Just

                Nothing ->
                    List.Nonempty.fromElement data |> Just
        )
        dict


fromProjectToModule : ModuleNameLookupTable -> ProjectContext -> ModuleContext
fromProjectToModule lookupTable projectContext =
    { lookupTable = lookupTable
    , hashedExpressions = Dict.empty
    }


fromModuleToProject : Rule.ModuleKey -> Rule.Metadata -> ModuleContext -> ProjectContext
fromModuleToProject moduleKey metadata moduleContext =
    { moduleKeys = Dict.singleton (Rule.moduleNameFromMetadata metadata) moduleKey
    }


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts contextA contextB =
    { moduleKeys = Dict.union contextA.moduleKeys contextB.moduleKeys }


type alias ProjectContext =
    { moduleKeys : Dict ModuleName Rule.ModuleKey }


type alias ModuleContext =
    { lookupTable : ModuleNameLookupTable
    , hashedExpressions : Dict String (Nonempty HashData)
    }


type alias HashData =
    { depth : Int, range : Range }


finalEvaluation =
    Debug.todo ""
