module RemoveDuplicateCode exposing (rule)

{-|

@docs rule

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Expression(..), Function, LetDeclaration(..))
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.Syntax.Range exposing (Range)
import List.Nonempty exposing (Nonempty(..))
import MD5
import Review.ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Error, ModuleRuleSchema, Rule)


{-| This rule reports when it finds code that has been copied too many times and should instead be made into a reusable function.

Note that "too many times" depends both on the number of copies and how much code is being copied.
You can write `Element.width Element.fill` hundreds of times and you won't get an error.
But if you were to copy paste a hundred line block of code twice then this rule would tell you to place it in a function instead.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template MartinSStewart/elm-review-remove-duplicate-code/example --rules RemoveDuplicateCode
```

-}
rule : Rule
rule =
    Rule.newProjectRuleSchema "RemoveDuplicateCode" initProject
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
        |> Rule.withFinalProjectEvaluation finalEvaluation
        |> Rule.fromProjectRuleSchema


initProject : ProjectContext
initProject =
    { moduleKeys = Dict.empty, hashedModules = Dict.empty }


moduleVisitor : ModuleRuleSchema {} ModuleContext -> ModuleRuleSchema { hasAtLeastOneVisitor : () } ModuleContext
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
            hashExpression lookupTable 1 implementation.expression hashDict

        newHash =
            hashText (Node.value implementation.name) result.hash
    in
    { hash = newHash
    , hashDict =
        insert
            newHash
            { depth = 0, range = range }
            result.hashDict
    }


hashText : String -> String -> String
hashText text hash =
    MD5.hex (text ++ hash)


{-| This is added to hashes to delimit parts of the hash
-}
escapeChar : String
escapeChar =
    "‱"


hashExpression :
    ModuleNameLookupTable
    -> Int
    -> Node Expression
    -> Dict String (Nonempty HashData)
    -> { hash : String, hashDict : Dict String (Nonempty HashData) }
hashExpression lookupTable depth (Node range expression) hashDict =
    let
        hashStuff :
            (Int -> Node a -> Dict String (Nonempty HashData) -> { hash : String, hashDict : Dict String (Nonempty HashData) })
            -> String
            -> List (Node a)
            -> { hash : String, hashDict : Dict String (Nonempty HashData) }
        hashStuff hashFunction_ hash nodes =
            let
                finalResult =
                    List.foldl
                        (\node state ->
                            let
                                result =
                                    hashFunction_ (depth + 1) node state.hashDict
                            in
                            { hash = hashText state.hash result.hash
                            , hashDict = result.hashDict
                            }
                        )
                        { hash = escapeChar ++ hash, hashDict = hashDict }
                        nodes
            in
            { hash = finalResult.hash
            , hashDict = insert finalResult.hash { depth = depth, range = range } finalResult.hashDict
            }

        hashHelper =
            hashStuff (hashExpression lookupTable)
    in
    case expression of
        UnitExpr ->
            { hash = "0" ++ escapeChar, hashDict = hashDict }

        Application nodes ->
            hashHelper ("1" ++ escapeChar) nodes

        OperatorApplication string _ left right ->
            hashHelper ("2" ++ escapeChar ++ string) [ left, right ]

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
            hashHelper ("4" ++ escapeChar) [ condition, ifTrue, ifFalse ]

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
            hashHelper ("10" ++ escapeChar) [ node ]

        Literal string ->
            { hash = "11" ++ escapeChar ++ string, hashDict = hashDict }

        CharLiteral char ->
            { hash = "12" ++ escapeChar ++ String.fromChar char, hashDict = hashDict }

        TupledExpression nodes ->
            hashHelper ("13" ++ escapeChar) nodes

        ParenthesizedExpression node ->
            hashExpression lookupTable (depth + 1) node hashDict

        LetExpression letBlock ->
            hashStuff (hashLetDeclaration lookupTable) ("14" ++ escapeChar) letBlock.declarations

        CaseExpression caseBlock ->
            let
                patternHash =
                    List.map (Tuple.first >> hashPattern lookupTable) caseBlock.cases |> String.join ","
            in
            hashHelper ("15" ++ escapeChar ++ patternHash) (caseBlock.expression :: List.map Tuple.second caseBlock.cases)

        LambdaExpression lambda ->
            let
                argsHash =
                    List.map (hashPattern lookupTable) lambda.args |> String.join " "
            in
            hashHelper ("16" ++ escapeChar ++ argsHash) [ lambda.expression ]

        RecordExpr nodes ->
            let
                sorted =
                    List.sortBy (Node.value >> Tuple.first >> Node.value) nodes

                fieldNames =
                    List.map (Node.value >> Tuple.first >> Node.value) sorted |> String.join " "
            in
            List.map (Node.value >> Tuple.second) sorted
                |> hashHelper ("17" ++ escapeChar ++ fieldNames)

        ListExpr nodes ->
            hashHelper ("18" ++ escapeChar) nodes

        RecordAccess value (Node _ accessor) ->
            hashHelper ("19" ++ escapeChar ++ accessor) [ value ]

        RecordAccessFunction string ->
            { hash = "20" ++ escapeChar ++ string, hashDict = hashDict }

        RecordUpdateExpression (Node _ record) nodes ->
            let
                sorted =
                    nodes
                        |> List.sortBy (Node.value >> Tuple.first >> Node.value)

                fieldNames =
                    List.map (Node.value >> Tuple.first >> Node.value) sorted |> String.join " "
            in
            List.map (Node.value >> Tuple.second) sorted
                |> hashHelper ("21" ++ escapeChar ++ record ++ " " ++ fieldNames)

        GLSLExpression string ->
            { hash = "22" ++ escapeChar ++ string, hashDict = hashDict }


hashLetDeclaration :
    ModuleNameLookupTable
    -> Int
    -> Node LetDeclaration
    -> Dict String (Nonempty HashData)
    -> { hash : String, hashDict : Dict String (Nonempty HashData) }
hashLetDeclaration lookupTable depth (Node range letDeclaration) hashDict =
    case letDeclaration of
        LetFunction letFunction ->
            hashFunction lookupTable range letFunction hashDict

        LetDestructuring pattern expression ->
            let
                result =
                    hashExpression lookupTable (depth + 1) expression hashDict

                newHash =
                    hashText (hashPattern lookupTable pattern) result.hash
            in
            { hash = newHash, hashDict = insert newHash { depth = depth, range = range } result.hashDict }


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


insert : comparable -> a -> Dict comparable (Nonempty a) -> Dict comparable (Nonempty a)
insert hash data dict =
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
fromProjectToModule lookupTable _ =
    { lookupTable = lookupTable
    , hashedExpressions = Dict.empty
    }


fromModuleToProject : Rule.ModuleKey -> Rule.Metadata -> ModuleContext -> ProjectContext
fromModuleToProject moduleKey metadata moduleContext =
    { moduleKeys = Dict.singleton (Rule.moduleNameFromMetadata metadata) moduleKey
    , hashedModules =
        Dict.map
            (\_ value -> List.Nonempty.map (toProjectHashData (Rule.moduleNameFromMetadata metadata)) value)
            moduleContext.hashedExpressions
    }


toProjectHashData : ModuleName -> HashData -> ProjectHashData
toProjectHashData moduleName hashData =
    { moduleName = moduleName, depth = hashData.depth, range = hashData.range }


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts contextA contextB =
    { moduleKeys = Dict.union contextA.moduleKeys contextB.moduleKeys
    , hashedModules =
        Dict.merge
            (\_ _ result -> result)
            (\hash a b result -> Dict.insert hash (List.Nonempty.append a b) result)
            (\hash b result -> Dict.insert hash b result)
            contextA.hashedModules
            contextB.hashedModules
            contextA.hashedModules
    }


type alias ProjectContext =
    { moduleKeys : Dict ModuleName Rule.ModuleKey, hashedModules : Dict String (Nonempty ProjectHashData) }


type alias ModuleContext =
    { lookupTable : ModuleNameLookupTable
    , hashedExpressions : Dict String (Nonempty HashData)
    }


type alias HashData =
    { depth : Int, range : Range }


type alias ProjectHashData =
    { moduleName : ModuleName, depth : Int, range : Range }


addRanges : Nonempty ProjectHashData -> Dict ModuleName (Nonempty Range) -> Dict ModuleName (Nonempty Range)
addRanges nonempty dict =
    List.Nonempty.toList nonempty
        |> List.foldl
            (\a dict_ ->
                insert a.moduleName a.range dict_
            )
            dict


finalEvaluation : ProjectContext -> List (Error { useErrorForModule : () })
finalEvaluation projectContext =
    let
        potentialErrors : List ( String, Nonempty ProjectHashData )
        potentialErrors =
            Dict.toList projectContext.hashedModules
                |> List.filter (Tuple.second >> passesHeuristic)
                |> List.sortBy (Tuple.second >> heuristic >> negate)
    in
    potentialErrors
        |> List.foldl
            (\( _, potentialError ) ( errors, moduleRanges ) ->
                let
                    usedRangesRemoved : List ProjectHashData
                    usedRangesRemoved =
                        List.Nonempty.toList potentialError
                            |> List.filter
                                (\potentialError_ ->
                                    case Dict.get potentialError_.moduleName moduleRanges of
                                        Just ranges ->
                                            List.Nonempty.any (\range -> Elm.Syntax.Range.combine [ potentialError_.range, range ] == range) ranges |> not

                                        Nothing ->
                                            True
                                )
                in
                case List.Nonempty.fromList usedRangesRemoved of
                    Just newPotentialErrors ->
                        if passesHeuristic newPotentialErrors then
                            ( potentialError :: errors, addRanges newPotentialErrors moduleRanges )

                        else
                            ( errors, moduleRanges )

                    Nothing ->
                        ( errors, moduleRanges )
            )
            ( [], Dict.empty )
        |> Tuple.first
        |> List.filterMap
            (\(Nonempty firstExample restOfExamples) ->
                case Dict.get firstExample.moduleName projectContext.moduleKeys of
                    Just moduleKey ->
                        let
                            posToString position =
                                String.fromInt position.row ++ ":" ++ String.fromInt position.column

                            allExamples =
                                List.map
                                    (\example ->
                                        "\n"
                                            ++ String.join "." example.moduleName
                                            ++ " "
                                            ++ posToString example.range.start
                                            ++ " to "
                                            ++ posToString example.range.end
                                    )
                                    (firstExample :: restOfExamples)
                                    |> String.concat
                        in
                        Rule.errorForModule moduleKey
                            { message =
                                "Found code that is repeated too often ("
                                    ++ String.fromInt (List.length restOfExamples + 1)
                                    ++ " times) and can instead be combined into a single function.\n\nHere are all the places it's used:\n"
                                    ++ allExamples
                            , details =
                                [ "It's okay to duplicate short snippets several times or duplicate larger chunks 2-3 times. But here it looks like this code is repeated too often and it would be better to have a single function for it."
                                ]
                            }
                            firstExample.range
                            |> Just

                    Nothing ->
                        Nothing
            )


passesHeuristic : Nonempty ProjectHashData -> Bool
passesHeuristic nonempty =
    heuristic nonempty > 2000


heuristic : Nonempty ProjectHashData -> Float
heuristic nonempty =
    let
        minimumRange =
            List.Nonempty.map
                (\{ range } ->
                    if range.start.row == range.end.row then
                        range.end.column - range.start.column

                    else
                        (range.end.row - range.start.row) * 100
                )
                nonempty
                |> nonemptyMinimumBy identity

        count =
            List.Nonempty.length nonempty
    in
    if count > 1 then
        logBase 2 (toFloat count) * toFloat minimumRange

    else
        0


{-| Given a function to map a type to a comparable type, find the **first**
minimum element in a non-empty list.

Copied from <https://github.com/langyu-app/elm-ancillary-nonempty-list>

-}
nonemptyMinimumBy : (a -> comparable) -> Nonempty a -> a
nonemptyMinimumBy f (Nonempty l ls) =
    let
        step : a -> ( a, comparable ) -> ( a, comparable )
        step x (( _, fY ) as acc) =
            let
                fX : comparable
                fX =
                    f x
            in
            if fX < fY then
                ( x, fX )

            else
                acc
    in
    Tuple.first <| List.foldl step ( l, f l ) ls
