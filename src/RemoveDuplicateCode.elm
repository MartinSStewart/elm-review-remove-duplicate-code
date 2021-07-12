module RemoveDuplicateCode exposing
    ( rule
    , Config, ModuleIgnoreType(..)
    )

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
import List.Extra as List
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
rule : Config -> Rule
rule config =
    Rule.newProjectRuleSchema "RemoveDuplicateCode" initProject
        |> Rule.withModuleVisitor (moduleVisitor config)
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule =
                Rule.initContextCreator fromProjectToModule
                    |> Rule.withModuleNameLookupTable
                    |> Rule.withMetadata
            , fromModuleToProject =
                Rule.initContextCreator fromModuleToProject
                    |> Rule.withModuleKey
                    |> Rule.withMetadata
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.withFinalProjectEvaluation (finalEvaluation config)
        |> Rule.fromProjectRuleSchema


type ModuleIgnoreType
    = OnlyThisModule
    | IncludeSubmodules


type alias Config =
    { ignore : List ( ModuleIgnoreType, List String ) }


initProject : ProjectContext
initProject =
    { moduleData = Dict.empty, hashedModules = Dict.empty }


moduleVisitor : Config -> ModuleRuleSchema {} ModuleContext -> ModuleRuleSchema { hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor config =
    Rule.withDeclarationListVisitor (declarationVisitor config)


isModuleIgnored : Config -> ModuleName -> Bool
isModuleIgnored config moduleName =
    List.any
        (\( ignoreType, moduleName_ ) ->
            case ignoreType of
                OnlyThisModule ->
                    moduleName_ == moduleName

                IncludeSubmodules ->
                    List.take (List.length moduleName_) moduleName == moduleName_
        )
        config.ignore


declarationVisitor : Config -> List (Node Declaration) -> ModuleContext -> ( List (Error {}), ModuleContext )
declarationVisitor _ declarations context =
    ( []
    , { context
        | hashedExpressions =
            List.foldl
                (hashDeclaration context)
                context.hashedExpressions
                declarations
      }
    )


hashDeclaration : ModuleContext -> Node Declaration -> Dict String (Nonempty HashData) -> Dict String (Nonempty HashData)
hashDeclaration context (Node range declaration) hashDict =
    case declaration of
        FunctionDeclaration function ->
            hashFunction context range function hashDict |> .hashDict

        _ ->
            hashDict


hashFunction : ModuleContext -> Range -> Function -> Dict String (Nonempty HashData) -> { hash : String, hashDict : Dict String (Nonempty HashData) }
hashFunction context range function hashDict =
    let
        implementation =
            Node.value function.declaration

        result =
            hashExpression context implementation.expression hashDict

        newHash =
            hashText (Node.value implementation.name) result.hash
    in
    { hash = newHash
    , hashDict =
        insert
            newHash
            { range = range }
            result.hashDict
    }


hashText : String -> String -> String
hashText text hash =
    MD5.hex (text ++ hash)


{-| This is used when building a hash
-}
delimiter : String
delimiter =
    "‱"


canonicalName : ModuleContext -> Range -> List String -> String -> String
canonicalName context range moduleName name =
    (case Review.ModuleNameLookupTable.moduleNameAt context.lookupTable range of
        Just [] ->
            context.currentModule

        Just moduleName_ ->
            moduleName_

        Nothing ->
            moduleName
    )
        ++ [ name ]
        |> String.join "."


hashExpression :
    ModuleContext
    -> Node Expression
    -> Dict String (Nonempty HashData)
    -> { hash : String, hashDict : Dict String (Nonempty HashData) }
hashExpression context (Node range expression) hashDict =
    let
        hashStuff :
            Dict String (Nonempty HashData)
            -> String
            -> List (Dict String (Nonempty HashData) -> { hash : String, hashDict : Dict String (Nonempty HashData) })
            -> { hash : String, hashDict : Dict String (Nonempty HashData) }
        hashStuff hashDict_ hash nodes =
            let
                finalResult =
                    List.foldl
                        (\node state ->
                            let
                                result =
                                    node state.hashDict
                            in
                            { hash = hashText state.hash result.hash
                            , hashDict = result.hashDict
                            }
                        )
                        { hash = delimiter ++ hash, hashDict = hashDict_ }
                        nodes
            in
            { hash = finalResult.hash
            , hashDict = insert finalResult.hash { range = range } finalResult.hashDict
            }

        hashHelper hash nodes =
            hashStuff
                hashDict
                hash
                (List.map (hashExpression context) nodes)
    in
    case expression of
        UnitExpr ->
            { hash = "0" ++ delimiter, hashDict = hashDict }

        Application nodes ->
            hashHelper ("1" ++ delimiter) nodes

        OperatorApplication string _ left right ->
            if string == "<|" then
                hashHelper ("2" ++ delimiter ++ "|>") [ right, left ]

            else
                hashHelper ("2" ++ delimiter ++ string) [ left, right ]

        FunctionOrValue moduleName name ->
            { hash = "3" ++ delimiter ++ canonicalName context range moduleName name
            , hashDict = hashDict
            }

        IfBlock condition ifTrue ifFalse ->
            hashHelper ("4" ++ delimiter) [ condition, ifTrue, ifFalse ]

        PrefixOperator string ->
            { hash = "5" ++ delimiter ++ string, hashDict = hashDict }

        Operator string ->
            { hash = "6" ++ delimiter ++ string, hashDict = hashDict }

        Integer int ->
            { hash = "7" ++ delimiter ++ String.fromInt int, hashDict = hashDict }

        Hex int ->
            { hash = "8" ++ delimiter ++ String.fromInt int, hashDict = hashDict }

        Floatable float ->
            { hash = "9" ++ delimiter ++ String.fromFloat float, hashDict = hashDict }

        Negation node ->
            hashHelper ("10" ++ delimiter) [ node ]

        Literal string ->
            { hash = "11" ++ delimiter ++ string, hashDict = hashDict }

        CharLiteral char ->
            { hash = "12" ++ delimiter ++ String.fromChar char, hashDict = hashDict }

        TupledExpression nodes ->
            hashHelper ("13" ++ delimiter) nodes

        ParenthesizedExpression node ->
            hashExpression context node hashDict

        LetExpression letBlock ->
            hashStuff
                hashDict
                ("14" ++ delimiter)
                (hashExpression context letBlock.expression :: List.map (hashLetDeclaration context) letBlock.declarations)

        CaseExpression caseBlock ->
            let
                patternHash =
                    List.map (Tuple.first >> hashPattern context) caseBlock.cases |> String.join ","
            in
            hashHelper ("15" ++ delimiter ++ patternHash) (caseBlock.expression :: List.map Tuple.second caseBlock.cases)

        LambdaExpression lambda ->
            let
                argsHash =
                    List.map (hashPattern context) lambda.args |> String.join " "
            in
            hashHelper ("16" ++ delimiter ++ argsHash) [ lambda.expression ]

        RecordExpr nodes ->
            let
                sorted =
                    List.sortBy (Node.value >> Tuple.first >> Node.value) nodes

                fieldNames =
                    List.map (Node.value >> Tuple.first >> Node.value) sorted |> String.join " "
            in
            List.map (Node.value >> Tuple.second) sorted
                |> hashHelper ("17" ++ delimiter ++ fieldNames)

        ListExpr nodes ->
            hashHelper ("18" ++ delimiter) nodes

        RecordAccess value (Node _ accessor) ->
            hashHelper ("19" ++ delimiter ++ accessor) [ value ]

        RecordAccessFunction string ->
            { hash = "20" ++ delimiter ++ string, hashDict = hashDict }

        RecordUpdateExpression (Node _ record) nodes ->
            let
                sorted =
                    nodes
                        |> List.sortBy (Node.value >> Tuple.first >> Node.value)

                fieldNames =
                    List.map (Node.value >> Tuple.first >> Node.value) sorted |> String.join " "
            in
            List.map (Node.value >> Tuple.second) sorted
                |> hashHelper ("21" ++ delimiter ++ record ++ " " ++ fieldNames)

        GLSLExpression string ->
            { hash = "22" ++ delimiter ++ string, hashDict = hashDict }


hashLetDeclaration :
    ModuleContext
    -> Node LetDeclaration
    -> Dict String (Nonempty HashData)
    -> { hash : String, hashDict : Dict String (Nonempty HashData) }
hashLetDeclaration context (Node range letDeclaration) hashDict =
    case letDeclaration of
        LetFunction letFunction ->
            hashFunction context range letFunction hashDict

        LetDestructuring pattern expression ->
            let
                result =
                    hashExpression context expression hashDict

                newHash =
                    hashText (hashPattern context pattern) result.hash
            in
            { hash = newHash, hashDict = insert newHash { range = range } result.hashDict }


hashPattern : ModuleContext -> Node Pattern -> String
hashPattern context (Node range pattern) =
    let
        hashHelper id nodes =
            List.foldl (\node hash -> hashText hash (hashPattern context node)) (delimiter ++ id ++ delimiter) nodes
    in
    case pattern of
        UnitPattern ->
            "23" ++ delimiter

        AllPattern ->
            "24" ++ delimiter

        CharPattern char ->
            "25" ++ delimiter ++ String.fromChar char

        StringPattern string ->
            "26" ++ delimiter ++ string

        IntPattern int ->
            "27" ++ delimiter ++ String.fromInt int

        HexPattern int ->
            "28" ++ delimiter ++ String.fromInt int

        FloatPattern float ->
            "29" ++ delimiter ++ String.fromFloat float

        TuplePattern nodes ->
            hashHelper "30" nodes

        RecordPattern fields ->
            "31" ++ delimiter ++ (List.map Node.value fields |> List.sort |> String.join " ")

        UnConsPattern a b ->
            hashHelper "32" [ a, b ]

        ListPattern nodes ->
            hashHelper "33" nodes

        VarPattern var ->
            "34" ++ delimiter ++ var

        NamedPattern qualifiedNameRef nodes ->
            hashHelper
                ("35" ++ delimiter ++ canonicalName context range qualifiedNameRef.moduleName qualifiedNameRef.name)
                nodes

        AsPattern node (Node _ text) ->
            hashHelper ("36" ++ delimiter ++ text) [ node ]

        ParenthesizedPattern node ->
            hashPattern context node


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


fromProjectToModule : ModuleNameLookupTable -> Rule.Metadata -> ProjectContext -> ModuleContext
fromProjectToModule lookupTable metadata _ =
    { lookupTable = lookupTable
    , currentModule = Rule.moduleNameFromMetadata metadata
    , hashedExpressions = Dict.empty
    }


fromModuleToProject : Rule.ModuleKey -> Rule.Metadata -> ModuleContext -> ProjectContext
fromModuleToProject moduleKey metadata moduleContext =
    { moduleData =
        Dict.singleton
            (Rule.moduleNameFromMetadata metadata)
            { moduleKey = moduleKey
            , isInSourceDirectories = Rule.isInSourceDirectories metadata
            }
    , hashedModules =
        Dict.map
            (\_ value -> List.Nonempty.map (toProjectHashData (Rule.moduleNameFromMetadata metadata)) value)
            moduleContext.hashedExpressions
    }


toProjectHashData : ModuleName -> HashData -> ProjectHashData
toProjectHashData moduleName hashData =
    { moduleName = moduleName, range = hashData.range }


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts contextA contextB =
    { moduleData = Dict.union contextA.moduleData contextB.moduleData
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
    { moduleData : Dict ModuleName { moduleKey : Rule.ModuleKey, isInSourceDirectories : Bool }, hashedModules : Dict String (Nonempty ProjectHashData) }


type alias ModuleContext =
    { lookupTable : ModuleNameLookupTable
    , currentModule : ModuleName
    , hashedExpressions : Dict String (Nonempty HashData)
    }


type alias HashData =
    { range : Range }


type alias ProjectHashData =
    { moduleName : ModuleName, range : Range }


addRanges : Nonempty ProjectHashData -> Dict ModuleName (Nonempty Range) -> Dict ModuleName (Nonempty Range)
addRanges nonempty dict =
    List.Nonempty.toList nonempty
        |> List.foldl
            (\a dict_ ->
                insert a.moduleName a.range dict_
            )
            dict


finalEvaluation : Config -> ProjectContext -> List (Error { useErrorForModule : () })
finalEvaluation config projectContext =
    let
        potentialErrors : List ( String, Nonempty ProjectHashData )
        potentialErrors =
            Dict.toList projectContext.hashedModules
                |> List.filter (Tuple.second >> passesHeuristic config projectContext)
                |> List.sortBy (Tuple.second >> heuristic config projectContext >> negate)
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
                        if passesHeuristic config projectContext newPotentialErrors then
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
                case Dict.get firstExample.moduleName projectContext.moduleData of
                    Just { moduleKey } ->
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


passesHeuristic : Config -> ProjectContext -> Nonempty ProjectHashData -> Bool
passesHeuristic config context nonempty =
    heuristic config context nonempty > 2000


heuristic : Config -> ProjectContext -> Nonempty ProjectHashData -> Float
heuristic config context nonempty =
    let
        filtered =
            List.Nonempty.toList nonempty |> List.filter (.moduleName >> isModuleIgnored config >> not)

        minimumRange =
            List.map
                (\{ range } ->
                    if range.start.row == range.end.row then
                        range.end.column - range.start.column

                    else
                        (range.end.row - range.start.row) * 100
                )
                filtered
                |> List.minimum
                |> Maybe.withDefault 0

        testCount =
            filtered
                |> listCount
                    (\a ->
                        case Dict.get a.moduleName context.moduleData of
                            Just { isInSourceDirectories } ->
                                not isInSourceDirectories

                            Nothing ->
                                False
                    )

        count =
            List.length filtered
    in
    if count > 1 then
        logBase 2 (toFloat count - toFloat testCount * 0.5) * toFloat minimumRange

    else
        0


{-| Returns the number of elements in a list that satisfy a given predicate.
Equivalent to `List.length (List.filter pred list)` but more efficient.

    count
        (modBy 2 >> (==) 1) [ 1, 2, 3, 4, 5, 6, 7 ]
    --> 4

    count
        ((==) "yeah")
        [ "She", "loves", "you", "yeah", "yeah", "yeah" ]
    --> 3

Copied from elm-community/list-extra

-}
listCount : (a -> Bool) -> List a -> Int
listCount predicate =
    List.foldl
        (\x acc ->
            if predicate x then
                acc + 1

            else
                acc
        )
        0
