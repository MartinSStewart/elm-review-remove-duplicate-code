module RemoveDuplicateCode exposing (rule)

{-|

@docs rule

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)
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


declarationVisitor : List (Node Declaration) -> ModuleContext -> ( List (Error {}), moduleContext )
declarationVisitor declarations context =
    { context
        | hashedExpression =
            List.map hashDeclaration declarations
    }


hashDeclaration : Node Declaration -> Dict String (Nonempty HashData) -> Dict String (Nonempty HashData)
hashDeclaration (Node range declaration) hashDict =
    case declaration of
        FunctionDeclaration function ->
            let
                implementation =
                    Node.value function.declaration

                result =
                    hashExpression implementation.expression 1 hashDict
            in
            insertHash
                (hashText (Node.value implementation.name) result.hash)
                { depth = 0, range = range }
                result.hashDict

        _ ->
            hashDict


hashText : String -> String -> String
hashText text hash =
    Debug.todo ""


escapeChar =
    "‱"


hashExpression : Node Expression -> Int -> Dict String (Nonempty HashData) -> { hash : String, hashDict : Dict String (Nonempty HashData) }
hashExpression (Node range expression) depth hashDict =
    case expression of
        UnitExpr ->
            { hash = "0" ++ escapeChar, hashDict = hashDict }

        Application nodes ->
            let
                result =
                    hashExpression node (depth + 1) hashDict

                newHash =
                    "10" ++ escapeChar ++ result.hash
            in
            { hash = newHash, hashDict = insertHash newHash { depth = depth, range = range } result.hashDict }

        OperatorApplication string infixDirection left right ->
            Debug.todo ""

        FunctionOrValue moduleName string ->
            Debug.todo ""

        IfBlock condition ifTrue ifFalse ->
            Debug.todo ""

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
            let
                result =
                    hashExpression node (depth + 1) hashDict

                newHash =
                    "10" ++ escapeChar ++ result.hash
            in
            { hash = newHash, hashDict = insertHash newHash { depth = depth, range = range } result.hashDict }

        Literal string ->
            { hash = "11" ++ escapeChar ++ string, hashDict = hashDict }

        CharLiteral char ->
            { hash = "12" ++ escapeChar ++ String.fromChar char, hashDict = hashDict }

        TupledExpression nodes ->
            Debug.todo ""

        ParenthesizedExpression node ->
            hashExpression node (depth + 1) hashDict

        LetExpression letBlock ->
            Debug.todo ""

        CaseExpression caseBlock ->
            Debug.todo ""

        LambdaExpression lambda ->
            Debug.todo ""

        RecordExpr nodes ->
            Debug.todo ""

        ListExpr nodes ->
            Debug.todo ""

        RecordAccess value accessor ->
            Debug.todo ""

        RecordAccessFunction string ->
            { hash = "20" ++ escapeChar ++ string, hashDict = hashDict }

        RecordUpdateExpression node nodes ->
            Debug.todo ""

        GLSLExpression string ->
            { hash = "22" ++ escapeChar ++ string, hashDict = hashDict }



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
