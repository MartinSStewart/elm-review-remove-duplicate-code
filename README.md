# elm-review-remove-duplicate-code

Provides [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rules to REPLACEME.


## Provided rules

- [`RemoveDuplicateCode`](https://package.elm-lang.org/packages/MartinSStewart/elm-review-remove-duplicate-code/1.0.0/RemoveDuplicateCode) - Reports REPLACEME.


## Configuration

```elm
module ReviewConfig exposing (config)

import RemoveDuplicateCode
import Review.Rule exposing (Rule)

config : List Rule
config =
    [ RemoveDuplicateCode.rule
    ]
```


## Try it out

You can try the example configuration above out by running the following command:

```bash
elm-review --template MartinSStewart/elm-review-remove-duplicate-code/example
```
