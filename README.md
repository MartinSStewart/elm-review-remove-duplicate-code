# elm-review-remove-duplicate-code

This [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rule detects when some code has been copied too many times and should instead be refactored into a single function.
A heuristic is used to estimate when some code has been copied too many times. A long block of code that has been copied 3 times might get reported while a very short piece of code might not get reported even when copied hundreds of times.
Additionally, this rule is more lenient on code in test modules since often you want to duplicate expected values in order to easily understand and edit tests.


## Configuration

```elm
module ReviewConfig exposing (config)

import RemoveDuplicateCode
import Review.Rule exposing (Rule)

config : List Rule
config =
    [ RemoveDuplicateCode.rule { ignore = [], threshold = 25 }
    ]
```

* `ignore` tells this rule to skip some modules when counting code duplication (though it will still mention all the places it found the code, even if some instances are in ignored modules)
* `threshold` determines the total "complexity" the duplicate code can have before it gets reported.
This value doesn't represent any easily explained metric but 25 is a good starting point and if you find that it reports code you don't want to de-duplicate, you can increase it.
To help with determining a good threshold, each error which will tell you how much complexity it had.


## Try it out

You can try the example configuration above out by running the following command:

```bash
elm-review --template MartinSStewart/elm-review-remove-duplicate-code/example
```
