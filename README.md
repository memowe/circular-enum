# circular-enum

**Circular successor & predecessor for bounded enum types**

[![Build and test][test-badge]][test]

[test-badge]: https://github.com/memowe/circular-enum/actions/workflows/test.yml/badge.svg
[test]: https://github.com/memowe/circular-enum/actions/workflows/test.yml

```haskell
import Data.Enum.Circular (csucc, cpred)

data Direction = N | E | S | W deriving (Show, Eq, Enum, Bounded)

show $ take 6 (iterate csucc N)
-- "[N,E,S,W,N,E]"
```

`csucc` and `cpred` are compatible with `succ` and `pred`, but they behave circular on the type boundaries. Requires `Eq`, `Enum` and `Bounded` instances.

## Contributors

[![Contributor Covenant 2.0][coc-img]][coc]

- Mirko Westermeier ([@memowe][memowe-gh])

## Author and License

Copyright (c) 2023 Mirko Westermeier ([@memowe][memowe-gh])

Released under the MIT license. See [LICENSE](LICENSE) for details.

[coc]: CODE_OF_CONDUCT.md
[coc-img]: https://img.shields.io/badge/Code%20of%20Conduct-Contributor%20Covenant%202.0-8f761b.svg?style=flat&logo=adguard&logoColor=lightgray
[memowe-gh]: https://github.com/memowe