## Slox implementation for the _Crafting Interpeters Tree Walk Interpreter_

As a first pass I will probably just more or less follow along with the `java` implementation (mutability and all), and then try to go back here and there to make things more functional (particularly the `Scanner` sort of bothers me - I think it could maybe be implemented using a `State` `Monad` to avoid the gloval `vars` and modification of the tokens `ArrayList`)