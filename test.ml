
let x = \p ->
  let y = foo + bar <| \s -> s bar;
  fixity none 9.3 + - <|;
  let z = { y = 1, b = \s -> + - s + + - s };
  z p;
x
