
let x = \p ->
  let y = foo <| \s -> s bar;
  let z = y;
  z p;
x
