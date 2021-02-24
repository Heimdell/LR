
let x = \p ->
  let y = \s -> s;
  let z = y;
  z p;
x
