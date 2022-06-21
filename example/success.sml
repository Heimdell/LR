let id     = \a => a in
let const  = \a => \b => b in
let pair   = \a => \b => \p => p a b in
let left   = \l => \f => \g => f l in
let right  = \r => \f => \g => g r in

  left () (\x => id x) \y => const () y
