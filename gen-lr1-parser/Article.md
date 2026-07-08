
# LR(1)-разбор

LR(1)-разбор является методом разбора посредством рекурсивного подъёма.
При этом для каждого следующего токена решение о дальнейших действиях выносится из таблиц разбора,
которые будут расмотрены далее.

Преимуществом LR(1)-разбора является то, что парсер полностью безоткатный.

Недостатком является сложность в построении таблиц или автомата разбора, а так же (по сравнению с LALR(1))
большое количество состояний.

## Грамматика

Для работы LR(1)-парсера необходимо составить грамматику.

Мы будем разбирать на примере арифметических выражений:

```ebnf
S = E

E = E + F
E = F

F = F * T
F = T

T = ( E )
T = number
```

Можно заметить, что в грамматике не используется операторa выбора (`|`), а так же операторов повтора.
Это сделано для упрощения анализа грамматики.

Выбор в грамматике осуществляется заданием нескольких правил для сущности, у которой есть варианты конструкции.

Используется следующая конвенция:
1) нетерминалы, они же сущности (`S`, `E`, `F`, `T`) -- пишутся с большой буквы.
2) терминалы (`+`, `*`, `(`, `)`, `number`) -- все остальные.

Терминал обозначает одну лексему из входного потока.
Предполагается, что фаза лексического разбора уже прошла, исходный текст нарезан на лексемы и каждой присвоен один из терминальных классов --
(`+`, `*`, `(`, `)`, `number`).
Класс `number` включает в себя все лексемы, представляющие из себя номер.
Остальные классы данной грамматики являются единичными и сожержат только одну, одноимённую с каждым, лексему.

Сущность `S` -- она же `Start` -- является фантомной и предназначена в качестве точки входа в грамматику.

Мы так же пока не рассматриваем действия свёртки, и не будем до построения автомата разбора.

## Элементы

Мы объединим терминалы и сущности под названием "элемент".

Таким образом у правила

```ebnf
E = E + F
```

Три элемента -- `E`, `+` и `F`.

## Позиция разбора

Рассмотрим позицию

```ebnf
E = E + . F {$ +}
```

для правила

```ebnf
E = E + F
```

Точка у позиции разделяет левую часть, которая уже разобрана и лежит в стеке, от правой -- которую ещё предстоит разобрать.
Множество терминалов после позиции в фигурных скобках обозначает, какие терминалы после завершения позиции приведут к свёртке этим правилом.

Для простоты работы мы будем позволять 1 и только 1 терминал предпросмотра.
Таким образом эта "позиция" будет в реальности представлена двумя позициями (с одинаковыми действиями свёртки):
```ebnf
E = E + . F {$}
E = E + . F {+}
```

Позиции необходимы для того, чтобы отслеживать процесс разбора.

Позиции бывают
1) завершённые `E = E + F . {+}`
2) незавершённые `E = E . + F {+}`

Элемент, перед которым стоит точка -- который будет разобран следующим -- мы будем называть локусом.
У завершённых позиций локус является пустым, у незавершённых он содержит терминал или нетерминал.

## Таблица `FIRST()`

Нам понадобится таблица, в которой будет указано множество возможных начальных терминалов каждой сущности.

Для каждого правила, если оно начинается с терминала, то этот терминал добавляется в множество для сущности, которую это правило создаёт.

Если же правило начинается с нетерминала, то в множество для создаваемой им сущности добавляется множество той сущности-нетерминала,
с которой это правило начитается.

```
S = {}
E = {}
F = {}
T = {number (}
```

Этот алгоритм нужно повторять до тех пор, пока таблица не перестанет меняться, так как терминалы `FIRST(T)` должны "добраться" до сущностей `F`, `E` и `S`.

```
S = {number (}
E = {number (}
F = {number (}
T = {number (}
```

В данном случае у всех сущностей множество возможных начальных терминалов одинаковое, но для произвольной грамматика это не так.

## Состояние разбора

Мы не можем отслеживать процесс разбора, используя только одну позицию.

```ebnf
E = E + .F {+}
```

В данном случае `F`, как нетерминал, может быть непустой последовательностью лексем абсолютно произвольной длины.
Нам необходимо так же отслеживать разбор _внутри_ `F`.

Для этого мы добавим ещё и все стартовые позиции правил, создающих `F`.

```ebnf
E =  E + .F {+}
F = .F *  T {?}
F = .T      {?}
```

Поскольку после данной позиции может идти только знак `+`, то только он и может следовать за последним нетерминалом `F` в исходной позиции.
Расставим предпросмотр для двух новых позиций:

```ebnf
E =  E + .F {+}
F = .F *  T {+}
F = .T      {+}
```

Мы по-прежнему не решили проблему -- в данном состоянии ожидается разбор сущностей `F` и `T`, и мы всё ещё не можем ничего сказать о процессе их разбора. Кстати, после нового упоминания `F` идёт терминал `*`.

----

Для полного решения проблемы необходимо расчитать _замыкание_ множества правил.

Для этого надо для каждого нетерминала в локусе добавить стартовые позиции всех его правил.
Предпросмотр этих позиций определяется следующим образом:
1) Если после сущности идёт терминал (как в `T = ( .E )`), тот этот терминал (здесь это `)`) становится предпросмотром.
2) Если после сущности идёт другая сущность (как в `Foo = . Bar Qux`), то для каждого терминала в `FIRST(Qux)` мы добавляем
   позицию для каждого правила у `Foo`.

   ```
   Foo = A kek
   Foo = foo B

   FIRST(Qux) = {a b}
   ```

   =>

   ```
   Foo = . A kek {a}
   Foo = . A kek {b}
   Foo = . foo B {a}
   Foo = . foo B {b}
   ```
3) Если после сущности ничего нет, то предпросмотр берётся из текущей позиции.

   ```
      E =  E + .F {+}
   => F = .F *  T {+}
   => F = .T      {+}
   ```

То есть, предпросмотром становится то, что может идти после локуса `.X` -- если там терминал, то это будет он;
если нетерминал, то его начальные терминалы; если после локуса ничего нет -- то предпросмотр исходного правила.

Опять, как и в случае с `FIRST()`, мы продолжаем, пока мы не перестанем находить новые позиции.

В случае нашей грамматики мы придём к следующему множеству позиций:

```
0) E =  E + .F {+}  -- начальная позиция
1) F = .F *  T {+}  -- вызвана позицией 0 из-за .F {+}
2) F = .T      {+}  -- вызвана позицией 0 из-за .F {+}
3) F = .F *  T {*}  -- вызвана позицией 1 из-за .F  *
4) F = .T      {*}  -- вызвана позицией 1 из-за .F  *
5) T = .( E )  {+}  -- вызвана позицией 2 из-за .T {+}
6) T = .number {+}  -- вызвана позицией 2 из-за .T {+}
7) T = .( E )  {*}  -- вызвана позицией 4 из-за .T {*}
8) T = .number {*}  -- вызвана позицией 4 из-за .T {*}
```

Заметим, что в предпросмотр просочился нетерминал `*`, так как во второй найденной позиции он идёт сразу после `F` в локусе:
```
F = .F *  T {+}
     ^ ^
     | +- * сразу за F
     |
     +--- F в локусе
```

Таким образом, _состояние разбора_ это замыкание от какого-то набора позиций, в котором есть позиции со всеми корректным нетерминалами в локусах.

Начальное множество позиций мы назовём _ядром_, а полный набор будем звать _замыканием_.

Для составления замыкания понадобится сама грамматика и готовая таблица `FIRST()`.

## Таблицы GOTO и ACTION

Состояние -- это прекрасно. Мы даже можем расчитать начальное:

```
S = .E      {$}
E = .E + F  {+ $}
E = .F      {+ $}
F = .F * T  {* + $}
F = .T      {* + $}
T = .( E )  {* + $}
T = .number {* + $}
```

Здесь терминал `$` обозначает конец потока входных лексем.

Так же мы вернёмся к компактной записи. Стоит помнить, что
```
E = .E + F  {+ $}
```
это
```
E = .E + F  {+}
E = .E + F  {$}
```

Нам необходимо построить все остальные состояния и переходы между ними.

Мы будем строить `GOTO` и `ACTION` одновременно, так что стоит обсудить их структуру.

`GOTO` имеет размерности "состояние" и "нетерминал" и `GOTO`[`StateN`, `Foo`] = `StateM`,
где `StateM` это то состояние, в которое надо переходить из `StateN` после того, как какое-то действие свёртки положит на стек нетерминал `Foo`.

`ACTION` [`StateN`, `foo`] = `Set<Act>`, где `Act` это одно из трёх:
1) `Shift` `StateM` -- если на входе был терминал `foo`, то мы вносим его значение на стек и переходим в состояние `StateM`;
2) `Reduce <ruleN>` -- если на входе был терминал `foo`, то мы снимаем со стека столько значений _и состояний разбора_ сколько элементов было у правила `ruleN` и кладём обратно результат свёрточного действия этого правила;
3) `Accept` -- если на входе был терминал `$`, то мы просто возвращаем единственный элемент стека, как результат разбора.

Структуру стека мы рассмотрим подробнее в последующих главах.

У таблиц `GOTO` и `ACTION` есть общая размерность -- исходное состояние. При распечатке этих таблиц они обычно склеены по этой размерности.
Мы объединим их в одну таблицу `TABLE[,]` и будем считать, что `TABLE[StateN, EntityM]` имеет тип "состояние", а `TABLE[StateN, termM]` имеет тип "действие".

Сначала мы выбираем одно состояние их множества нерассмотренных и каким-либо образом удаляем с него пометку "новое".

Теперь, разобъём позиции этого состояния на три группы:
1) ожидает терминал;
2) ожидает нетерминал;
3) ожидает свёртки.

### GOTO

Возьмём те позиции, которые ожидают нетерминал и сгруппируем их по этим нетерминалам.
Мы получим `GROUPED[Entity] = Set<Position>`.
Далее, для каждой группы -- мы сдвигаем каждую позицию в этой группе на один элемент:

```
E = .E + F {$}
T = ( .E ) {$}
```
=>
```
E = E .+ F {$}
T = ( E .) {$}
```
Результат становится ядром нового состояния, после того, как мы отпарсили сущность `E` в исходном состоянии.

Вычисляем замыкание этого ядра и присваиваем его в таблицу:
```
TABLE[StateN, E] = map(next, GROUPED[E])
```

Так же добавляем это состояние в список "новых"

### ACTION (Shift)

Возьмём те позиции, которые ожидают терминал и сгруппируем их по этим терминалам.
Мы получим `GROUPED[term] = Set<Position>`.

Далее, для каждой группы -- мы сдвигаем каждую позицию в этой группе на один элемент:

```
T = .( E ) {$}
```
=>
```
T = ( .E ) {$}
```
Результат становится ядром нового состояния, после того, как мы отпарсили сущность `E` в исходном состоянии.

Вычисляем замыкание этого ядра и присваиваем его в таблицу:
```
TABLE[StateN, '('] += SHIFT(map(next, GROUPED['(']))
```

Так же добавляем это состояние в список "новых".

### ACTION (Reduce)

Возьмём завершённые позиции, и сгруппируем их по символам предпросмотра.

```
E = E + F . {+}  -- позиция порождена rule1
T = ( E ) . {*}  -- позиция порождена rule2
```

Для каждой такой позиции мы добавляем действие Reduce:
```
TABLE[StateN, '+'] += Reduce(<rule1>)
TABLE[StateN, '*'] += Reduce(<rule2>)
```

### Построение таблиц

Мы продолжаем операции выше до тех пор, пока мы не перестанем порождать новые состояния.
Если для какого-то вновь порождённого состояния уже есть колонка в таблице, его нужно проигнорировать.

## Разрешение конфликтов

У нас раздел `ACTION` имеет сигнатуру `ACTION[State, term] : Set<Act>`.
В случае если множество для какогого `ACTION[StateN, foo]` состоит из более чем одного действия, мы имеем конфликт.

Есть два вида конфликтов:
1) **shift/reduce-конфликт** -- традиционно решается предпочтением shift все reduce в это ситуации (так делает генератор парсеров happy)
2) **reduce/reduce-конфликт** -- традиционно решается либо никак (сообщаем пользователю об ошибке), либо использованием GLR-подхода,
   в котором стек расслаивается при конфликте, а отсутствие решений в таблице в одной из ветвей просто её завершает.

Мы будем придерживаться классического LR(1)-подхода с разрешением shift/reduce в shift.

## Автомат разбора

Можно построить парсер, который будет игнорировать типы сущностей, которые он разбирает.

Но это не интересно, потому что будет совершенно непонятно, что и почему происходит; так же придётся регулярно "забывать" типы сущностей и действий свёртки и столь же регулярно из "вспоминать" -- и под "вспоминать" я имею ввиду "вслепую приводить из тому типу, который мы ожидаем".

Поэтому мы воспользуемся подходом, сохраняющим типы.

Для того, чтобы нам не пришлось извлекать из широких штанин кубическую Агду или какие другие завтипы, мы будем из грамматики генерировать файл с парсером.

Генерировать мы будем на языке хаскелль, потому что навскидку только он одновременно прост и обладает нужными возможностями.
Мы будем освещать эти возможности по мере их появления.

### Подготовительная работа

Объявим тип лексем.
```haskell
data T
  = TOpen
  | TClose
  | TPlus
  | TMult
  | TNumber Int
```

Так же объявим типы значений для этих лексем

```haskell
data Open  = Open
data Close = Close
data Plus  = Plus
data Mult  = Mult
```

Эти типы не содержат никакой информации, их задача -- предотвратить ошибки.

В более практичной реализации вместо них можно использовать тип `()`.

Типом значения лексемы `TNumber` будет `Integer`.

### Состояния разбора

Здесь будут приведены состояния по их номерам и их таблицы, полученные по алгоритму описанному выше в справочных целях.

Т.е., можно пропустить и перейти сразу к [Состояния автомата](#состояния-автомата)

#### Состояния по номерам

Далее указаны только ядра состояний, для читаемости.

```
  0  => S = .E        {$}
  1  =>
        S =  E .      {$}
        E =  E .+ F   {$ +}
  2  =>
        E =  F .      {$ +}
        F =  F .* T   {$ * +}
  3  =>
        E =  E + F .  {$ +}
        F =  F .* T   {$ * +}
  4  => F =  T .      {$ * +}
  5  => T =  number . {$ * +}
  6  => T =  ( .E )   {$ * +}
  7  => E =  E + .F   {$ +}
  8  => F =  F * .T   {$ * +}
  9  =>
        E =  E .+ F   {) +}
        T =  ( E .)   {$ * +}
  10 => F =  F * T .  {$ * +}
  11 => T =  ( E ) .  {$ * +}
  12 =>
        E =  E .+ F   {) +}
        T =  ( E .)   {) * +}
  13 =>
        E =  F .      {) +}
        F =  F .* T   {) * +}
  14 =>
        E =  E + F .  {) +}
        F =  F .* T   {) * +}
  15 => F =  T .      {) * +}
  16 => T =  number . {) * +}
  17 => T =  ( .E )   {) * +}
  18 => E =  E + .F   {) +}
  19 => F =  F * .T   {) * +}
  20 => F =  F * T .  {) * +}
  21 => T =  ( E ) .  {) * +}
  ```

#### Таблица разбора

Таблицы разбора, для номеров состояний.

```
  0  => E      GOTO   1
        F      GOTO   2
        T      GOTO   4
        (      Shift  6
        number Shift  5

  1  => $      ACCEPT
        +      Shift  7

  2  => $      Reduce E = F
        *      Shift  8
        +      Reduce E = F

  3  => $      Reduce E = E + F
        *      Shift  8
        +      Reduce E = E + F

  4  => $      Reduce F = T
        *      Reduce F = T
        +      Reduce F = T

  5  => $      Reduce T = number
        *      Reduce T = number
        +      Reduce T = number

  6  => E      GOTO   9
        F      GOTO   13
        T      GOTO   15
        (      Shift  17
        number Shift  16

  7  => F      GOTO   3
        T      GOTO   4
        (      Shift  6
        number Shift  5

  8  => T      GOTO   10
        (      Shift  6
        number Shift  5

  9  => )      Shift  11
        +      Shift  18

  10 => $      Reduce F = F * T
        *      Reduce F = F * T
        +      Reduce F = F * T

  11 => $      Reduce T = ( E )
        *      Reduce T = ( E )
        +      Reduce T = ( E )

  12 => )      Shift  21
        +      Shift  18

  13 => )      Reduce E = F
        *      Shift  19
        +      Reduce E = F

  14 => )      Reduce E = E + F
        *      Shift  19
        +      Reduce E = E + F

  15 => )      Reduce F = T
        *      Reduce F = T
        +      Reduce F = T

  16 => )      Reduce T = number
        *      Reduce T = number
        +      Reduce T = number

  17 => E      GOTO   12
        F      GOTO   13
        T      GOTO   15
        (      Shift  17
        number Shift  16

  18 => F      GOTO   14
        T      GOTO   15
        (      Shift  17
        number Shift  16

  19 => T      GOTO   20
        (      Shift  17
        number Shift  16

  20 => )      Reduce F = F * T
        *      Reduce F = F * T
        +      Reduce F = F * T

  21 => )      Reduce T = ( E )
        *      Reduce T = ( E )
        +      Reduce T = ( E )
```

### Состояния автомата

Присвоим всем состояниям номера: `STATES[Int] = State`.

Нам нужно описать тип состояния так, чтобы он был параметризован списком типов объектов, которые к этому моменту уже были разобраны.

```haskell
data St (a :: [*]) where
```

С заголовком типа мы справились.

Теперь, для каждого состояния, мы берём самую длинную последовательность разобранных типов.

Каждому состоянию будет сопоставлен конструктор типа `St`, параметризованный определённым образом.

Например для состояния, которое у меня имеет номер 9:
```haskell
  -- 9 =>
  --   E = E .+ F {) +}
  --   T = ( E .) {$ * +}
  S9 :: St (Expr : Open : a)
```
Самый длинный стек это `['(', Expr]`.
Для тех, кто не знаком с синтаксисом списков в хаскелле, `Expr : Open : a` соответствует `[Expr, Open, ...a]` из менее удачных языков программирования.

Вся конструкция целиком означает "объявить конструктор `S9` без параметров, создающий значение типа `St` параметризованного списком типов `[Expr, Open, ...a]`, где `a` это любой список типов".

То есть, `S9` накладывает ограничения на стек -- верхним типом должно быть `Expr`, далее `Open`, а потом что угодно.

#### Реализация в хаскелле

```haskell
data St :: [*] -> * where
  S0  :: forall a. St  a
  S1  :: forall a. St (Expr   : a)
  S2  :: forall a. St (Factor : a)
  S3  :: forall a. St (Factor : Plus  : Expr   : a)
  S4  :: forall a. St (Term   : a)
  S5  :: forall a. St (Int    : a)
  S6  :: forall a. St (Open   : a)
  S7  :: forall a. St (Plus   : Expr   : a)
  S8  :: forall a. St (Mult   : Factor : a)
  S9  :: forall a. St (Expr   : Open   : a)
  S10 :: forall a. St (Term   : Mult   : Factor : a)
  S11 :: forall a. St (Close  : Expr   : Open   : a)
  S12 :: forall a. St (Expr   : Open   : a)
  S13 :: forall a. St (Factor : a)
  S14 :: forall a. St (Factor : Plus   : Expr   : a)
  S15 :: forall a. St (Term   : a)
  S16 :: forall a. St (Int    : a)
  S17 :: forall a. St (Open   : a)
  S18 :: forall a. St (Plus   : Expr   : a)
  S19 :: forall a. St (Mult   : Factor : a)
  S20 :: forall a. St (Term   : Mult   : Factor : a)
  S21 :: forall a. St (Close  : Expr   : Open   : a)
```

### Доменные типы

Для демонстрации, мы объявим по типу на каждую не-начальную сущность, и сделаем так, что
конструкция этих типов повторяет структуру разбора.

```haskell
data Term
  = Number Int
  | Group  Expr

data Expr
  = Expr :+ Factor
  | Factor  Factor

data Factor
  = Factor :* Term
  | Term Term
```

### Стек автомата разбора

У нас должно храниться не одно состояние, а целый стек -- вместе со стеком разобранного.

```haskell
data Stack' xs where
  Nil  ::                  Stack' '[]
  (:>) :: x -> Stack xs -> Stack' (x : xs)

type Stack a = (St a, Stack' a)
```
Здесь мы объявляем тип `Stack` как пару из состояния типа `St` и стека `Stack'`, аргументы которых согласованы.

Мы так же объявляем тип `Stack'` который
1) либо пуст
   ```haskell
   Nil :: Stack' '[]
   ```
2) либо дополняет какой-то `Stack` значением (но не состоянием!)
   ```haskell
   (:>) x -> Stack xs -> Stack' (x : xs)
   ```
   Конструктор этого варианта является оператором.
   Если
   ```haskell
   x  :: A
   xs :: Stack XS
   ```
   то
   ```haskell
   x :> xs :: Stack (A : XS)
   ```

_Если вы неудачно моргнули, то `Stack` и `Stack'` это разные типы!_

Нам понадобится `Stack` для goto-функций, реализующих GOTO- и ACTION-таблицы и
`Stack'` в качестве вспомогательного типа.

То есть, у нас может быть стек разбора
```haskell
stack :: Stack (Factor : Plus : Expr : SomeList)
stack = (s0, f :> (s1, Plus :> (s2, e :> stk)))
```
где
```haskell
s0  :: St (Factor : Plus : Expr : SomeList)
s1  :: St          (Plus : Expr : SomeList)
s2  :: St                 (Expr : SomeList)
stk :: Stack                      SomeList
e   :: Expr
f   :: Factor
```

Кроме того, для удобства мы добавим паттерн для разбора стека `Stack`.
```
pattern (:?) :: x -> Stack xs -> Stack (x : xs)
pattern x :? xs <- (_, x :> xs)

infixr 5 :>, :?
```

### Конструкция автомата разбора

Автомат разбора является слегка видоизменённой версией автомата из [вот этой статьи](http://cristal.inria.fr/~fpottier/publis/fpottier-regis-gianas-typed-lr.pdf).

В нашем случае реализация таблицы ACTION он состоит из функции

```haskell
run :: Stack a -> [T] -> Expr
```

Реализацией таблицы GOTO является набор функций
```haskell
gotoExpr   :: [T] -> Expr   -> Stack a -> Expr
gotoFactor :: [T] -> Factor -> Stack a -> Expr
gotoTerm   :: [T] -> Term   -> Stack a -> Expr
```

#### Реализация GOTO-функций

Рассмотрим на примере `GOTO[-, Expr]`.

Мы сразу запрашиваем у стека его верхнее состояние:

```haskell
gotoExpr :: [T] -> Expr -> Stack a -> Expr
gotoExpr toks expr stk@(state, _) = case state of
  ...
```

Если посмотреть по [таблице разбора](#таблица-разбора), то состояний, в которых ожидается `E`, всего три:
```
  0  => E GOTO 1
  6  => E GOTO 9
  17 => E GOTO 12
```

Вот эти состояния:
```
  0  => S = .E        {$}

  1  => S =  E .      {$}
        E =  E .+ F   {$ +}

  ---------------------------

  6  => T =  ( .E )   {$ * +}

  9  => E =  E .+ F   {) +}
        T =  ( E .)   {$ * +}

  ---------------------------

  17 => T =  ( .E )   {) * +}

  12 => E =  E .+ F   {) +}
        T =  ( E .)   {) * +}
```
В каждой паре `E` сначала находится в локусе, а потом становится разобрано.

Нам необходимо перезапустить `run` в нужном состоянии, положив переданный нам `Expr` на стек.

```haskell
gotoExpr :: [T] -> Expr -> Stack a -> Expr
gotoExpr toks expr stk@(state, _) = case state of
  S0  -> run (S1 , expr :> stk) toks
  S6  -> run (S9 , expr :> stk) toks
  S17 -> run (S12, expr :> stk) toks
  _   -> error ""
```

В оригинальной статье вводятся меры для того, чтобы избежать клаузы `_ -> error ""`, но мы её
оставим для простоты.

Заметьте, что в
```haskell
gotoExpr toks expr stk@(state, _) = case state of
  S0  -> run (S1 , expr :> stk) toks
```

Состояние `S0` остаётся на стеке! Мы не заменяем его, а добавляем новую пару состояние/сущность.

Точно так же распишем и остальные сущности:
```haskell
gotoFactor :: [T] -> Factor -> Stack a -> Expr
gotoFactor toks factor stk@(state, _) = case state of
  S0  -> run (S2 , factor :> stk) toks
  S6  -> run (S13, factor :> stk) toks
  S7  -> run (S3 , factor :> stk) toks
  S17 -> run (S13, factor :> stk) toks
  S18 -> run (S14, factor :> stk) toks
  _   -> error ""

gotoTerm :: [T] -> Term -> Stack a -> Expr
gotoTerm toks term stk@(state, _) = case state of
  S0  -> run (S4 , term :> stk) toks
  S6  -> run (S15, term :> stk) toks
  S7  -> run (S4 , term :> stk) toks
  S8  -> run (S10, term :> stk) toks
  S17 -> run (S15, term :> stk) toks
  S18 -> run (S15, term :> stk) toks
  S19 -> run (S20, term :> stk) toks
  _   -> error ""
```

#### Реализация ACTION-функций

На надо расписать клаузы для функции
```haskell
run :: Stack a -> [T] -> Expr
run = \cases
  ...
```

##### ACCEPT

Самый простой случай это `(S1, '$') => ACCEPT`:

```haskell
run :: Stack a -> [T] -> Expr
run = \cases
  (S1, e :> _) [] -> e
  ...
```
В состоянии `S1` при пустом потоке лексем мы возвращаем верхнее выражени со стека.
Тот факт, что `S1 :: St (Expr : a)` и то, что типы состояния со стеком согласованы, позволяет нам это делать безо всяких динамических проверок и приведения типов.

##### SHIFT

Теперь рассмотрим какое-нибудь действие Shift, например в состоянии
```
  12 => E =  E .+ F   {) +}
```
Следующей лексемой является `+` и мы выполняем
```
  (S12, '+') => Shift 18
```

Для этого мы добавляем клаузу
```haskell
run :: Stack a -> [T] -> Expr
run = \cases
  ...
  (S12, stk) (TPlus : toks) -> run (S18, Plus :> (S12, stk)) toks
  ...
```
В ней мы убираем `+` из потока лексем (`... (TPlus : toks) -> .. toks`)
и кладём его значение на стек разбора, одновременно переходя из состояния 12 в 18
(`(S12, stk) ... -> run (S18, Plus :> (S12, stk))`).
При этом состояние `S12` остаётся на стеке, но глубже.

##### REDUCE

Рассмотрим какое-нибудь действие свёртки, например
```
  (20, ')') => Reduce [F = F * T]
```

Для того, чтобы его выполнить, нам нужно снять со стека значения для `T`, `*` и `F` и свернуть из в `F`. Снимать значения му будем _вместе с состояниями_, поэтому состояние после снятия этой троицы будет нам неизвестно.

Но мы точно знаем, что функция `gotoFactor` способна сопоставить этому состоянию другое, в котором его позиции перешагнули через `F`. Поэтому мы делаем следующее:

```haskell
run :: Stack a -> [T] -> Expr
run = \cases
  ...
  (S20, t :> (_, Mult :> (_, f :> stk))) (TClose : toks) ->
    gotoFactor (TClose : toks) (f :* t) stk
  ...
```
Из-за мешанины операторов может быть сложно отследить, что происходит.
1) Лексема `)` остаётся в потоке лексем `... (TClose : toks) -> ... (TClose : toks)`
2) Мы снимаем со стека три состояния и три значения:
   ```haskell

   --      +--------+-----------+--- снимаемые значения
   --      |        |           |
   --      v        v           v
     (S20, t :> (_, Mult :> (_, f :> stk))) ... ->
                                     stk
   -- ^          ^           ^
   -- |          |           |
   -- +----------+-----------+--- снимаемые состояния
   ```
3) Верхнее состояние используется для выбора пути; все три состояния пропадают со стека.
4) Мы просим `gotoFactor` для верхнего состояния _стека `stk`_ (назовём его `S`) подобрать такое `T`, что `(S, Factor) => T` есть в таблице GOTO.

То есть, в `stk` есть какое-то состояние, ожидающее `Factor`, например
```
  18 => F      GOTO   14
        T      GOTO   15
        (      Shift  17
        number Shift  16
```
где
```
  18 => E =  E + .F   {) +}
```
мы кладём `Factor` на стек `stk` (который всё ещё `S18`!) и добавляем `S14`:
```haskell
gotoFactor :: [T] -> Factor -> Stack a -> Expr
gotoFactor toks factor stk@(state, _) = case state of
  ...
  S18 -> run (S14, factor :> stk) toks
  ...
```

### ACTION-автомат грамматики-примера

Помимо описанного выше, мы так же докладываем об ошибочных лексемах.

```haskell
{- |
  Parsing procedure, contains table ACTION.
-}
run :: Stack a -> [T] -> Expr
run = \cases
  ---- SHIFT/REDUCE -----------------------------------------------------------

  (S1, e :> _) [] -> e

  {-
    Shift actions. They consume token and push its value onto stack with new state
    (s, x, xs) -> (s1, value, (s, x, xs))
  -}
  (S0 , stk) (TOpen     : toks) -> run (S6 , Open  :> (S0 , stk)) toks
  (S0 , stk) (TNumber n : toks) -> run (S5 , n     :> (S0 , stk)) toks
  (S1 , stk) (TPlus     : toks) -> run (S7 , Plus  :> (S1 , stk)) toks
  (S2 , stk) (TMult     : toks) -> run (S8 , Mult  :> (S2 , stk)) toks
  (S3 , stk) (TMult     : toks) -> run (S8 , Mult  :> (S3 , stk)) toks
  (S6 , stk) (TOpen     : toks) -> run (S17, Open  :> (S6 , stk)) toks
  (S6 , stk) (TNumber n : toks) -> run (S16, n     :> (S6 , stk)) toks
  (S7 , stk) (TOpen     : toks) -> run (S6 , Open  :> (S7 , stk)) toks
  (S7 , stk) (TNumber n : toks) -> run (S5 , n     :> (S7 , stk)) toks
  (S8 , stk) (TOpen     : toks) -> run (S6 , Open  :> (S8 , stk)) toks
  (S8 , stk) (TNumber n : toks) -> run (S5 , n     :> (S8 , stk)) toks
  (S9 , stk) (TPlus     : toks) -> run (S18, Plus  :> (S9 , stk)) toks
  (S9 , stk) (TClose    : toks) -> run (S11, Close :> (S9 , stk)) toks
  (S12, stk) (TPlus     : toks) -> run (S18, Plus  :> (S12, stk)) toks
  (S12, stk) (TClose    : toks) -> run (S21, Close :> (S12, stk)) toks
  (S13, stk) (TMult     : toks) -> run (S19, Mult  :> (S13, stk)) toks
  (S14, stk) (TMult     : toks) -> run (S19, Mult  :> (S14, stk)) toks
  (S17, stk) (TNumber n : toks) -> run (S16, n     :> (S17, stk)) toks
  (S17, stk) (TOpen     : toks) -> run (S17, Open  :> (S17, stk)) toks
  (S18, stk) (TNumber n : toks) -> run (S16, n     :> (S18, stk)) toks
  (S18, stk) (TOpen     : toks) -> run (S17, Open  :> (S18, stk)) toks
  (S19, stk) (TNumber n : toks) -> run (S16, n     :> (S19, stk)) toks
  (S19, stk) (TOpen     : toks) -> run (S17, Open  :> (S19, stk)) toks

  {-
    Reduce actions. They grab some information from stack and apply some reducer
    (_, f, (_, Plus, (_, e, (s, _, _)))) -> (s1, e :+ f, (s, _, _))
    where s1 is determined by GOTO table from state of s.
  -}
  (S2 ,                  f    :> stk) []                 -> gotoExpr    []              (Factor f) stk
  (S2 ,                  f    :> stk) (TPlus     : toks) -> gotoExpr    (TPlus  : toks) (Factor f) stk
  (S3 , f     :> Plus :? e    :? stk) []                 -> gotoExpr    []              (e :+ f)   stk
  (S3 , f     :> Plus :? e    :? stk) (TPlus     : toks) -> gotoExpr    toks            (e :+ f)   stk
  (S4 ,                  t    :> stk) []                 -> gotoFactor  []              (Term t)   stk
  (S4 ,                  t    :> stk) (TPlus     : toks) -> gotoFactor  (TPlus  : toks) (Term t)   stk
  (S4 ,                  t    :> stk) (TMult     : toks) -> gotoFactor  (TMult  : toks) (Term t)   stk
  (S5 ,                  i    :> stk) []                 -> gotoTerm    []              (Number i) stk
  (S5 ,                  i    :> stk) (TPlus     : toks) -> gotoTerm    (TPlus  : toks) (Number i) stk
  (S5 ,                  i    :> stk) (TMult     : toks) -> gotoTerm    (TMult  : toks) (Number i) stk
  (S10, t     :> Mult :? f    :? stk) []                 -> gotoFactor  []              (f :* t)   stk
  (S10, t     :> Mult :? f    :? stk) (TPlus     : toks) -> gotoFactor  (TPlus  : toks) (f :* t)   stk
  (S10, t     :> Mult :? f    :? stk) (TMult     : toks) -> gotoFactor  (TMult  : toks) (f :* t)   stk
  (S11, Close :> e    :? Open :? stk) []                 -> gotoTerm    []              (Group e)  stk
  (S11, Close :> e    :? Open :? stk) (TPlus     : toks) -> gotoTerm    (TPlus  : toks) (Group e)  stk
  (S11, Close :> e    :? Open :? stk) (TMult     : toks) -> gotoTerm    (TMult  : toks) (Group e)  stk
  (S13,                  f    :> stk) (TClose    : toks) -> gotoExpr    (TClose : toks) (Factor f) stk
  (S13,                  f    :> stk) (TPlus     : toks) -> gotoExpr    (TPlus  : toks) (Factor f) stk
  (S14, f     :> Plus :? e    :? stk) (TClose    : toks) -> gotoExpr    (TClose : toks) (e :+ f)   stk
  (S14, f     :> Plus :? e    :? stk) (TPlus     : toks) -> gotoExpr    (TPlus  : toks) (e :+ f)   stk
  (S15,                  t    :> stk) (TClose    : toks) -> gotoFactor  (TClose : toks) (Term t)   stk
  (S15,                  t    :> stk) (TPlus     : toks) -> gotoFactor  (TPlus  : toks) (Term t)   stk
  (S15,                  t    :> stk) (TMult     : toks) -> gotoFactor  (TMult  : toks) (Term t)   stk
  (S16,                  i    :> stk) (TClose    : toks) -> gotoTerm    (TClose : toks) (Number i) stk
  (S16,                  i    :> stk) (TPlus     : toks) -> gotoTerm    (TPlus  : toks) (Number i) stk
  (S16,                  i    :> stk) (TMult     : toks) -> gotoTerm    (TMult  : toks) (Number i) stk
  (S20, t     :> Mult :? f    :? stk) (TClose    : toks) -> gotoFactor  (TClose : toks) (f :* t)   stk
  (S20, t     :> Mult :? f    :? stk) (TPlus     : toks) -> gotoFactor  (TPlus  : toks) (f :* t)   stk
  (S20, t     :> Mult :? f    :? stk) (TMult     : toks) -> gotoFactor  (TMult  : toks) (f :* t)   stk
  (S21, Close :> e    :? Open :? stk) (TClose    : toks) -> gotoTerm    (TClose : toks) (Group e)  stk
  (S21, Close :> e    :? Open :? stk) (TPlus     : toks) -> gotoTerm    (TPlus  : toks) (Group e)  stk
  (S21, Close :> e    :? Open :? stk) (TMult     : toks) -> gotoTerm    (TMult  : toks) (Group e)  stk

  (S0 , _) toks -> error $ show S0  <> " expected '(' or number, got " <> showToks toks
  (S1 , _) toks -> error $ show S1  <> " expected <end-of-file> or '+', got " <> showToks toks
  (S2 , _) toks -> error $ show S2  <> " expected <end-of-file>, '*' or '+', got " <> showToks toks
  (S3 , _) toks -> error $ show S3  <> " expected <end-of-file>, '*' or '+', got " <> showToks toks
  (S4 , _) toks -> error $ show S4  <> " expected <end-of-file>, '*' or '+', got " <> showToks toks
  (S5 , _) toks -> error $ show S5  <> " expected <end-of-file>, '*' or '+', got " <> showToks toks
  (S6 , _) toks -> error $ show S6  <> " expected '(' or number, got " <> showToks toks
  (S7 , _) toks -> error $ show S7  <> " expected '(' or number, got " <> showToks toks
  (S8 , _) toks -> error $ show S8  <> " expected '(' or number, got " <> showToks toks
  (S9 , _) toks -> error $ show S9  <> " expected <end-of-file> or '+', got " <> showToks toks
  (S10, _) toks -> error $ show S10 <> " expected <end-of-file>, '*' or '+', got " <> showToks toks
  (S11, _) toks -> error $ show S11 <> " expected <end-of-file>, '*' or '+', got " <> showToks toks
  (S12, _) toks -> error $ show S12 <> " expected ')' or '+', got " <> showToks toks
  (S13, _) toks -> error $ show S13 <> " expected ')', '*' or '+', got " <> showToks toks
  (S14, _) toks -> error $ show S14 <> " expected ')', '*' or '+', got " <> showToks toks
  (S15, _) toks -> error $ show S15 <> " expected ')', '*' or '+', got " <> showToks toks
  (S16, _) toks -> error $ show S16 <> " expected ')', '*' or '+', got " <> showToks toks
  (S17, _) toks -> error $ show S17 <> " expected '(' or number, got " <> showToks toks
  (S18, _) toks -> error $ show S18 <> " expected '(' or number, got " <> showToks toks
  (S19, _) toks -> error $ show S19 <> " expected '(' or number, got " <> showToks toks
  (S20, _) toks -> error $ show S20 <> " expected <end-of-file>, '*' or '+', got " <> showToks toks
  (S21, _) toks -> error $ show S21 <> " expected <end-of-file>, '*' or '+', got " <> showToks toks
```

### Запуск

Для запуска парсера нам нужно две вещи
1) процедуры распечатки сущностей
   ```haskell
   instance Show Term where
     show = \case
       Number n -> show n
       Group  e -> "(" <> show e <> ")"

   instance Show Expr where
     show = \case
       e :+ f   -> show e <> " + " <> show f
       Factor f -> show f

   instance Show Factor where
     show = \case
       f :* t -> show f <> " * " <> show t
       Term t -> show t
   ```
2) Тестовый пример
   ```haskell
   example :: Expr
   example = run (S0, Nil)
     [ TNumber 1  -- 1
     , TMult      -- *
     , TOpen      -- (
     , TNumber 2  -- 2
     , TPlus      -- +
     , TNumber 4  -- 4
     , TClose     -- )
     , TMult      -- *
     , TNumber 3  -- 3
     ]            -- $
   ```

В результате получаем `1 * (2 + 4) * 3`.

## Список литературы

1) http://cristal.inria.fr/~fpottier/publis/fpottier-regis-gianas-typed-lr.pdf
