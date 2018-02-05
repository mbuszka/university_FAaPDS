# Zadanie 1
### Nieformalnie
Weźmy jakąś dużą liczbę np `2 ^ k` po czym wykonajmy naprzemiennie `n` operacji `dec`, `inc`

Wtedy realny koszt każdej z nich to `k` co daje sumaryczny koszt `2 * n * k` co nie może być zamortyzowane do `c * n` jako, że `k` jest zmienną

### Formalnie
Weźmy ciąg `O` operacji `2 ^ k` inc, a następnie `2 ^ k` `dec` oraz `inc`
Załóżmy, że inc, dec działają w zamortyzowanym czasie `c`

Wtedy koszt rzeczywisty `O` to `2 ^ (k + 1) + 2 ^ (k + 1) * k`, natomiast zamortyzowany to `2 ^ (k + 2) * c`

# Zadanie 2

```sml
    fun inc (Zero :: ds) = One :: ds
      | inc (One  :: ds) = Two :: ds
      | inc (Two  :: ds) = One :: inc ds
    
    fun dec [One] = []
      | dec (Zero :: ds) = One  :: dec ds
      | dec (One  :: ds) = Zero :: ds
      | dec (Two  :: ds) = One  :: ds
    
    fun isZero [] = true
      | isZero _  = false
```
# Zadanie 3

### Metoda bankiera
Ustalamy niezmiennik kredytowy.
Ustawiamy kredyty w różnych częściach struktury.
Każdy kredyt może zostać wysdany co najwyżej raz, po tym jak został postawiony.
Niech
  `Cr` - koszt rzeczywisty
  `Cz` - zdjęte kredyty
  `Cp` - postawione kredyty
  `Ca` - koszt zamortyzowany

Wtetdy `Ca` = `Cr - Cz + Cp`

Przyjmijmy niezmiennik `na każdym Zero oraz Two jest jeden kredyt`
Przyjmijmy koszty zamortyzowane `Cinc = 2`, `Cdec = 2`, `CisZero = 1` 


### Metoda fizyka
Ustalamy nieujemną funkcję potencjału `Φ`
`Ca = Cr + ΔΦ`

Niech `Φ = #Two + #Zero`

# Zadanie 4

  - Trwała struktura danych to taka, której każda operacja może mieć wiele logiczych przyszłości.
  - Struktura ulotna to taka, której każda operacja ma co najwyżej jedną logiczną przyszłość.

# Zadanie 5
### Metoda leniwego fizyka
Niech
  `Cu` - unshared cost - koszt wykonania operacji przy założeniu, że wszystkie odrocznenia istniejące wcześniej zostały już spamiętane
  `Cs` - shared cost - koszt utworzonych odroczeń
  `Φ`  - Nieujemna funkcja potencjału
  `Ca` - koszt zamortyzowany - `Ca = Cu + Cs -ΔΦ`

type Nat = Digit list susp



# Zadanie 6
  - Obliczenie monolityczne - takie które po wymuszeniu musi zostać obliczone w całości ( np `$rev xs`)
  - Obliczenie inkrementacyjne - takie które można wymuszac kawałkami - np (`xs ++ ys` - stream append)

### Metoda leniwego bankiera
Niech
  `Cu` - unshared cost - koszt wykonania operacji przy założeniu, że wszystkie odrocznenia istniejące wcześniej zostały już spamiętane
  `Cs` - shared cost - koszt utworzonych odroczeń
  `Cd` - spłacone debety
  `Cc` - `Cu + Cs`
  `Ca` - koszt zamortyzowany

  `Ca = Cu + Cd`

Niech
  `Cr` - realized shared cost - koszt wymuszonych odroczeń (po jakimś ciągu operacji)
  `Cur` - unrealized shared cost - koszt niewymuszonych
  `Cact` - actual total cost - `Cact = Sum(Cu) + Cr`

Chcemy aby `Sum(Ca_i) >= Cact`

Utworzenie odroczenia powoduje odłożenie tylu debetów ile kosztuje jego wykonanie w dane miejsce struktury danych.
Odroczenie może zostać wymuszone dopiero gdy wszystkie debety z nim związane zostały spłacone. 

### Amortyzacja Digit stream
Numerujemy od 0
`di` = ilość debetów na i-tej cyfrze
`Di` = `Sum(0, i, di)`

Niezmiennik `Di <=L(i)`
gdzie L(i) = liczba jedynek w prefiksie dł. i

# Zadanie 7

type Nat = (Digit * bool) Stream * (Digit * bool) Stream list

fun exec (job :: jobs) = case job of
      | $Cons((_, true), _) => jobs
      | $Cons((_, false, ds)) => ds :: jobs
      | $Nil => jobs
  | exec [] = []

fun inc (n, s) =
    let val m = inc1 n
    in (m, exec (exec (m :: s)))

fun inc1 ($Cons((Zero, _), ds)) = $Cons((One, true), ds)
  | inc1 ($Cons((One, _), ds))  = $Cons((Two, true), ds)
  | inc1 ($Cons((Two, _), ds))  = $Cons((One, false), inc1 ds)
  | inc1 $Nil                   = $Cone((One, true), $Nil)


Wykonujemy exec 2 razy co odpowiada spłaceniu 2 kredytów w schemacie amortyzacji. Flagi przy cyfrach mówią nam kiedy trzeba kontynuować wymuszanie.