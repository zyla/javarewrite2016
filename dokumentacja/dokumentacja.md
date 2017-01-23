---
title: "JL.W1: Inspekcja kodu Java"
author:
- Maciej Bielecki
- Przemysław Kopański
- Mateusz Forc
---

## Zadanie

_(cytat z <https://studia.elka.pw.edu.pl/file/16Z/PSZT.A/priv/PSZT-projekt-16Z.pdf>)_

Napisać program, który wykorzystując jeden z dostępnych parserów kodu Java
wykona inspekcje kodu i wykona przekształcenia upraszczające kod. Program ma
przekształcać zadany plik Java wykorzystując zdefiniowane reguły wnioskowania,
np:

```java
System.println(“To jest” + “ “ + “kot”);
System.println(“który jest rudy”);
```

Reguły:

```java
String($a) + String($b) -> String($a$b)
println($a); println($b); -> String($a\n$b)
```

Wynik:

```java
System.println(“To jest kot\nktóry jest rudy”);
```

## Instrukcja obsługi

Aplikacja jest napisana przy użyciu języka Haskell, zatem do jej uruchomienia
potrzebny jest program Stack dodany do zmiennej środowiskowej `PATH`.

Aby zbudować projekt wykorzystując narzędzie Stack należy w katalogu projektu
uruchomić:
```bash
$ stack setup     # jeśli nie ma w systemie zainstalowanego GHC 8.0.1
$ stack build
```
Aby uruchomić projekt, należy po zbudowaniu:

```bash
$ stack exec javarewrite2016 -- rules.txt test.java

```
gdzie

- `rules.txt` - plik zawierający reguły przekształcania kodu zapisane zgodnie
  ze składnią omówioną poniżej w sekcji Reguły  
- `test.java` - Plik źródłowy w języku Java, który zostanie poddany przekształceniom

## Reguły

Reguła składa się ze _zbioru metazmiennych_, _wyrażenia wzorca_ (dalej: LHS) i _wyrażenia
podstawianego_ (dalej RHS), w następującej składni:

```
forall a b c. lhs -> rhs
```

Składnia jest zainspirowana funkcją `RULES` kompilatora GHC[^ghc-rules].

Zbiór metazmiennych jest to zbiór identyfikatorów, które w RHS mają być
traktowane jako metazmienne (zamiast rozróżnienia składniowego typu poprzedzenie
identyfikatora znakiem dolara).

Przykład:

```java
forall a. a + b -> b + a
```

`a` jest tutaj metazmienną, natomiast `b` jest zmienną języka Java. Reguła
pasuje do wyrażenia `3 * 5 + b`, dając w wyniku `b + 3 * 5`, ale nie do
`a + 17`.

Metazmienne mogą dodatkowo mieć określony _typ wyrażenia_ jaki akceptują. W
obecnej wersji programu są to różne rodzaje literałów.

Na przykład wzorzec:

```java
forall (s : StringLiteral). s
```

pasuje do wyrażenia `"EiTI"`, ale nie do `17` lub `new Object().toString()`.

[^ghc-rules]: "Playing by the rules: rewriting as a practical optimisation
  technique in GHC". Simon Peyton Jones, Andrew Tolmach and Tony Hoare; Haskell
  workshop 2001.
  <http://research.microsoft.com/en-us/um/people/simonpj/Papers/rules.htm>

### Kolejność stosowania reguł

Reguły są stosowane w następującej kolejności (do każdego wyrażenia w pliku które nie jest podwyrażeniem innego wyrażenia):

1. Dopóki któraś z reguł pasuje, jest ona aplikowana do wyrażenia po czym wykonywany jest constant folding.
2. Procedura jest aplikowana do każdego podwyrażenia w wyniku powyższej transformacji.

Powyższa procedura jest powtarzana aż żadna z reguł nie zostanie zaaplikowana w pełnym przebiegu.


### Constant folding
Po wykonaniu każdej reguły każde wyrażenie w postaci `constant_fold(...)` jest wyliczane.
`constant_fold(e)` jest zamieniane na wynik operacji wyrażenia _e_, jeśli składa się tylko ze stałych operacji arytmetycznych i łączenia stringów, np:  

```
forall (a : StringLiteral) (b : StringLiteral).
  a + b -> constant_fold(a + b)
```


### Semantyka reguł

Podstawową operacją wykonywaną na wzorcach jest _dopasowanie wzorca do
wyrażenia_. Jej wynikiem jest _podstawienie_ - przypisanie metazmiennym wzorca
konkretnych wyrażeń, lub porażka w przypadku gdy wyrażenie nie pasuje do wzorca.
Jest to szczególny przypadek _unifikacji_, gdzie tylko jedna strona zawiera
metazmienne.

_Dopasowanie wzorca *e* do wyrażenia *p*_ jest zdefiniowane następująco:

- Jeśli _p_ jest metazmienną wzorca bez określonego typu i _e_ jest wyrażeniem,
  _e_ pasuje do _p_ z podstawieniem `p ~> e`.

- Jeśli _p_ jest metazmienną wzorca o typie wyrażenia _t_ i _e_ jest wyrażeniem
  pasującym do typu _t_, _e_ pasuje do _p_ z podstawieniem `p ~> e`.

- Jeśli _p_ i _e_ są wyrażeniami tego samego rodzaju, ich _części stałe_
  są równe, a każde z podwyrażeń _e_ pasuje do kolejnych podwyrażeń _p_ i
  wszystkie podstawienia wynikające z dopasowania podwyrażeń zgadzają się ze
  sobą, _e_ pasuje do _p_ z podstawieniem równym sumie podstawień podwyrażeń.

- W przeciwnym przypadku _e_ nie pasuje do _p_.

Podstawienia _zgadzają się ze sobą_, jeśli dla każdej wspólnej metazmiennej
wyrażenia, które przypisują tej metazmiennej są sobie równe.

Z powyższej semantyki wynika, że obsługiwane są wzorce _nieliniowe_, czyli
takie, w których metazmienna może występować więcej niż jeden raz. Przy
dopasowywaniu wszystkie wystąpienia metazmiennej muszą zostać dopasowane do
takiego samego wyrażenia.

Kolejną ważną operacją jest _zastosowanie podstawienia do wyrażenia_ - zamiana
wszystkich wystąpień metazmiennych na odpowiadające im wyrażenia.

_Zastosowanie reguły do wyrażenia_ polega na dopasowaniu wzorca reguły do tego
wyrażenia i zastosowania wynikowego podstawienia do RHS reguły w przypadku
sukcesu.

### Gramatyka języka reguł

```
Rules:
  [ Rule { ";;" Rule } ]

Rule:
  Pattern "->" Expression

Pattern:
  [ "forall" Metavariables "." ] Expression

Metavariables:
  Metavariable { Metavariable }

Metavariable:
  Identifier
  "(" Identifier ":" ExpressionType ")"

ExpressionType:
  "IntLiteral"
  "WordLiteral"
  "FloatLiteral"
  "DoubleLiteral"
  "BooleanLiteral"
  "CharLiteral"
  "StringLiteral"
  "NullLiteral"
```

Konwencja zapisu gramatyki, jak i symbole nieterminalne `Expression` i `Identifier`
są zdefiniowane w _Java Language Specification, 2. Grammars_[^jls-grammars], z
tą różnicą, że terminale są zawarte w znakach "`"`".

[^jls-grammars]: <https://docs.oracle.com/javase/specs/jls/se8/html/jls-2.html>

## Implementacja

Program został zaimplementowany w języku [Haskell][].

Do konwersji pomiędzy tekstem a AST Javy została wykorzystana biblioteka
`language-java`, dostępna na repozytorium Hackage[^language-java]. Wykorzystanie w tej
bibliotece parserów utworzonych za pomocą kombinatorów (_parser combinators_)
pozwoliło łatwo zdefiniować parser reguł używając fragmentów gramatyki języka
Java (moduł `JavaRewrite.RuleParser`). Biblioteka nie jest jednak w pełni
dopracowana, więc w celu jej użycia było konieczne poprawienie niektórych
defektów[^bug-ClassFieldAccess][^bug-precedence][^bug-QualInstanceCreation].

### Testowanie programu

Do najważniejszych funkcji programu zostały napisane testy jednostkowe (_test/Spec.hs_). Niektóre właściwości zostały sprawdzone za pomocą _QuickCheck_.

[haskell]: https://www.haskell.org/

[^language-java]: <https://hackage.haskell.org/package/language-java>

[^bug-ClassFieldAccess]: `Fix pretty-printing for ClassFieldAccess`
  <https://github.com/zyla/language-java/commit/98a8c092b900bed665c5e932a8dec4c8b63024e7>
[^bug-precedence]: `Parser: fix operator precedence`
  <https://github.com/zyla/language-java/commit/2369019638881d8ea4c564cd1f3eb3b41149769b>
[^bug-QualInstanceCreation]: `Make ExpName a primary expression`
  <https://github.com/zyla/language-java/commit/8baf42b35aa918c6102515c60bf0d1dde77eb90f>



## Struktura działania programu
  
Klasycznie start działania programu zaczyna się od funkcji Main.  
Pierwszym krokiem działania programu jest wczytanie plików biorących udział
w działaniu algorytmu :  
1. Plik źródłowy Java  
2. Plik zawierający reguły przekształceń kodu  

Następnie rozpoczyna pracę głowny algorytm zajmujący się przetwarzaniem kodu.  

Przekształcenie kodu przebiega z użyciem rozpoznawania wzorców w kodzie na podstawie
prawie wszystkich elementów języka (nie wszystkie rodzaje wyrażeń są obsługiwane).

## Organizacja plików

_main/Main.hs_ - W tym pliku znajduje się główne ciało programu wywołujące funkcje
odpowiedzialne za parsowanie danych wejściowych  
_JavaRewrite/ApplySubst.hs_ - Odnajdziemy tutaj funkcję odpowiedzialną za zamianę odpowiednich wyrażeń  
_JavaRewrite/ConstantFold.hs_ - Moduł odpowiedzialny za obliczanie stałych wyrażeń (jak np. 2 + 3)  
_JavaRewrite/Match.hs_ - Zawiera definicje funkcji match oraz matchPattern  
_JavaRewrite/MatchResult.hs_ - Moduł zawierający abstrakcje do przechowywania wyniku po sprawdzeniu, czy dany szablon(pattern) pasuje to wyrażenia Javowego  
_JavaRewrite/Rewrite.hs_ - Odnajdziemy tutaj funkcje stosowane podczas przepisywania kodu np. applyRulesTopDown  
_JavaRewrite/RuleParser.hs_ - Plik zawierający funkcje odpowiedizalne za parsowane regułek  
_JavaRewrite/Rulse.hs_ - Zawiera definicje typów algebraicznych Pettern oraz Rule  
_JavaRewrite/Syntax.hs_ - Znajduje się tu pare pomocniczych funkcji do pracy ze składnią Javową  
_JavaRewrite/Traversals.hs_ - Moduł zawierający funkcje umożliwiające przechodzenie przez AST i aplikowanie przekształceń na znajdujących się w drzewie wyrażeń  

## Wnioski dotyczące osiągniętych rezultatów

W wyniku naszego projektu, jesteśmy w stanie przekształcać kod na poziomie reguł dotyczących wyrażeń (Expression) w języku Java. Składa się na to:

1. Wyrażenia będące nazwami klas, interfejsów, prymitywów, itp.
2. Post/pre inkrementacja
3. Post/pre dekrementacja
4. Operacje dodawania i odejmowania
5. Rzutowania
6. Operacje bitowe
7. Operator `instanceof`
8. Instrukcje warunkowe
9. Referencje do metod
10. Obliczanie wyrażen na podstawie stałych znanych w czasie kompilacji (constant folding)

Niestety w Javie można wyrazić niewiele użytecznych reguł za pomocą tego języka. Istniała wręcz obawa, że niniejszy program okaże się bezużyteczny. Okazało się jednak że może on zostać użyty jako bardzo niewygodny w użyciu i niezwykle mało wydajny funkcyjny język programawania. Przykładowo - _examples/bf.[txt|java]_ - implementuje interpreter języka Brainfuck.  
Dodanie do naszego programu funkcjonalności obsługi instrukcji (Statement) w sposób niewielki zwiększyłoby jego realną użyteczność. W celu optymalizacji należałoby wykroczyć poza mechanizm reguł - przez transformacje niemożliwe do wyrażenia za pomocą języka reguł (np. inlining, propagracja stałych, wyciąganie niezmienników pętli).

## Ograniczenia

Nie wszystkie elementy języka zostały pokryte  w naszym projekcie, w wyniku czego przeprowadzanie części przekształceń nie jest możliwe.  
Nie została zaimplementowana obsługa pełnych instrukcji (Statement) oraz niektóre rodzaje wyrażeń:

1. Przypisanie wyniku wyrażenia do zmiennej
2. Wyrażenia Lambda
3. Referencje do metod oraz wywołania metod
4. Odwołania do tablic oraz ich tworzenie

Nie została zaimplementowana również obsługa na poziomie całych bloków zawierających w sobie instrukcje oraz wyrażenia.  

Na autorze reguł spoczywa obowiązek sprawdzenia, czy zestaw reguł jest confluenty i zapewnia, że przekształcenia zakończą się.
Automatyczne sprawdzanie reguł jest niemożliwe:  
Dowód nie wprost:  
Załóżmy, że istnieje algorytm sprawdzający, czy dla danego zestawu reguł i kodu w Javie program się zakończy.  
Podajemy algortmowi program w brainfucku.  
On stwierdza, czy program się zakończy.  
W ten sposób uzyskaliśmy procedure sprawdzenia, czy dowolny program napisany w Brainfucku się zakończy. Język ten jest Turing-complete, więc taka procedura nie może istnieć => sprzeczność.  
