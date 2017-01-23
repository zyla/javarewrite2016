---
title: "JL.W1: Inspekcja kodu Java"
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

[haskell]: https://www.haskell.org/

[^language-java]: <https://hackage.haskell.org/package/language-java>

[^bug-ClassFieldAccess]: `Fix pretty-printing for ClassFieldAccess`
  <https://github.com/zyla/language-java/commit/98a8c092b900bed665c5e932a8dec4c8b63024e7>
[^bug-precedence]: `Parser: fix operator precedence`
  <https://github.com/zyla/language-java/commit/2369019638881d8ea4c564cd1f3eb3b41149769b>
[^bug-QualInstanceCreation]: `Make ExpName a primary expression`
  <https://github.com/zyla/language-java/commit/8baf42b35aa918c6102515c60bf0d1dde77eb90f>
