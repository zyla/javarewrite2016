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

## Reguły

TODO: napisać bardziej po polsku

Postanowiliśmy zmodyfikować składnię reguł przedstawionych w przykładzie.

Pierwszą z modyfikacji jest zmiana sposobu rozróżniania odwołań do zmiennych
Javy of zmiennych języka reguł (dalej: metazmiennych). W składni użytej w tym
programie nie różnią się one składniowo; dla każdej reguły wymienione są
identyfikatory, które mają być traktowane jako metazmienne. Na przykład, w
regule

```java
forall a. a + b -> b + a
```

`a` jest metazmienną, natomiast `b` jest zmienną języka Java. Reguła pasuje do
wyrażenia `3 * 5 + b`, ale nie do `a + 17`.

### Gramatyka języka reguł

```
Rules:
  [ Rule { ;; Rule } ]

Rule:
  Pattern -> Expression

Pattern:
  [ forall Metavariables . ] Expression

Metavariables:
  Metavariable { Metavariable }

Metavariable:
  Identifier
```

Konwencja zapisu gramatyki, jak i nieterminale `Expression` i `Identifier` są
zdefiniowane w _Java Language Specification, 2. Grammars_[^jls-grammars].

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
