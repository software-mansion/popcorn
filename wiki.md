# Wiedza powszechna o Elixir WASM

### Jak patchować funkcje w fission_lib?
- Stwórz moduł w `fission_lib/patches/*`, jeśli jeszcze nie istnieje
- Dodaj tam nową implementację funkcji. Jeśli funkcja jest prywatna, to:
  - W Elixirze musi być zmieniona na publiczną, inaczej kompilator ją wyprunuje. Oprócz tego trzeba dodać przed nią `@compile {:flb_patch_private, name: arity}`
  - W erlangu wystarczy dodać `-compile({:flb_patch_private, name/arity})` (musi być w miarę na górze pliku, przed implementacjami funkcji), ale jak nie zrobimy jej publicznej (`-export([name/arity])`) to linter się będzie pluł
- Jeśli funkcja woła inne funkcje (prywatne lub publiczne) z tego samego modułu, które nie są w pliku z patchem i nie musisz ich patchować, to nie kopiuj ich, tylko zamień ich wywołania na `foo() -> :flb_module.foo()` (w Erlangu `flb_module:foo()`).
- Przy każdej zmianie, którą wprowadzasz względem oryginalnej funkcji,, dodaj komentarz zaczynający się od `Patch reason: `, z opisem dlaczego ta funkcja została spatchowana.

### Jak działa GC w AtomVM?
Poniższy opis nie zawiera wszystkich szczegółów i czasem GC może działać inaczej ¯\_(ツ)_/¯ więcej szczegółów tutaj: https://www.atomvm.net/doc/master/memory-management.html 
- Garbage collector może wywołać się za każdym razem gdy zawołamy którąś z funkcji  `memory_ensure_free*`
- Wtedy, jeśli spełnione są magiczne warunki (np brakuje pamięci na stercie procesu), uruchamia się GC
- GC działa per proces i operuje na pamięci procesu, na którą składa się stack i heap, ale cała ta pamięć też jest często określana jako heap ¯\_(ツ)_/¯
- GC w pierwszej kolejności patrzy na rzeczy które wiadomo, że są potrzebne, czyli
  - Stos i rejestry (czyli np argumenty przekazane do NIFa/BIFa)
  - Process dictionary
- GC w pierwszej kolejności alokuje nową pamięć procesu (new heap) i kopiuje tam te rzeczy potrzebne™. Jeśli mają one zależności do innych termów, to te inne termy też są kopiowane, i tak rekursywnie. Jeśli jakiś term jest skopiowany, to jego wartość w starej pamięci procesu (old heap) jest podmieniana na <<0x2B, adres_w_new_heap>>, tak żeby nie skopiować 2 razy.
- GC usuwa old heap
- Po takiej operacji, termy na których operowaliśmy, a także potencjalnie dowolne pointery (mogły wskazywać na coś co było na starym heapie), mogą być nieaktualne, bo:
  - zostały przeniesione na nowy heap, albo
  - zostały wyjebane wpizdu, bo GC myśli że już są nieużywane
- Na obie sytuacje może pomóc `memory_ensure_free_with_roots`, do którego przekazujemy tablicę termów (roots). Wtedy jeśli wykona się GC, to ta funkcja podmieni termy w tej tablicy na nowe termy na new heap. Wtedy wszystko będzie pięknie działać, pod warunkiem że odwołujemy się do tablicy `roots`, a nie do jakichś swoich zmiennych sprzed GC.`roots`, jakie musimy przekazać, to:
  - termy które nie znajdują się jeszcze na stosie / w rejestrach / w process dictionary, np termy które właśnie stworzyliśmy używając term_create_cośtam
  - termy które znajdują się w ww miejscach, np argumenty NIFa/BIFa, jeśli będziemy ich jeszcze używać po potencjalnym wywołaniu GC
