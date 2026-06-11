# connecting-dots

uruchamianie:

1. stack build
2. stack run solver-exe -- +RTS -N4 -s
3. stack run generator-exe -- 3 5 7 10

ghc -O2 -threaded "-with-rtsopts=-N" -iapp/solver app/solver/Solver.hs -o solver-exe
ghc -iapp/generator app/generator/Generator.hs -o generator-exe
./generator-exe 10 15 20
./solver-exe
n = 23
10 20
11 21
12 20
13 21
14 20
15 21
16 20
17 21
18 20
19 21
20 20
21 21
22 20
23 21
24 20
25 21
26 20
27 21
28 20
29 21
30 22
31 23
32 24
33 25
ODPOWIEDŹ TAK
