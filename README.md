# connecting-dots

uruchamianie:

1. stack build
2. stack run solver-exe -- +RTS -N4 -s
3. stack run generator-exe -- 3 5 7 10

ghc -O2 -threaded "-with-rtsopts=-N" -iapp/solver app/solver/Solver.hs -o solver-exe
ghc -iapp/generator app/generator/Generator.hs -o generator-exe
./generator-exe 10 15 20
./solver-exe
