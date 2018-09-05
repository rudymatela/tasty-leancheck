mk/All.o: \
  src/Test/Tasty/LeanCheck.hs \
  mk/All.hs
mk/Toplibs.o: \
  src/Test/Tasty/LeanCheck.hs \
  mk/Toplibs.hs
Setup.o: \
  Setup.hs
Setup: \
  Setup.hs \
  mk/toplibs
src/Test/Tasty/LeanCheck.o: \
  src/Test/Tasty/LeanCheck.hs
tests/test.o: \
  tests/test.hs \
  src/Test/Tasty/LeanCheck.hs
tests/test: \
  tests/test.hs \
  mk/toplibs
