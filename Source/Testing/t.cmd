preprocess TestSuite
compile TestSuite -o
optimize TestSuite -v
codegen TestSuite
dasm TestSuite

preprocess TestNumbers
compile TestNumbers -o
optimize TestNumbers -v
codegen TestNumbers
dasm TestNumbers

preprocess TestStrings
compile TestStrings -o -x
optimize TestStrings -v
codegen TestStrings
dasm TestStrings

TestSuite
TestNumbers
TestStrings
