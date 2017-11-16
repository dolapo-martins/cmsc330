To run all tests, simply run:
    ruby test_all.rb

To run a single test, you may just run its gotest file alone. For example, for a test named testCurry:
    ruby testing/public_curry/gotest_public_curry.rb
This will run all of the modules for this test

To see your output and expected output for any module, you can go to the test directory and open the file however you prefer. Sticking with testCurry (which has 1 module, module0), you can find each file at the following locations:
    - testing/public_curry/module0.expected (correct code's output)
    - testing/public_curry/module0.output (your code's output)
    - testing/public_curry/module0.error (your code's errors; won't exist if there were no errors)

The simplest way to show what's in one of these files is just to use the command "cat", for example:
    - cat testing/public_curry/module0.expected

If you fail any test, you can find the testing code, your output, and the expected output in the testing/<testname> directory!
