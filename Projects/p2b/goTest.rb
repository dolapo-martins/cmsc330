#!/usr/bin/ruby -w

tests = [ "testHigherOrder0", "testHigherOrder1",
          "testIntTree", "testpTree","testCommon1", "testCommon2",
          "testGraph1", "testGraph2", "testReachable" ]

tests.each { |x|
	system("ocaml #{x}.ml > #{x}.log")
	if $? != 0
		puts "#{x} failed: run-time error"
	end
	#system("echo choose fc for Windows or diff for Apple or Unix in goTest.rb")
	system("diff #{x}.log #{x}.out") # Apple/Unix
	# system("fc #{x}.log #{x}.out")   # Windows/DOS
	if $? != 0 
		puts "#{x} failed: incorrect output"
	else
		puts "#{x} passed"
	end
}
