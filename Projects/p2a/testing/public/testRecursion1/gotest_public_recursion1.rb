#!/usr/bin/env ruby

require_relative "../../framework/test.rb"

TestCase.new("recursion1", "public", File.dirname(__FILE__), 1).run
