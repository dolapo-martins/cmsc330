#Dolapo A. Martins | Lab 1 - CMSC 330
#Exercise 1
class Stack
	@stack
	def initialize
		@stack = []
	end
	
	def push(ele)
		@stack.push(ele)
	end
	
	def pop
		@stack.pop
	end
end

#Test
puts "Exercise 1"
s = Stack.new
s.push(1)
s.push(2)
s.push(3)
puts s.pop
puts s.pop
puts s.pop
puts

#Exercise 2
class Queue
	@queue
	def initialize
		@queue = []
	end
	
	def enqueue(ele)
		@queue.push(ele)
	end
	
	def dequeue
		@queue.shift
	end
end

#Test
puts "Exercise 2"
q = Queue.new
q.enqueue(1)
q.enqueue(2)
q.enqueue(3)
puts q.dequeue
puts q.dequeue
puts q.dequeue
puts

#Exercise 3
puts "Exercise 3"
ary = [5, 11, -8, 3.9, 0]
ary.unshift(13)
puts ary[0..2]
puts
puts ary.permutation(ary.length).to_a.sample(1)
puts
puts ary.sort
puts
puts ary.include?(5)
puts ary.index(3)

#Exercise 4
puts "Exercise 4"
h = Hash.new
h = {"Stacy": 10, "Jim": 9, "Fred": 8}
puts h.to_a.to_s
puts h.keys.to_s
puts h.values.to_s
puts h.has_key?("Stacy")
puts

#Exercise 5
puts "Exercise 5"
for i in 0..ary.size()
	puts ary[i]
end
puts
ary.size().times() do |ele|
	puts ary[ele]
end
puts
ary.each { |ele|
	puts ele
}
puts 

#Exercise 6
puts "Exercise 6"
h.each { |k, v|
	print k, " => ", v
	puts
}
puts
@keys = h.keys
@values = h.values
puts @keys
puts
puts @values
puts

#Exercise 7
puts "Exercise 7"
ary3 = ["Hello, ", "Hi, ", "What's up, "]
ary3.each { |str|
	str += "Dolapo"
	puts str
}
puts 
my_array = [4, 8, 12, 16, 3, 9, 12, 15]
puts my_array.select { |x|
	x % 3 == 0
}