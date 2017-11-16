#Exercise 1
ary = ["Adam", "Austin", "Damien", "Anwar", "Craig"]
ary.delete_if { |ele|
	(ele.length) % 2 == 0
}

ary.sort_by { |ch|
	ch[-1]
}

power_levels = { "Damien" => "2", 
				"Austin" => "3", 
				"Anwar" => "400000", 
				"Adam" => "Unlimited"}
				
power_levels.select! { |k,v|
	k.length < v.length
}

#Exercise 2
r = /^gr[ae]y$/
"grey" =~ r
"gray" =~ r

r = /^colo[u]?r$/
"color" =~ r
"colour" =~ r

r = /^(?:na)+, Batman!$/ #don't want/ need capture groups
"nana, Batman!" =~ r
"nananana, Batman!" =~ r
"na, Batman!" =~ r

#Exercise 3
class Traveller
	@@cities = { "detroit" => ["cleveland", "columbus"],
                "chicago" => ["columbus", "indianapolis"],
                "columbus" => ["detroit", "cincinnati"] }

   def travel(from)
	@@cities[from].each { |v|
      yield from, v
	 }
   end
end

t = Traveller.new
t.travel("detroit") { |from, dest| puts "#{dest} is reachable from #{from}" }

#Exercise 4
class Stack
   include Enumerable
   include Comparable
	attr_accessor :s
   def initialize
      @s = []
   end

   def push(ele)
      @s.push(ele)
   end

   def pop(ele)
      @s.pop(ele)
   end   
   
   def each(&block)
	  @s.each(&block)
	end
	
	def <=>(other)
	
		if (@s.size == other.s.size) then
			@s.each { |x|
				x == other.s[@s.index(x)]
			}
		else
			@s.size <=> other.s.size
		end
		
		
	end
	
end

puts
puts

s1 = Stack.new
s2 = Stack.new

s1.push("eggplant")
s1.push("tomato")
s1.push("avacado")
s2.push("peach")
s2.push("blackberry")

s1 >= s2
s1.each { |veggie| puts veggie }