class WordNet
	attr_accessor :common_parents, :common_lengths, :LCAs
	def initialize(syns, hyps)
		@edges = Hash.new { |k,v| k[v] = [] }
		@vertices = Hash.new { |k,v| k[v] = [] }
		@LCAs = Array.new

		#check data validity
		
			#check synsets
			invalids = 0
			syn_file = File.open(syns, "r")
			syn_file.each do |line|
				line.chomp! #remove end of line character
			#split via regex of form -> will return an array
				if line !~ /^id:\s{1}\d+\s{1}synset:\s{1}[^\s,]+(,[^\s,]+)*$/ then
					invalids += 1
					if invalids == 1 then
						puts "invalid synsets" #only the first time
					end #end inner if
					puts line #output invalid lines';
				end #end outer if
			end #end do
			if invalids != 0 then exit end #do nothing if there was an invalid line
					
			#check hypernyms
			invalids = 0
			hyp_file = File.open(hyps, "r")
			hyp_file.each do |line|
				line.chomp! #remove end of line character
				if line !~ /^^from:\s{1}\d+\s{1}to:\s{1}[\d]+(,[\d]+)*$/ then
					invalids += 1
					if invalids == 1 then
						puts "invalid hypernyms" #only the first time
					end #end inner if
					puts line #output invalid lines';
				end #end outer if
			end #end do
			if invalids != 0 then exit end #do nothing if there was an invalid line
		
		#----Tested and Validity Works------
		#----Construct the graph------------
			#----Construct the graph------------
			#parse file
			syn_file = File.open(syns, "r")
			syn_file.each do |line|
				#map id to synsets
				data = line.split(" ") #get data elements, space delimited
				
				data[3].split(",").each { |ele|
					@vertices[Integer(data[1])].push ele
				}
			end
			
			hyp_file = File.open(hyps, "r")
			hyp_file.each do |line|
				#map synset relationships
				data = line.split(" ") #get data elements, space delimited
				
				data[3].split(",").map(&:to_i).each { |ele|
					@edges[Integer(data[1])].push ele
				}
			end
		#----Graph has been built-----------
		syn_file.close
		hyp_file.close	
	end
	
	def isnoun(words)
		if words.empty? then false
		else 
			matched_terms = 0
			words.each { |ele|
				visited = []
				@vertices.keys.each { |arr|
					if (@vertices[arr].include?(ele)) && (!visited.include?(ele))
						matched_terms += 1
						visited << ele
					end
				}
			}
			(words.length != 0) && (matched_terms == words.length)
		end
		
	end
	
	def nouns
		count = 0
		@vertices.keys.each do |arr|
			count += @vertices[arr].length
		end
		count
	end
	
	def edges
		count = 0
		@edges.keys.each do |arr|
			count += @edges[arr].length
		end
		count
	end	

	#Need to account for when "V" is not in the graph --> remove all those not in graph
	def length(v,w) #arrays
		@common_lengths = Hash.new{ |k,v| k[v] = []}			
		len = 0
		@lengths = []
		v.each { |v_ele|
			w.each { |w_ele|
				if ((@vertices.keys.include?(v_ele) == false) || (@vertices.keys.include?(w_ele) == false)) then
					@lengths << -1
				else
					@lengths << cost(v_ele, w_ele)
				end
			}
		}
		@lengths.delete_if{ |x|
			x == -1
		}.sort!
		
		if @lengths.empty? then 
			len = -1 
		else
			len	= @lengths[0] 
		end
		len
	end
	
	def cost(v, w)
		v_paths = Hash.new
		w_paths = Hash.new
		tracker = Array.new
		if (@vertices[v] == @vertices[w]) then 
			@LCAs << v
			0
		else
			tracker << v
			length = 0
			v_paths[v] = length
			while (tracker.any?)
				tracker.shift
				length += 1
				v_paths.keys.each { |ele|
					@edges[ele].each { |parent|
						unless v_paths.keys.include?(parent)
							tracker << parent
							v_paths[parent] = length
						end
					}
				}
			end
			
			tracker << w
			length = 0
			w_paths[w] = length
			while (tracker.any?)
				tracker.shift
				length += 1
				w_paths.keys.each { |ele|
					@edges[ele].each { |parent|
						unless w_paths.keys.include?(parent)
							tracker << parent
							w_paths[parent] = length
						end
					}
				}
			end
			@common_parents = (v_paths.keys & w_paths.keys)
			@common_parents.each { |ancestor|
				@common_lengths[ancestor] << v_paths[ancestor] + w_paths[ancestor]
			}
			
			#print common_lengths.values.first.sort
			lengths = common_lengths.values.sort.first.first
			#all keys that have same length
			common_lengths.keys.each { |x|
				if (@common_lengths[x].include?(lengths) && !@LCAs.include?(x)) then
					if x.kind_of?(Array) then
						x.each { |elem|
							@LCAs << x
						}
					else
						@LCAs << x
					end
				end
			}
			#unless @LCAs.include?(common_lengths.key(length))
			#	@LCAs << common_lengths.key(length)
			#end
			lengths
		end
	end
	
	def ancestor(v,w)
		shortest = length(v,w)
		if shortest == -1 then -1
		elsif shortest == 0 then 0
		else
		#call cost on each pair, if the cost is the length push that as an ancestor
			@LCAs
		end
	end
	
	def root(v, w)
		roots = []
		search_v = [] #Array.[](@vertices.key(v_arr))
		search_w = [] #Array.[](@vertices.key(w_arr))
		@vertices.keys.each { |x|
			if @vertices[x].include?(v) then 
				search_v << x
			end
			
			if @vertices[x].include?(w) then 
				search_w << x
			end
		}
		
		len = length(search_v, search_w)
		#should root only return parents of the minimum length away?
		if len  == -1 then -1 
		elsif len == 0 then
			(search_v & search_w).each { |ele|
				roots.concat(@vertices[ele])
			}
			roots 		
		else
		@vertices.each { |k,v|
			if (@LCAs.include?(k) == true) &&  (@common_lengths[k].include?(len)) then
					roots.concat(v)
				end
				
				#unless roots.include?(x)
				#	@vertices[x].each { |root|
					#	roots << root
					#}
				#end
			}
			roots
		end
	end

	def outcast(nouns)
		outcast_val = 0
		outcast = []
		temp_arr = []
		distances = Hash.new
		nouns.each { |noun|
			noun_arr = []
			temp_arr = nouns.select{ |other|
				other != noun
			}
			
			@vertices.keys.each{ |k|
				if @vertices[k].include?(noun) then noun_arr << k end
			}
			
			distances[noun] = 0
			temp_arr.each { |ele|
				temp =[]
				@vertices.keys.each{ |k|
					if @vertices[k].include?(ele) then temp << k end
				}
				distances[noun] += (length(noun_arr, temp) ** 2)
			}
		}
		
		distances = Hash[distances.sort_by { |k,v| v}.reverse]
		outcast_val = distances.values.first #greatest distance
		
		outcast = distances.keys.select{ |k|
			distances[k] == outcast_val
		}
		
		multipliers = []
		outcast.each { |out|
			mult = 0
			loop {
				mult += 1
				multipliers << out
				break if mult == nouns.count(out)
			}
		}
		multipliers
	end
end

#If the result is an array, then the array's contents will be printed in a sorted and space-delimited string. 
#Otherwise, the result is printed as-is
def print_res(res)
    if (res.instance_of? Array) then 
        str = ""
        res.sort.each {|elem| str += elem.to_s + " "}
        puts str.chomp
    else 
        puts res
    end
end 

#Checks that the user has provided an appropriate amount of arguments
if (ARGV.length < 3 || ARGV.length > 5) then
  fail "usage: wordnet.rb <synsets file> <hypersets file> <command> <input file>"
end

synsets_file = ARGV[0]
hypernyms_file = ARGV[1]
command = ARGV[2]
input_file = ARGV[3]

wordnet = WordNet.new(synsets_file, hypernyms_file)

#Refers to number of lines in input file
commands_with_0_input = %w(edges nouns)
commands_with_1_input = %w(isnoun outcast)
commands_with_2_input = %w(length ancestor)

#Executes the program according to the provided mode
case command
when *commands_with_0_input
	puts wordnet.send(command)
when *commands_with_1_input 
	file = File.open(input_file)
	nouns = file.gets.split(/\s/)
	file.close    
    print_res(wordnet.send(command, nouns))
when *commands_with_2_input 
	file = File.open(input_file)   
	v = file.gets.split(/\s/).map(&:to_i)
	w = file.gets.split(/\s/).map(&:to_i)
	file.close
    print_res(wordnet.send(command, v, w))
when "root"
	file = File.open(input_file)
	v = file.gets.strip
	w = file.gets.strip
	file.close
    print_res(wordnet.send(command, v, w))
else
  fail "Invalid command"
end