
class DataStore

	UNION = "union_by"
	INTERSECT = "intersect_by"

	def initialize(store = {})
		@store = store
	end

	def set_data(name, list)
		@store[name] = list
	end

	def method_missing(method_name_symbol, *args)
		method_name = method_name_symbol.to_s
		return union_by(method_name) if method_name.start_with? UNION
		return intersect_by(method_name) if method_name.start_with? INTERSECT
		raise "Invalid method: " + method_name
	end

	def get_data(name)
		res = @store[name]
		res ? res : []
	end

	private

	def get_keywords(method_name)
		words = method_name.split("_")[2..-1]
		words.delete("and")
		words
	end

	def union_by(method_name)
		words = get_keywords(method_name)
		res = []
		words.each{ |word|
			res += get_data(word)
		}
		res.uniq
	end

	def intersect_by(method_name)
		values = []
		words = get_keywords(method_name)
		words.each do |word|
			data = get_data(word)
			values += [data] if data.size > 0
		end
		values.reduce(:&)
	end


end


d = DataStore.new
d.set_data("polimi",["milano","piacenza","lecco","como"])
d.set_data("statale", ["milano"])

puts "union:"
puts d.union_by_polimi_and_statale_and_bicocca

puts "intersection:"
puts d.intersect_by_polimi_and_statale

