module Math

	def self.min(n1, n2)
		n1 < n2 ? n1 : n2
	end

	def self.max(n1, n2)
		n1 > n2 ? n1 : n2
	end
end



class Range

	def +(other)
		new_min = Math::min(self.min, other.min)
		new_max = Math::max(self.max, other.max)
		# alternative is to use [self.min, other.min].min
		Range.new(new_min, new_max)
	end
end

# a = (1..5) + (10..20)
# puts a



# Implement the fibonacci algorithm using metaprogramming
# and memoization.
# Example taken from: http://www.commandercoriander.net/blog/2013/04/10/metaprogramming-fibonacci

class Fib

	@@memo = {0 => 0, 1 => 1}

	def self.fib_rec(n)
		@@memo[n] ||= fib_rec(n-1) + fib_rec(n-2)
		# if @@memo[n].nil?
		# 	@@memo[n] = fib_rec(n-1) + fib_rec(n-2)
		# end
		# @@memo[n]
	end
end


module Memo
	@@memo = {0 => 0, 1 => 1}

	def memoize(method)
		alias_method "old_#{method}".to_sym, method
		define_method(method) do |*args|
			@@memo[args] ||= send("old_#{method}".to_sym, *args)
		end
	end
end


module Memo
	def memoize(method)
		old_method = instance_method(method)
		memo = {}
		define_method(method) do |*args|
			memo[args] ||= old_method.bind(self).call(*args)
		end
	end
end

class Fib
	extend Memo
	def fib_rec(n)
		return n if n < 2
		return fib_rec(n-1) + fib_rec(n-2)
	end
	memoize :fib_rec
end

# n = 10
# f = Fib.new
# puts f.fib_rec(n)
# puts Fib.fib_rec(n)


# Define an attr_checked method for class which takes 
# a validation condition for a field
class Person
	attr_checked :age do |v|
		v >= 18
	end
end

class Wine
	attr_checked :age do |v|
		v >= 0
	end
end

# p = Person.new
# p.age = 18
# puts p.age


class Class

	def attr_checked(attribute, &validation)
		define_method "#{attribute}=" do |value|
			raise "Invalid attribute" unless validation.call(value)
			instance_variable_set("@#{attribute}", value)
		end
		define_method "#{attribute}" do
			instance_variable_get("@#{attribute}")
		end
	end
end



# A DSL for unit testing
# taken from: http://www.skorks.com/2011/02/a-unit-testing-framework-in-44-lines-of-ruby/

module Kernel
	def describe(description, &block)
		tests = Dsl.new.parse(description, block)
		tests.execute
	end
end

class Object
	def should
		self
	end
end

class Dsl
	def initialize
		@tests = {}
	end
	def parse(description, block)
		instance_eval(&block)
		Executor.new(description, @tests)
	end
	def it(description, &block)
		@tests[description] = block
	end
end


class Executor
	def initialize(description, tests)
		@description = description
		@tests = tests
		@success_count = 0
		@failure_count = 0
	end

	def execute
		puts "#{@description}"
		@tests.each_pair do |name, proc|
			print "- #{name}"
			result = instance_eval(&proc)
			result ? @success_count += 1 : @failure_count += 1
			puts result ? " SUCCESS" : " FAILURE"
		end
		summary
	end

	def summary
		puts "\n #{@tests.keys.size} tests, #{@success_count} success, #{@failure_count} failures"
	end
end


# describe "some tests" do
# 	it "should be true" do
# 		true.should == true
# 	end

# 	it "should show that an expression can be true" do
# 		(5 == 5).should == true
# 	end

# 	it "should be failing deliberately" do
# 		5.should == 6
# 	end
# end




# DSL for the classic logo game.

class Turtle

	# directions 0=E, 1=S, 2=W, 3=N
	# axis 0=x, 1=y
	MARK = "#"
	def initialize
		@board = Hash.new(" ")
		@x = @y = 0
		@direction = 0
		pen_up
	end

	def pen_up
		@pen_down = false
	end
	def pen_down
		@pen_down = true
		mark_current_location
	end
	def forward(n=1)
		n.times { move }
	end

	def left
		@direction -= 1
		@direction = 3 if @direction < 0 
	end

	def right
		@direction += 1
		@direction = 0 if @direction > 3
	end

	def draw
		min_x, max_x = @board.keys.map{|x,y| x}.minmax
		min_y, max_y = @board.keys.map{|x,y| y}.minmax
		min_y.upto(max_y) do |y|
			min_x.upto(max_x) do |x|
				print @board[[x,y]]
			end
			puts
		end
	end

	def walk(&block)
		instance_eval(&block)
	end

	private

	def move
		increment = @direction > 1 ? -1 : 1
		if @direction.even?
			@x += increment
		else
			@y += increment
		end
		mark_current_location
	end

	def mark_current_location
		@board[[@x, @y]] = MARK if @pen_down
	end
end

turtle = Turtle.new
turtle.walk do
	3.times do
		forward 8
		pen_down
		4.times do
			forward 4
			left
		end
		pen_up
	end
end

turtle.draw








