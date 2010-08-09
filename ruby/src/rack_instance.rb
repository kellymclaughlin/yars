require 'optparse'
require 'rack'
require 'thin'
require File.dirname(__FILE__) + '/erlang_coupling.rb'


#TODO: Work out what to do about installing gems for target rack project.

@options = {}

def load_adapter
  adapter = @options[:adapter] || Rack::Adapter.guess(@options[:rails_root])
  puts ">> Using #{adapter} adapter"
  puts @options[:rails_root]
  Rack::Adapter.for(adapter, {:chdir => @options[:rails_root], :environment => @options[:rails_env]})
rescue Rack::AdapterNotFound => e
  raise InvalidOption, e.message
end


opts = OptionParser.new do |opts|
  opts.on("-r", "--rails-root RAILS_ROOT", String) do |x|
    @options[:rails_root] = x
  end
  opts.on("-t", "--test", "enable test mode") do
    @options[:test] = true
  end
  opts.on("-e", "--rails-env ENV", String) do |x|
    @options[:rails_env] = x
  end
end
opts.parse(ARGV)
@options[:rails_root] = File.join(File.dirname(__FILE__), *%w[.. test app]) if @options[:test]
puts "Rails root " + @options[:rails_root] if @options[:rails_root] + "\n"
@options[:rails_env] ||= 'development'
puts "Rails env " + @options[:rails_env] + "\n"

# Load adapter for Rack application. 
#app = Rack::Adapter::Rails.new(:root => options[:rails_root], :environment => options[:rails_env])
app = load_adapter
logfile = @options[:rails_root] + "/log/yeref.#{Process.pid}.log"
$handler = Yeref::ErlangCoupling.new(app, logfile)


# request dispatcher
class RequestDispatcher
  class << self
    attr_accessor :signatures, :node_kind, :pkgs, :tags, :roles, :extra_config, :exception_handler, :exit_after_current_dispatch
  end

  self.exception_handler = nil
  self.exit_after_current_dispatch = false

  # Define a handler method
  #   +method+ is the Symbol method name
  #   +names+ is a list of required parameters
  #
  # Returns nothing
  def self.handle(method, *names, &block)
    RequestDispatcher.signatures[method] = names.sort { |a, b| a.to_s <=> b.to_s }

    define_method("handle_proxy_#{method}", &block)

    define_method("handle_#{method}".to_sym) do |iargs|
      args = convert_args(iargs)
      self.verify_args(method, args)

      self.send("handle_proxy_#{method}", args)
    end
  end

  def self.start
	#h = $handler.new
	RequestDispatcher.new.start
  end

  #def handle(:handle_request, :request) do |args|
    #$handler.service(args[:request])
  #end

    # Dispatch a method by its name
  #   +method+ is the Symbol representing the method name
  #   +retype+ is the response type Symbol (:json | :pure)
  #   +args+ is the Erlang-style args for the call
  #
  # Returns one of:
  #   [:result, <jsonified result>]
  #   [:error, <error string>]
  def dispatch(method, retype, args)
    #result = self.send("handle_#{method}".to_sym, args)
    result = $handler.service(convert_args(args))
    result_key = RequestDispatcher.exit_after_current_dispatch ? :last_result : :result
    case retype
      when :json
        [result_key, [:raw, result.to_json]]
      when :pure
        [result_key, result]
      else
        raise "Unknown response type: #{retype}"
    end
  rescue Exception => e
    if e.instance_of?(SystemExit)
      exit
    elsif RequestDispatcher.exception_handler
      begin
        RequestDispatcher.exception_handler.call(e)
      rescue Exception => e2
        [:error, e2.message + "\n\n" + e2.backtrace.join("\n")]
      end
    else
      [:error, e.message + "\n\n" + e.backtrace.join("\n")]
    end
  end


  # Start the Erlectricity recieve/respond loop
  #
  # Never returns
  def start
    receive(IO.new(3), IO.new(4)) do |f|
      f.when([:request, Array]) do |args|
        method = args[0]
		return_type = args[1]
        args = args[2..-1]
		puts "Method #{method}"
		puts args.inspect
        f.send! dispatch(method, return_type, args)
        exit if RequestDispatcher.exit_after_current_dispatch
        f.receive_loop
      end

      #f.when(:config) do
        #f.send! [:result, self.config]
        #f.receive_loop
      #end
      
      f.when(:ping) do
        f.send!(:pong)
        f.receive_loop
      end
      
      f.when(:quit) do
        exit(0)
      end
    end
  end

  # Specify a block that takes an Exception as its single argument
  # to be called when an exception is not handled inside a handler
  #   +block+ is the block to execute
  #
  # Block should
  #   Raise -or-
  #   Return one of:
  #     [:result, <jsonified result>]
  #     [:error, <error string>]
  #
  # Returns nothing
  def handle_exception(&block)
    RequestDispatcher.exception_handler = block
  end

  def return_and_exit(data)
    RequestDispatcher.exit_after_current_dispatch = true
    data
  end

    # Convert Erlang-style args to Ruby-style args
  #   +iargs+ is the Erlang-style arg structure
  #
  # Returns Hash (converted args)
  def convert_args(iargs)
    args = HashWithIndifferentAccess.new
    iargs.each do |a|
      args[a[0]] = convert_args_node(a[1])
    end
    args
  end

  # Helper for +convert_args+. Recursively converts a node in
  # Erlang-style to a node in Ruby-style
  #  +node+ is the Erlang-style node
  #
  # Returns String|Symbol|Hash|Array
  # Raises if an invalid node is encountered
  def convert_args_node(node)
    if node.kind_of?(String) || node.kind_of?(Symbol) || node.kind_of?(Numeric) ||
       node.kind_of?(TrueClass) || node.kind_of?(FalseClass) || node.kind_of?(NilClass)
      node
    elsif node.instance_of?(Array)
      if node[0] == :struct
        node[1].inject(HashWithIndifferentAccess.new) do |acc, x|
          acc[x[0]] = convert_args_node(x[1]); acc
        end
      elsif node[0] == :array
        node[1].inject([]) do |acc, x|
          acc << convert_args_node(x)
        end
      else
        raise "Invalid tagged node: #{node.inspect}"
      end
    else
      raise "Invalid node, must be an instance of String, Symbol, Numeric, True, False, Nil, or Array: #{node.inspect} (#{node.class.inspect})"
    end
  end

end

at_exit do
  RequestDispatcher.start
end

