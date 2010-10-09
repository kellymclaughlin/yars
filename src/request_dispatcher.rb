require File.dirname(__FILE__) + '/request_handler.rb'

class RequestDispatcher
  class << self
    attr_accessor :handler, :exception_handler, :exit_after_current_dispatch
  end

  self.exception_handler = nil
  self.exit_after_current_dispatch = false

  def initialize(app, logfile)
    @handler = Yars::RequestHandler.new(app, logfile)
  end

  def self.start(app, logfile)
	RequestDispatcher.new(app, logfile).start
  end

  # Dispatch a request to be handled by the handler module
  def dispatch(args)
    result = @handler.service(convert_args(args))
    result_key = RequestDispatcher.exit_after_current_dispatch ? :last_result : :result
    [result_key, result]
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

  # Erlectricity recieve/respond loop
  def start
    receive(IO.new(3), IO.new(4)) do |f|
      f.when([:request, Array]) do |args|
        f.send! dispatch(args)
        exit if RequestDispatcher.exit_after_current_dispatch
        f.receive_loop
      end

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
