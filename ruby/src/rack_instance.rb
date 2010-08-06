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
  Rack::Adapter.for(adapter, {:chdir => @options[:rails_root]})
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
puts "Rails root " + @options[:rails_root] if @options[:rails_root]
@options[:rails_env] ||= 'development'

# Load adapter for Rack application. 
#app = Rack::Adapter::Rails.new(:root => options[:rails_root], :environment => options[:rails_env])
app = load_adapter
logfile = @options[:rails_root] + "/log/yeref.#{Process.pid}.log"
$handler = Yeref::ErlangCoupling.new(app, logfile)


# chassis
class RequestDispatcher
  class << self
    attr_accessor :signatures, :node_kind, :pkgs, :tags, :roles, :extra_config, :exception_handler, :exit_after_current_dispatch
  end

  def self.start
    #h = $handler.new
    #h.start
  end

  #def handle(:handle_request, :request) do |args|
    #$handler.service(args[:request])
  #end

  # Start the Erlectricity recieve/respond loop
  #
  # Never returns
  def self.start
    receive(IO.new(3), IO.new(4)) do |f|
      f.when([:call, Array]) do |args|
		puts args.inspect
        method = args[0]
		#method = args['method']
		#path = args['path']
        #retype = args[1]
        #args = args[2..-1]
		puts "Method #{method}"
		#puts "Path #{path}"
        #f.send! self.dispatch(method, retype, args)
        #exit if exit_after_current_dispatch
        f.receive_loop
      end

      f.when([:request, Array]) do |args|
        method = args[0]
		puts args.inspect
		#method = args['method']
		#path = args['path']
        #retype = args[1]
        #args = args[2..-1]
		puts "Method #{method}"
		#puts "Path #{path}"
        #f.send! self.dispatch(method, retype, args)
        #exit if exit_after_current_dispatch
        f.receive_loop
      end
      
      #f.when([:request, Array]) do |args|
        #method = args[0]
        #path = args[1]
        #puts "Method #{method}"
        #puts "Path #{path}"
        #f.receive_loop
      #end

      f.when(:config) do
        f.send! [:result, self.config]
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

end

at_exit do
  RequestDispatcher.start
end

