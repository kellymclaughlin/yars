require 'optparse'
require 'rack'
require 'thin'
require File.dirname(__FILE__) + '/request_dispatcher.rb'


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
app = load_adapter
logfile = @options[:rails_root] + "/log/yars.#{Process.pid}.log"

at_exit do
  RequestDispatcher.start(app, logfile)
end

