require 'test/unit'
require File.dirname(__FILE__) + '/../src/erlang_coupling'

class ErlangCoupling_Test < Test::Unit::TestCase
  def setup
    @ec_instance = ErlangCoupling.new
    puts "THERE"
    @ec_instance.run
  end

  def string_test
    puts "HERE2"
    assert_equal("You said: test string", @ec_instance.send("test string"))
  end

  def ping_test
    puts "HERE3"
    assert_equal(:pong, @ec_instance.send(:ping))
  end

  #def teardown
  #end
end

