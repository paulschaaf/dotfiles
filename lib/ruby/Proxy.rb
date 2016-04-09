#!/usr/bin/env ruby

def proxy?; false; end

module Proxy
  def proxy?; true; end

  def preSetProxy(aValue);  end

  def postSetProxy(aValue); end

  def proxy;  @proxy; end

  def proxy=(aValue)
    self.preSetProxy(aValue)
    @proxy=aValue
    self.postSetProxy(aValue)
  end

  def method_missing(*args)
    begin
      @proxy.send(*args)
    rescue Exception => ex
      raise(Exception, "#{self.class} (proxy for #{@proxy}) does not understand #{args[0]}(#{args[1..-1]})", ex.backtrace)
    end
  end

end
