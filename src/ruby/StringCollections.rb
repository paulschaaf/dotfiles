#!/usr/bin/env ruby

#require 'debugMethod.rb'

class String

  def select(pattern=nil, *patterns)
    if pattern == nil
      self
    else
      (self.scan(pattern).join).select(*patterns)
    end
  end

  def reject!(*patterns)
    patterns.each do |pattern|
      self.gsub!(pattern, "")
    end
    self
  end

  #def reject(*patterns)
  #	self.clone.reject!(*patterns)
  #end

  def gsubAll!(pattern='', aString=nil, *pairlist)
    if pattern.empty?
      self
    else
      newStr = aString.gsub(/\\n/,"\n").gsub(/\\t/,"\t").gsub(/\\r/,"\r")
      self.gsub!(pattern, newStr)
      self.gsubAll!(*pairlist)
    end
  end

  def gsubAll(pattern='', aString=nil, *pairlist)
    if pattern.empty?
      self
    else
      newStr = aString.gsub(/\\n/,"\n").gsub(/\\t/,"\t").gsub(/\\r/,"\r")
      self.gsub(pattern, newStr)
      self.gsubAll(*pairlist)
    end
  end

  #def replace(*pairlist)
  #	self.clone.replace!(*pairlist)
  #end
  #def replacei!(*pairlist)
  #	return self if pairlist.empty?
  
  #	#this is actually an error
  #	return self if pairlist.size.modulo(2) == 1
  #	index = 0
  #	while index < (pairlist.size)-1
  #		newStr = pairlist[index+1].gsub(/\\n/,"\n").gsub(/\\t/,"\t").gsub(/\\r/,"\r")

  #		self.gsub!(pairlist[index], newStr)
  #		index += 2
  #	end
  #	self
  #end

end
