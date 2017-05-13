#!/usr/bin/env ruby

class CygPath
  attr_reader :unixAbsolute
  alias :to_s :unixAbsolute

  def initialize(aPath)
    @unixAbsolute = convert('-au', aPath)
  end

  def winRelative
    convert('-w')
  end

  def winAbsolute
    convert('-aw')
  end

  def unixRelative
    convert('-u')
  end

private

  def convert(convert_type, aPath=@unixAbsolute)
    IO.popen("cygpath #{convert_type} \"#{aPath}\"").readlines[0].chomp!
  end

end
