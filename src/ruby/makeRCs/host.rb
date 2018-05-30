# $Source$
# $Revision$
# $Date$
# $Author$
# $State$
# $Name$

class Host
  attr_accessor :uname

  def initialize(uname)
    @uname = uname
  end

  def color
    @color ||= Color.bold +
      (case @uname.downcase
       when 'aix'
         Color.cyan    + Color.on_blue
       when /cygwin.*/
         Color.white   + Color.on_blue
       when 'hp.ux'
         Color.cyan
       when 'linux'
         Color.green
       when 'sunos'
         Color.yellow  + Color.on_blue
       when 'windows'
         Color.no_color
       else
         Color.red
       end)
    end
end
