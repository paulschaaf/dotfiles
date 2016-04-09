#require 'extn/Module'
#require 'extn/Proc'

# $Source: e:/MyDocuments/cvsroot/bin/xrescreen,v $
# $Revision: 1.6 $
# $Date: 2003/08/30 01:02:35 $
# $Author: pschaaf $
# $State: Exp $
# $Name:  $

# //////////////////////////////////////////////////////////////
# ##### running on Un*x
$ON_CYGWIN  = (/cygwin/ =~ RUBY_PLATFORM)
$ON_LINUX   = (/linux/  =~ RUBY_PLATFORM)
$ON_UNIX    = (/[iu]x/  =~ RUBY_PLATFORM)
$ON_WINDOWS = $ON_CYGWIN || (/win/ =~ RUBY_PLATFORM)

$ON_POSIX   = $ON_LINUX || $ON_CYGWIN

if $ON_UNIX
  def on_unix; yield; end
else
  def on_unix; false; end
end

if $ON_WINDOWS
  def on_windows; yield; end
else
  def on_windows; false; end
end

if $ON_CYGWIN
  def on_cygwin; yield; end
else
  def on_cygwin; false; end
end

if $ON_LINUX
  def on_linux; yield; end
else
  def on_linux; false; end
end

if $ON_POSIX
  def on_posix; yield; end
else
  def on_posix; false; end
end

# //////////////////////////////////////////////////////////////
# If this script is run as stand-alone script
if __FILE__ == $0 # ////////////////////////////////////////////

  on_unix {puts 'This is Unix'} || puts('This is not Unix')

end # //////////////////////////////////////////////////////////
