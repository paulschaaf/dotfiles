require 'yaml'

$editor = ENV['EDITOR'] || 'vi'
$tmp    = ENV['TMP']    || '/tmp'

def new_tempfile
  `mktemp #{$tmp}/$$.XXXXXX`.chomp
end

def yaml_edit(obj)
  tempfile = new_tempfile
  File.open(tempfile, 'w') {|f| f << obj.to_yaml}
  system("#$editor #{tempfile}")
  return obj unless File.exists?(tempfile)
  YAML::load(File.open(tempfile))
ensure
  File.delete(tempfile)
end
