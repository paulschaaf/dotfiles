require 'yaml'

class Contact
  attr_accessor :cell, :cell_provider, :email, :home, :login, 
    :name, :nickname, :work, :work_email
    
  def initialize(args)
    args.each_pair {|var, val| self.send("#{var}=", val)}
  end

end

$contacts = Hash.new

YAML.load(File.read('contacts.yml')).each {|e| $contacts[e.login] = e}

# If this script is run as stand-alone script
if __FILE__ == $0 # ////////////////////////////////////////////

p $contacts

# //////////////////////////////////////////////////////////////

# $contacts = []
# $contacts << Contact.new(:name     => 'Paul Schaaf',
#                          :login    => :pschaaf,
#                          :nickname => :paul,
#                          :email    => 'paul_schaaf@yahoo.com',
#                          :cell     => 4086444762,
#                          :cell_provider => :TMOBILE)

# $contacts << $contacts[-1].dup
# $contacts[-1].login = :root

# $contacts << Contact.new(:name     => 'Victoria Schaaf', 
#                          :login    => :vschaaf,
#                          :nickname => :vic,
#                          :email    => 'designer007@excite.com',
#                          :cell     => 4086444769,
#                          :cell_provider => :TMOBILE)

end
# //////////////////////////////////////////////////////////////
