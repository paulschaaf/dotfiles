#!/usr/bin/env ruby

#require 'builder'

require 'logger'
$log = Logger.new(STDERR)
$log.sev_threshold = Logger::INFO # DEBUG
$log.progname = 'PCF'

class Array
  def to_xml
    self.collect {|child| child.to_xml}.join("\n")
  end
end

module PCF
  module Container
    def close_xml
      ">\n" + self.contents_xml + "\n</#{self.name}>"
    end

    def contents_xml
      self.contents.to_xml
    end

    def <<(anElement);  self.contents << anElement;     end
    def add(*args);     args.each {|arg| self << arg};  end
    def contents;       @contents ||= [];               end
    def properties;     super - [:@contents];           end
  end

  class Element
    attr_accessor :id

    def initialize(init_hash={})
      @id = nil
      init_hash.each_pair {|key, value| self.send("#{key}=", value)}
      yield self if block_given?
    end

    def name;       self.class.name[/[^:]+$/]; end
    def properties; self.instance_variables;   end

    def to_xml
      # xml = Builder::XmlMarkup.new(:indent => 3)
      # self.attributes
      self.open_xml + self.close_xml
    end

    def open_xml
      xml = "<#{self.name}"
      self.properties.each do |ivar|
        $log.debug("ivar = #{ivar.inspect}")
        vname = ivar[1..-1];            $log.debug("vname = #{vname.inspect}")
        value = self.send(vname);       $log.debug("value = #{value.inspect}")
        xml += %Q[ #{vname}="#{value}"] if value
      end
      xml
    end

    def close_xml; "/>"; end
  end

  class Widget < Element
    attr_accessor :desc, :editable, :hideIfEditable, :hideIfReadOnly, :mode, :visible
  end

  class Require < Element
    attr_accessor :initialValue, :name, :type
    def initialize(*args)
      @name, @type = nil
      super
    end
  end

  class WidgetContainer < Widget
    include Container
    attr_accessor :requires

    def properties;  super - [:@requires];  end

    def initialize(*args)
      @requires = []
      super
    end

    def <<(arg)
      arg.kind_of?(Require) ? self.requires << arg : super
      self
    end

    def contents_xml
      self.requires.to_xml + super
    end
  end

  class AtomicWidget < Widget
    attr_accessor :value
    attr_writer :label

    def self.for(aValue, *args, &block)
      self.new(:value => aValue, &block)
    end

    def initialize(*args)
      super
      @label = nil
    end

    def field_name; self.value[/[^.]+$/];                            end
    def id;         @id    ||= self.field_name + 'ID';               end
    def label;      @label ||= 'displaykey.Ext.' + self.field_name;  end
   end

  class InputSet < WidgetContainer; end

  class Input < AtomicWidget; end

end

$i = PCF::InputSet.new
$i << PCF::Require.new(:name => :aContact, :type => :ABContact)
$i << PCF::Input.for('rootObj.CreateDate')
$i << PCF::Input.new(:value => 'rootObj.FullName', :editable => false)
puts $i.to_xml
