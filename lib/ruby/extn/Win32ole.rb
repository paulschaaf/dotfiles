#!/usr/bin/env ruby

# ================================================
# ======= require statements

require 'extn/Module'
require 'cygwin'
require 'win32ole'
require 'extn/Enumerable'
require 'Proxy'

# ================================================
# ======= Standard library modifications

class String
  def to_methodName
    # remove illegal chars, capitalize each but first word, remove spaces
    name = self.gsub(/[^ \d\w]/, '')
    name[0..0]= name[0..0].downcase
    name.gsub!(/ (.)/) {|letter| $1.upcase}
    name
  end
end

module Kernel
  def classFor aName
    begin
      eval(aName.to_s)
    rescue NameError
      nil
    end
  end
end

# ================================================
# =======

class UnableToCloseException < Exception
end

# ================================================
# =======

class WIN32OLE
  include Enumerable

  def vbType
    self.ole_obj_help['name'].delete('_')
  end

  def rubyClass
    classFor self.vbType
  end

  def wrapped
    return self if self.class == WIN32OLE
    aClass = self.rubyClass
    puts "wrapping class"; p aClass
    return self unless aClass
    aClass.new.proxy= self
  end
end

# ================================================
# =======

class WIN32App < WIN32OLE
  @@singleton = nil

  def self.singleton
    @@singleton
  end

  def self.open
    @@singleton ||= self.new(self.to_s + '.Application')
  end

  def self.close
    @@singleton.close
    @@singleton == null
  end

  def self.openWhile
    instance = self.open
    begin
      yield instance
    ensure
      begin
        instance.close
      rescue WIN32OLERuntimeError
        $stderr.puts 'Unable to close application.'
      end
    end
  end

  def self.docsDo(fileNames, *flags, &block)
    self.openWhile {|app|
      app.docsDo(fileNames, *flags, &block)
    }
  end

  def initialize(*args)
    #WIN32OLE.const_load(@app, PowerPoint_const)
    super
    self.minimize
  end

  def cd(aPath)
    ChangeFileOpenDirectory(CygPath.new(aPath).winRelative)
  end

  def documents
    self.subclassResponsibility
  end

  def create(aName, *params)
    instance = self.documentClass.createIn(aName, self, *params)
    return instance unless block_given?

    begin
      yield instance
    ensure
      instance.close(false)
    end
  end

  def basicCreateFile(aPath, *params)
    if params.empty?
      self.documents.add(aPath)
    else
      self.documents.add(aPath, *params)
    end
  end

  def basicOpenFile(aPath, *params)
    if params.empty?
      self.documents.open(aPath)
    else
      self.documents.open(aPath, *params)
    end
  end

  def restoreWindow
    self.windowState=1
    self
  end

  def minimize
    self.windowState=2
    self
  end

  def maximize
    self.windowState=3
    self
  end

  def documentClass
    self.subclassResponsibility
  end

  def close(*args)
    begin
      self.quit(*args)
    rescue Exception => ex
      raise(UnableToCloseException, "Unable to close the application.", ex.backtrace)
    end
  end

  def docsDo(fileNames, *flags)
    fileNames.each {|file|
      self.openDoc(file, *flags) {|aDoc| yield aDoc}
    }
  end

  def openDoc(aName, *params)
    instance = self.documentClass.openIn(aName, self, *params)
    return instance unless block_given?

    begin
      yield instance
    ensure
      instance.close(false)
    end
  end

end

# ================================================
# =======

class Win32Doc
  include Proxy

  attr_reader :application, :properties, :fileSpec

  def self.exposeMacroOrReturn(aSymbol, defaultReturnValue="<Unimplemented macro: #{aSymbol}>")
    self.module_eval <<-CODE
       def #{aSymbol};
         self.runMacro(#{aSymbol});
       rescue;
         # $stderr.puts("ERROR: Unimplemented macro: #{aSymbol}");
         '#{defaultReturnValue}';
       end
    CODE
  end

  def self.exposeMacro(*symbols)
    symbols.each {|aName| self.exposeMacroOrReturn(aName)}
  end

  def self.openIn(aPath, anApp, *params)
    aLocalPath = CygPath.new(aPath).winRelative
    begin
      aPres = anApp.basicOpenFile(aLocalPath, *params)
      (instance = self.new(anApp, aPath)).proxy= aPres
      instance
#     rescue WIN32OLERuntimeError => ex
#       raise(ex, "Cannot open '#{aPath}'", ex.backtrace)
    end
  end

  def self.createIn(aPath, anApp, *params)
    aLocalPath = CygPath.new(aPath).winRelative
    begin
      aPres = anApp.basicCreateFile(aLocalPath, *params)
      (instance = self.new(anApp, aPath)).proxy= aPres
      instance
#     rescue WIN32OLERuntimeError => ex
#       raise(ex, "Cannot open '#{aPath}'", ex.backtrace)
    end
  end

  def close(*args)
    self.Close(*args)
  rescue Exception => ex
    raise(UnableToCloseException.new, "Unable to close the document (it may already be closed).", ex.backtrace)
  end

  def [](aKey)
    @properties[aKey].Value rescue nil
  end

  def []=(aKey, aValue)
    @properties[aKey].Value = aValue rescue nil
  end

  def =~(aString)
    (self.to_s) =~ aString
  end

  def alias_method(newName, oldName)
    self.addMethod("alias_method #{newName.inspect}, #{oldName.inspect}")
  end

  def initialize(app, filename)
    @application, @fileSpec = app, filename
  end

  def postSetProxy(aValue)
    self.initProperties
  end

  def initProperties
    # cache the document properties, and create accessors and mutators for each
    @properties = {}
    self.CustomDocumentProperties.each do |aProp|
      @properties[aProp.name]= aProp
      self.addMethod("def #{aProp.name.to_methodName}; self['#{aProp.name}']; end")
      self.addMethod("def #{aProp.name.to_methodName}= aValue;
                        self['#{aProp.name}']= aValue
                      end")
    end
    self.BuiltInDocumentProperties.each do |aProp|
      @properties[aProp.name]= aProp
      getMethod = self.addMethod("def #{aProp.name.to_methodName}
                                    self['#{aProp.name}']
				  end")
      self.addMethod("def #{aProp.name.to_methodName}= aValue
                        self['#{aProp.name}']= aValue
                      end")
    end
  end

  def runMacro(aMacro)
    #VB code -> Application.Run "Intro-02_IntroToEnJin.ppt!ShowCourseExclusions"
    self.application.run(self.name + '!' + aMacro.to_s)
  end

  def stats
    <<STATS_STRING
    #{self.to_s}
    Author(s):         #{self.author}, #{self.lastAuthor}
      Last updated by:   #{self.lastAuthor}, #{self.lastSaveTime}
      Template build:    #{self.templateBuild}
      Keywords:          #{self.keywords}
      Comments:          #{self.docComments}
STATS_STRING
  end
end

# ================================================
# =======

class Win32DocComponent
  include Proxy

  attr_reader :container

  def self.newIn(aContainer, win32OLEObj)
    (instance = self.new(aContainer)).proxy= win32OLEObj
    instance
  end

  def initialize(aContainer)
    @container = aContainer
  end

  def =~(aString)
    (self.to_s) =~ aString
  end
end

