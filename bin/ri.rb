#!/usr/bin/env ruby

# $Source: e:/MyDocuments/cvsroot/bin/ri.rb,v $
# $Revision: 1.11 $
# $Date: 2003/09/11 02:19:55 $
# $Author: pschaaf $
# $State: Exp $
# $Name:  $

puts(`ri`) && exit if ARGV.empty?

require 'lib/ansicolor'
require 'extn/Module'
require 'extn/String'
#require 'extn/Enumerable'

$ignore_ancestors = [Kernel]
$ignore_methods = %w(wordwrap)
$indent = 5
$divider ='-'*66
$undocumented_method_color = Color.yellow + Color.bold
$deprecated_method_color = Color.cyan

$ruby_keywords = %w(alias and begin break case catch class def do elsif else fail ensure for end if in module next not or raise redo rescue retry return then throw super unless undef until when while yield)

$not_deprecated = %w(abort at_exit autoload binding block_given? callcc caller chomp chomp! chop chop! eval exec exit exit! fork format gets global_variables gsub gsub! iterator? lambda load local_variables loop new open p print printf proc putc puts rand readline readlines require scan select set_trace_func singleton_method_added sleep split sprintf srand sub sub! syscall system test trace_var trap untrace_var)
$not_deprecated << '`'
$not_deprecated += Module.constants
$not_deprecated += $ruby_keywords
($ignore_ancestors << Object).each {|anc| $not_deprecated += anc.instance_methods(true)}

def dump(*args)
  args.each {|e| puts '', e.inspect, '' if ! block_given? || yield(e)}
  args[-1]
end

def namesClass?(string)
  [Module, Class].include?(Module.const_get(string).class)
rescue
  false
end

def section(name=nil, words=[], divider=nil)
  [(name || divider) && [$/], divider, name && [name], words] \
    .select  {|each| each && ! each.empty?} \
  .collect {|e| e.wordwrap(:indent=>$indent)}
end

def divided_section(name=nil, words=[])
  section(name, words, $divider)
end

def parse_output(output)
  output \
    .gsub(/\n/, '') \
    .split(/-{10,}/) \
    .reject {|e| e.empty?} \
    .collect {|e| e.strip!}
end

puts ARGV.collect {|arg|
  raw_output, err = `ri '#{arg}'`, $?.to_i
  raw_output.freeze

  if err > 0
    if arg == "classes"
      [divided_section('All Classes',  Class.allInstances.collect {|e| e.to_s}.sort!),
       divided_section('All Modules', Module.allInstances.collect {|e| e.to_s}.sort!)]
    else
      raw_output
    end

  elsif namesClass?(arg)
    thisClass = Module.const_get(arg)
    methods = {}
    ancestors = thisClass.ancestors.reverse - [thisClass]
    name, description, methods[:documented] = *parse_output(raw_output)

    methods[:all]          = thisClass.instance_methods(true) - $ignore_methods #+ thisClass.methods.collect {|cls_meth| "::#{cls_meth}"}
    methods[:inherited]    = []
    methods[:local]        = thisClass.instance_methods(false) - $ignore_methods
    methods[:documented]   = methods[:documented].split(/,?[\s ]+/)
    methods[:undocumented] = methods[:local]      - methods[:documented]
    methods[:deprecated]   = methods[:documented] - methods[:all] - $not_deprecated

    # ancestors section
    ancestors_section = ancestors.collect {|each|
      (sup_meths = each.instance_methods(false)).sort!
      methods[:inherited]    += sup_meths
      methods[:undocumented] -= sup_meths
      section("...from #{each.class.to_s.downcase} #{each}", sup_meths) unless $ignore_ancestors.include? each
      #each.methods.each {|cls_meth| methods[:inherited] << "::#{cls_meth}"}
    }.select {|each| each}
    ancestors_section.unshift([divided_section('Inherited')]) unless ancestors_section.empty?

    methods[:local] -= methods[:inherited]

    # description section
    description_section = divided_section((Color.bold {name}) + $/, description)

    # methods section
    methods_section = divided_section('Methods: Documented, Deprecated, and Undocumented', \
                                      (methods[:documented] + methods[:local]).sort!.uniq)
    methods_section.flatten.each {|each|
      each.highlight!(methods[:undocumented].collect {|e| Regexp.escape(e)} << /undocumented/i,
                      $undocumented_method_color)
      each.highlight!(methods[:deprecated].collect {|e| Regexp.escape(e)} << /deprecated/i,
                      $deprecated_method_color)
    }

    [description_section,
     methods_section,
     ancestors_section,
     divided_section(nil)]

  else
    raw_output
  end
}.select {|each| each}
