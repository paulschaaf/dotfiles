#!/usr/bin/env ruby

require "rexml/document"
include REXML

# file = File.new("IssueNavigator.jspa.xml")
# doc = Document.new(file)

doc = Document.new File.new("mydoc.xml")
root = doc.root

doc.elements.each("inventory/section") { |element| puts element.attributes["name"] }
# -> health
# -> food

doc.elements.each("*/section/item") { |element| puts element.attributes["upc"] }
# -> 123456789
# -> 445322344
# -> 485672034
# -> 132957764

puts root.attributes["title"]
# -> OmniCorp Store #45x10^3

puts root.elements["section/item[@stock='44']"].attributes["upc"]
# -> 132957764

puts root.elements["section"].attributes["name"]
# -> health (returns the first encountered matching element)

puts root.elements[1].attributes["name"]
# -> health (returns the FIRST child element)

root.detect {|node| node.kind_of? Element and node.attributes["name"] == "food" }

doc.elements.delete("inventory/section/item") { |element| p element}

doc = Document.new File.new("IssueNavigator.jspa.xml")
root = doc.root
