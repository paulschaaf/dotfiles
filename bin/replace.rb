#!/usr/bin/env ruby

# $Source: e:/MyDocuments/cvsroot/src/Makefile,v $
# $Revision: 1.1 $
# $Date: 2003/08/30 00:16:28 $
# $Author: pschaaf $
# $State: Exp $
# $Name:  $

require "StringCollections.rb"

# print usage info
if (ARGV.length < 2) or (ARGV.length.modulo(2) == 1) then
  puts "Usage: #{File.basename($0)} <pattern1> <replacement1> <pattern2> <replacement2>..."
  puts <<VALID
Special replacement characters can be used:
   ReplacementText      Substitution
   \\1, \\2, ... \\9    The value matched by the nth grouped subexpression.
   \\&                  The last match.
   \\\`                  The part of the string before the match.
   \\\'                  The part of the string after the match.
   \\\+                  The highest-numbered group matched.
VALID
  exit 1
end

input = STDIN.gets(nil)
if input
	input.gsubAll!(*ARGV)
	puts input
end
