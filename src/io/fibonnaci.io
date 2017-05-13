#!/usr/bin/env io
//  *- comment-start: "// "; comment-start-skip: "//+ *"; comment-style: "indent" -*-
// 0, 1, 2, 3, 4, 5, 6,  7
// 0, 1, 1, 2, 3, 5, 8, 13

// Iterative solution
Number fibonacci := method(
  if(self < 1, return 0)
  prev       := 0
  current    := 1
  (self-1) repeat(
    old_prev := prev
    prev     := current
    current  := prev + old_prev)
  current)

// Using a queue looks cleaner, but is probably slower
Number fib1 := method(
  seq := List clone append(0, 1)
  self repeat(
    seq append(seq first + seq second)
    seq removeFirst
  )
  seq first
)

Number testFib := method(
  writeln(self, " fib1      == ", self fib1)
  writeln(self, " fibonacci == ", self fibonacci, "\n"))

0 testFib
1 testFib
2 testFib
3 testFib
4 testFib
5 testFib
6 testFib
7 testFib
