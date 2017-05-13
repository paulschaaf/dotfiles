#!/usr/bin/env io

Builder := Object clone
Builder forward := method(
   writeln("<", call message name, ">")

   call message arguments foreach(
        arg,
        content := self doMessage(arg);
        if(content type == "Sequence", writeln(content)))

   writeln("</", call message name, ">"))

Builder ul(li("IO"),
           li("Lua",
              i("another day")
              i("study later")),
           li("JavaScript"))


//comment-styles's value is
((plain nil nil nil nil)
 (indent nil nil nil t)
 (indent-or-triple nil nil nil multi-char)
 (aligned nil t nil t)
 (multi-line t nil nil t)
 (extra-line t nil t t)
 (box nil t t t)
 (box-multi t t t t))

                               io                ruby
//comment-start                "# "              "# "
//comment-start-skip

n
n
           "\\|//+\\s-*"     "#+ *"
//font-lock-comment-start-skip nil               nil

//comment-end                  ""                ""
//comment-end-skip             nil               nil
//font-lock-comment-end-skip   nil               nil

//comment-style                multi-line        multi-line
block-comment-start            nil               nil
block-comment-end              nil               nil
comment-continue               nil               nil
comment-multi-line             nil               nil
comment-use-syntax             undecided         undecided
