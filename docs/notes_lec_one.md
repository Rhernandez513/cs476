
Prof William Mansky
 
Website: https:///www.cs.uic.edu/~mansky/teaching/cs476/fa23
	=> Class schedule is on here
 
Anon in-class questions: Pollev.com/wmansky771
 
Lectures recorded
 
Piazza & Grade scope
 
Pre-Req: CS341 Functional Programming, CS 151 Logic and Proofs
 
// TODO => Skill up on Functional Programming
 
 
 
Content:
Programming Language Design
	• Up to now, we've been users of programming languages
	• We will look at programming languages as designers and implementers:  how should the language work and how would we get a computer to run it?
	• We will look at different type of languages and features, (imperative, OO, functional, pointers, etc.)
 
Structure of a language:
Syntax: concrete (what do programs look like? vs abstract (what are pieces of a program?)
 
Semantics: static (which programs make sense?) vs dynamic (what do programs do when we run them?) main focus of the class
 
Pragmatics: implementation, IDE and tool support (how can we actually make the semantics happen?)
 
 
Metalanguage
	• We want to describe how programming languages should work
	• We need a metalanguage: a language for talking about programming languages
	• Metalanguage 1: English
	• "The code x := y +z sets the value of x to the value of y plus the value of z"
	• Pros: intuitive, familiar, and easy to write
	• Cons, ambiguous, informal
 
Metalanguage: Inference rules
	• Mathematical logic
	• Pros: precise, formal
	• Cons, hard to read and write, hard to apply to apply to real programs
 
(epsilon, sigma) down arrow V               "if the value of E is V"
--------------------------------------------
(X := epsilon, sigma) down arrow sigma[x=>v]          "then X:=E sets  X to V"
 
 
Metalanguage: Interpreters
The OCaml Programming Language
 
Match s with
| Assign x e => update env x (eval env e)
 
	• Pros: precise, executable, designed to describe programming languages
	• Cons: can be tricky to write and has to producer a single answer
 
We will be doing:
Writing something in natural languages, then an inference rule, and then in OCaml, and we will learn to translate between these three metalanguages
 
Textbook:  Types and Programming Languages: Pierce 2002
 
Grading:
 
In-class exercise: 25%
Assignments: 60%
Final Project: 15%
Participation: up to 5% extra credit (asking questions in class, posting in Piazza, etc.)
Final grades will be curved up
 
Final project will basically be to design a new programming languages or extending some programming languages features we have discussed in class
 
Assignments:
Programming assignments: write an interpreter for a language feature, implement a type checker etc.
Written homework: try out logical systems, write proofs about programs
Each assignment submitted twice:
	• First submission: write as much as you can, receive full credit as long as effort is put in and feedback is received
	• Second: Actually test code and check work and grade on correctness after feedback
Collaboration encouraged but must write up your own solution, cite sources (incl ChatGPT)
Submitted and returned via GradeScope
 
 
Content:
The OCaml Programming Language
 
OCaml: A functional language in the "ML" (metalanguage) family
	• ML family also includes: SML, F#, F*, etc.
	• Designed to operate on elements of a programming language
Strongly typed functional language with references, based on lambda calculus with pattern-matching
 
 
OCaml comes with a REPL
 
	• https://try.ocamlpro.com
	• Commands are always terminated with 2 semicolons ";;"
 
"#quit;;" to stop REPL execution
 
HMWK 1
Posted on course website
Set up Ocaml programming language env and write some simple functions in Ocaml
First submission due Thurdday 8/24 at 11:59PM
Submit via Gradescope
 
 