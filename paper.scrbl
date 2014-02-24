#lang scribble/sigplan

@title{Enabling Optimizations through Demodularization}

@doi{}

@authorinfo["Kimball Germaine" 
	    "University of Utah" 
            ""]
@authorinfo["Blake Johnson" 
            "Brigham Young University" 
            "blake.johnson@byu.edu"]
@authorinfo["Jay McCarthy" 
            "Brigham Young University" 
            ""]

@section{Introduction}

Programmers should not have to sacrifice the software engineering goals of
modular design and good abstractions for performance.  Instead, their tools
should make running a well-designed program as efficient as possible.

Many languages provide features for creating modular programs which enable
separate compilation and module reuse.  Some languages provide expressive macro
systems, which enable programmers to extend the compiler in arbitrary ways.
Combining module systems with expressive macro systems allows programmers to
write modular programs with each module written in its own domain-specific
language.  A compiler for such a language must ensure that modular programs
have the same meaning independent of module compilation order.  A phased module
system, like the one described by Flatt [XXX] for Racket, is a way to allow
both separately compiled modules and expressive macros in a language.

Modular programs are difficult to optimize because the compiler has little to
no information about values that come from other modules when compiling a
module in isolation.  To facilitate optimization, the compiler can compute
extra information about module exports [XXX], selectively inline imports [XXX],
or do additional optimizations while linking compiled modules [XXX]. 

Our solution for enabling optimizations on modular programs is to remove
modules altogether through a process called demodularization. In a language
where all code is runtime code, a tool can demodularize a program by simply
concatenating all modules of the program (possibly handling naming conflicts as
well). In a language with macros, the tool must statically discover all of the
runtime code program by tracing through imports and including runtime code that
is referenced by expanded macros.

The downside of demodularization is that even simple modular programs become
very large if they use standard libraries. It would be possible to carefully
craft imports and exports such that demodularized programs contain exactly the
code that will run, but then module abstraction becomes leaky and
impractical. To make demodularization useful on large programs, we couple it
with dead-code elimination to make the resulting programs practical for
distribution and tractable for optimization.
