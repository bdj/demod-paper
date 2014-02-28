#lang scribble/sigplan
@require[scribble/manual]

@title{Enabling Optimizations through Demodularization}

@doi{}

@authorinfo["Kimball Germane" 
	    "University of Utah" 
            ""]
@authorinfo["Matthew Might" 
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
should make running a well-designed program as efficient as a program without
abstractions.

Many languages provide features for creating modular programs which enable
separate compilation and module reuse.  Some languages provide expressive macro
systems, which enable programmers to extend the compiler in arbitrary ways.
Combining module systems with expressive macro systems allows programmers to
write modular programs with each module written in its own domain-specific
language.  A compiler for such a language must ensure that modular programs
have the same meaning independent of module compilation order.  A phased module
system, like the one described by Flatt [XXX] for Racket, is a way to allow
both separately compiled modules and expressive macros in a language.

Separately compiled modules are difficult to optimize because the compiler has
limited information about imported values. Existing solutions for optimizing
separately compiled modules range from generating extra information about
imports and exports [XXX] to linking all the modules together and then
optimizing [XXX].

Our solution, which we call demodularization, lies on the linking end of the
continuum. We combine all necessary separately compiled modules into a single
module, thus providing the whole program for analysis and
optimization. Discovering all necessary modules in a phased module system
requires analyzing imports and exports across phases.

After combining a set of modules into a single program, the result can be quite
large. For example, a simple "Hello, World" program in Racket become a XXX MB
program after demodularization. We do a dead-code elimination pass on the
resulting program to make analysis and optimization more tractable. Our example
program becomes XXX KB after dead-code elimination.

@section{An Example}

@codeblock{
  #lang racket/base
  (require "queue.rkt")
  (with-queue (1 2 3 4 5 6)
    (enqueue 4)
    (displayln (dequeue))
    (displayln (dequeue)))
}

@section{Phased Module System}


@section{Demodularization}

@section{Dead-code Elimination}

After demodularization, a program is essentially a prefix--a mutable array with a slot for each variable defined at the top level--and a sequence of top-level forms.
These forms may be [racket]require[] forms which introduce bindings from a portion of Racket's kernel language, [racket]def-values[] forms which update slots in the prefix, and expressions which are wrapped in a [racket]print-values[] by the compiler.

The bytecode language distinguishes between top-level and local references and assignments.

Within our test suite, 75% of top-level forms are devoted to the definition of a single identifier by a simple expression, such as a lambda expression, syntactic value, or a primitive value reference.

subsection elimination strategies

Based on the observation that 75% of top-level forms define a single identifier by a simple expression--an expression that won't diverge or exhibit any other side effects--and the fact that references to top-level values are explicit, we calculate the transive closure of top-level forms related by top-level reference and seeded by complex (as opposed to simple) expressions.
This strategy is sound...

too conservative?
higher-order control flow problem

alternative strategy using purity analysis
augmenting the simple expressions with known simple forms such as the application of a primitive value to simple expressions
or the obvious extension that the application of pure primitive values is assumed to be free of side effects and so the evaluation of
- a simple expression is side-effect free
- the application of a pure primitive value to side-effect-free argument expressions is side-effect free
this probably covers a vast majority of the top-level definitions

we should also consider code size as well as the proportion of top-level definitions eliminated.

@section{Implementation}

@section{Results}

