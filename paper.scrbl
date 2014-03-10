#lang scribble/sigplan @10pt @preprint
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

The declared language of a Racket module is itself a module with its own dependency graph, and even the ostensibly compact@racket[racket/base] language includes a significant amount of code in its graph.
Much of this code is unreferenced by the language-declaring module, however, so it can in principle be removed from the demodularized program.


@section{An Example}

Demodularization enables whole-program optimizations and eliminates module loading overhead while maintaining the runtime meaning of a program.
To understand how demodularization preserves the runtime meaning of a program, we first need to understand the runtime meaning of a modular Racket program.
Consider the program in Figure~\ref{main-rkt}.

@codeblock{
#lang racket/base
(require "queue.rkt")

(with-queue (1 2 3 4 5 6)
  (enqueue 4)
  (displayln (dequeue))
  (displayln (dequeue)))
}
This program imports a queue library through a \scheme{require} expression and then uses the library to do some queue operations.
Figures~\ref{queue-rkt}, \ref{long-queue-rkt}, and \ref{short-queue-rkt} contain the three modules that make up the queue library.
The library consists of a macro and two queue implementations, where the macro decides which implementation to use based on the length of the initial queue at compile time.

@codeblock{
#lang racket/base
(require (for-syntax racket/base
                     racket/syntax)
         "short-queue.rkt"
         "long-queue.rkt")

(define-syntax (with-queue stx)
  (syntax-case stx ()
    [(with-queue (v ...) e ...)
     (begin
       (define type 
         (if (> (length (syntax->list #'(v ...))) 5) 
	     'long 
             'short))
       (define make-queue 
         (format-id #'stx "make-~a-queue" type))
       (define enqueue (format-id #'stx "~a-enqueue" type))
       (define dequeue (format-id #'stx "~a-dequeue" type))
       #`(let ([q (#,make-queue v ...)])
           (define (#,(datum->syntax stx 'dequeue)) 
             (#,dequeue q))
           (define (#,(datum->syntax stx 'enqueue) x)
             (#,enqueue q x))
           e ...))]))

(provide with-queue)
}

@codeblock{
#lang racket/base
(define (make-long-queue . vs)
  .... make-vector ....)

(define (long-enqueue q v)
  .... vector-set! ....)

(define (long-dequeue q)
  .... vector-ref ....)

(provide (all-defined-out))
}
@codeblock{
#lang racket/base
(define (make-short-queue . vs)
  .... list ....)

(define (short-enqueue q v)
  .... cons ....)

(define (short-dequeue q)
  .... list-ref ....)

(provide (all-defined-out))
}

The Racket runtime evaluates this program by compiling and then running the main module.
Whenever the runtime encounters a \scheme{require} expression during compilation, it either loads an existing compiled version of the required module or compiles the required module.
Whenever the runtime encounters a macro expression during compilation, it expands the macro.
In this example, the runtime begins to compile \texttt{main.rkt} and loads the compiled version of \scheme{racket/base}. 
Next, it encounters the \scheme{require} expression for \texttt{queue.rkt} and compiles it.
Compilation of \texttt{queue.rkt} triggers compilation of both \texttt{short-queue.rkt} and \texttt{long-queue.rkt}. 

After finishing \texttt{queue.rkt}, the runtime returns to \texttt{main.rkt} and expands the \scheme{with-queue} macro.
The \scheme{with-queue} macro checks the length of the initial queue, which in this case is six, and chooses to use the \scheme{long-queue} implementation.
The macro expands into a \scheme{let} expression that binds the identifier \scheme{q} to the initial queue, along with internal definitions of \scheme{enqueue} and \scheme{dequeue} that use the \scheme{long-queue} implementations.
Figure~\ref{main-expanded-rkt} shows what \texttt{main.rkt} looks like after expansion.

@codeblock{
(module main racket/base
  (#%module-begin
   (require "queue.rkt")
   (let ((q (make-long-queue 1 2 3 4 5 6)))
     (define (dequeue) (long-dequeue q))
     (define (enqueue x) (long-enqueue q x))
     (enqueue 4)
     (displayln (dequeue))
     (displayln (dequeue)))))
}

After compiling the whole program, the Racket runtime evaluates the program by loading and executing the compiled main module.
As is the case with compilation, evaluation also follows the \scheme{require} expressions and runs required modules as it encounters them.
In this example, the runtime evaluates \texttt{main.rkt} and encounters the \scheme{require} expression for \texttt{queue.rkt}.
It then follows the \scheme{require}s for \texttt{short-queue.rkt} and \texttt{long-queue.rkt} and installs the definitions that those modules provide.
When evaluation returns to \texttt{queue.rkt}, nothing else happens because macros are only needed at compile time.
Finally, evaluation returns to \texttt{main.rkt} and the runtime evaluates the rest of the program.

A demodularized version of this program should contain all code that ran while evaluating the modular program, minus the module loading steps.
The demodularization algorithm starts with the compiled versions of all of the modules for the program, and then traces the \scheme{require} expressions and includes all runtime code into a single module in the order it encounters the code.
Figure~\ref{main-demod-rkt} shows what the single module looks like after running the demodularization algorithm on it.
There are no require statements and the macro definition is gone, but the all the code that ran during the evaluation of the modular version is there in the same order as before.

@codeblock{
(module main racket/base
  (#%module-begin
   (define (make-short-queue . vs)
    .... list ....)

   (define (short-enqueue q v)
    .... cons ....)

   (define (short-dequeue q)
    .... list-ref ....)

   (define (make-long-queue . vs)
    .... make-vector ....)

   (define (long-enqueue q v)
    .... vector-set! ....)

   (define (long-dequeue q)
    .... vector-ref ....)

   (let ((q (make-long-queue 1 2 3 4 5 6)))
     (define (dequeue) (long-dequeue q))
     (define (enqueue x) (long-enqueue q x))
     (enqueue 4)
     (displayln (dequeue))
     (displayln (dequeue)))))
}


This example is rather simple because it only uses a single macro, but in practice, Racket programs use many macros, even macros that use other macros in their implementations, creating a language tower.
The demodularization algorithm takes this into account by only gathering runtime code while tracing through \scheme{require} expressions.
The details about how this works is further explained in the operational semantics model in Chapter 3.

With all of the code in a single module, it is easy to see how standard optimizations such as inlining and dead code elminiation can reduce the module to the code in Figure~\ref{main-demod-opt-rkt}.
@codeblock{
(module main racket/base
  (#%module-begin
   (let ((q (.... make-vector .... 1 2 3 4 5 6 ....)))
     (.... vector-set .... 4 ....)
     (displayln (.... vector-ref ....))
     (displayln (.... vector-ref ....)))))
}
In this simple example, with constant folding this progam could be optimized even more, but even in programs with dynamic inputs, demodularization enables many optimizations that aren't possible when the program is separated into modules.

@section{Model}

We can understand the specifics of demodularization by describing it as an algorithm for a simple language with a well defined semantics.
The \emph{mod} language (Figure~\ref{source-lang}) contains only the features necessary to write modular programs where it is possible to observe the effects of module evaluation order.

\begin{figure}[h]
\includegraphics{source}
\caption{\emph{mod} language grammar}
\label{source-lang}
\end{figure}

A program in \emph{mod} consists of a list of modules that can refer to each other.
Each module has a name, any number of imports, any number of definitions, and sequenced code expressions. 
All definitions in a module are exposed as exports to other modules, but to use definitions from another module, the program must import it through a \scheme{require} expression.
Both \scheme{require} and \scheme{define} expressions have phase annotations; this simulates the interactions between modules in a language with macros and a language tower without requiring a model of macro expansion.
The language includes variable references, numbers, addition, and mutation.
Mutation makes module evaluation order observable, and addition represents the work that a module does.
In addition to numbers and variables, there are two special forms of values and references that model the interaction of macros with the module system.
A \scheme{quote} expression is like a reference to syntax at runtime.
A \scheme{ref} expression is like a macro that can only do one thing: refer to a variable at a phase.

The \emph{mod} language exposes phases as an integral part of the language, while languages like Racket keep phases obscured from the end user even though it uses phases during compiling and evaluating a program.
So, what is a phase?
In the discussion of the example program in section XXX, we used the terminology of runtime and compile-time.
Phases are just numerical designations for these terms, where runtime is phase 0 and compile-time is phase 1.
The reason phases are numbers is because phases exist outside of the range of 0 to 1.
Given that phase 1 is the compile-time for phase 0, we can extend this idea so that phase 2 is the compile-time for phase 1.
Conversely, compile-time code generates code for the phase below it, so it can refer to bindings at negative phases.
(Talk about relative phases)
Phases allow programmers to build syntactic abstractions that use other syntactic abstractions, creating a tower of intermediate languages.
The \emph{mod} language does not allow programmers to create language towers, but evaluating a \emph{mod} program uses the same mechanisms as evaluating a Racket program.

We have to compile \emph{mod} programs before demodularizing them, just like in the Racket implementation.
In Racket, compiling expands all macros in a program and changes definitions and variable references to refer to memory locations.
In \emph{mod}, compiling eliminates \scheme{ref} expressions, turns definitions into \scheme{set!} expressions, changes variable references to include module information, and sorts code into phases.
Compilation in both cases still leaves behind a relatively high-level language, but the language is free of syntactic extensions.
This is important for demodularization because otherwise macro expansion would have to be part of the algorithm, which would complicate it and possibly duplicate work.
The grammar in Figure~\ref{compiled-lang} specifies the compiled language for \emph{mod}.

@image{compiled-lang.pdf}

\begin{figure}[h]
\includegraphics{compiled-lang}
\caption{compiled language grammar}
\label{compiled-lang}
\end{figure}

We evaluate the compiled language using a small-step reduction semantics. 
Because the reduction rules are syntactic, we extend the compiled language further with evaluation contexts, a heap representation, and a stack representation to keep track of the order to instantiate modules.
These extensions are in Figure~\ref{compiled-eval-lang}.
An expression of the form:
\setspecialsymbol{sigma}{$\sigma$}
\begin{schemedisplay}
(sigma / (mod ...) / ((id phase) ...)  / ((id phase) ...))
\end{schemedisplay}
represents the state of the machine during evaluation.
$\sigma$ represents the heap of the program, and when evaluation finishes represents the output of the program.
The list of modules is the code of program in the compiled language.
The first list of \scheme{(id phase)} pairs is the list of modules to evaluate, and the second list is the modules that have already been evaluated.

\begin{figure}[h]
\includegraphics{compiled-eval-lang}
\caption{extensions to compiled language grammar}
\label{compiled-eval-lang}
\end{figure}

The reduction rules in Figure~\ref{eval-reduction} evaluate a compiled program that starts with an empty heap, the program code, a stack that contains the identifier of the main module at phase 0, and an empty completed module list. 

\begin{figure}[h]
\includegraphics[width=\textwidth]{eval-reduction}
\caption{modular evaluation}
\label{eval-reduction}
\end{figure}

The \emph{module require} rule matches a program with a \scheme{require} expression in the module at the top of the evaluation stack and evaluates it by removing the \scheme{require} expression from the module and pushing the required module onto the evaluation stack with the phase shifted appropriately.
The current module is still on the stack and will continue evaluating after the required module is done evaluating.
The subsequent rules all apply only when the phase relative to the main module is zero.
The \emph{var ref} rule looks up a variable in the heap and replaces the variable with its current value.
The \emph{add} rule replaces an addition expression of numbers with the result of computing their sum.
The \emph{set!} rule installs a value for a variable into the heap and reduces to the value.
When an expression is a value, the \emph{expression done} rule matches and removes the expression from the module.
When there are no more expressions left in a module, the \emph{module done} rule applies by removing the module from the program and placing a reference to it in the list of finished modules.
The \emph{module done already} rule applies when the current module on the stack is in the finished list, so that modules are not evaluated multiple times. 

Figure~\ref{demod-redex} shows the demodularization algorithm for the compiled language.
\begin{figure}[h]
\includegraphics[width=\textwidth]{demod-redex}
\caption{Demodularization algorithm}
\label{demod-redex}
\end{figure}

After demodularization, a program is essentially a prefix--a mutable array with a slot for each variable defined at the top level--and a sequence of top-level forms.
These forms may be [racket]require[] forms which introduce bindings from a portion of Racket's kernel language, [racket]def-values[] forms which update slots in the prefix, and expressions which are wrapped in a [racket]print-values[] by the compiler.

The bytecode language distinguishes between top-level and local references and assignments.

Within our test suite, 75% of top-level forms are devoted to the definition of a single identifier by a simple expression, such as a lambda expression, syntactic value, or a primitive value reference.

subsection elimination strategies

Based on the observation that 75% of top-level forms define a single identifier by a simple expression--an expression that won't diverge or exhibit any other side effects--and the fact that references to top-level values are explicit, we calculate the transive closure of top-level forms related by top-level reference and seeded by complex (as opposed to simple) expressions.
This strategy is sound...

we get the top-level expressions that we need and then we determine which top-level identifiers we need based on the definitions of those expressions. suppose we have def-values x y e and x is referenced by main. then we keep the buckets for x and y. we don't need to worry about expressions that reassign y; its inclusion is benign.

we might be too (too) conservative with this since top-level references might occur in operand position but we include all references of the definition body.

too conservative?
higher-order control flow problem

alternative strategy using purity analysis
augmenting the simple expressions with known simple forms such as the application of a primitive value to simple expressions
or the obvious extension that the application of pure primitive values is assumed to be free of side effects and so the evaluation of
- a simple expression is side-effect free
- the application of a pure primitive value to side-effect-free argument expressions is side-effect free
this probably covers a vast majority of the top-level definitions

we should also consider code size as well as the proportion of top-level definitions eliminated.

We can mark primval as pure or impure, but that is really too coarse. A paremeter can be invoked as a procedure with an argument, and such an operation is impure, but when invoked without an argument is pure. We really need to keep track of (1) whether the primitive value is a procedure and, if so, (2) its purity for different arities. We can parition the arities into known pure and impure (and be conservative with the pure set), and in many cases the number of arguments will be statically known. If the number of arguments is not statically known, as in the case of apply-values, we take the minimum purity level of all arities. 

A demodularized program consists of a prefix which serve as references to top-level definitions and a sequence of top-level forms.
Top-level definitions may be definitions put by the programmer or local procedures lifted from an enclosing lambda.
A form at the top-level may be a require of a racket kernel language module, a definition, or an expression.

The demodularization process may include modules which aren't actually used.
These modules would be culled, but are included as part of the base language.
Therefore, a demodularized program contains a vast amount of dead code.

We distinguish between [emph]simple[] and non-[emph]simple[] expressions.
A simple expression is one which evaluation will not exhibit side effects, including non-termination.
We cannot decide whether an expression is simple in general, so we consider a few different approximations.

We then consider the non-simple expressions as a root set and calculate the transitive closure by reference of this set.
This is possible since top-level references are distinguished from local references.
This sidesteps the control-flow problem at the expense of a great reduction in precision.

First approximation

An expression is simple if it is a syntactic value, a top-level reference, a primitive value, or a lambda expression.
A primitive value is one of the approximately 1200 values provided by the virtual machine, a vast majority of which are procedures.
A lambda expression includes case-lambda.
The transitive closure of the root set induced by this definition includes 90% of top-level forms; the calculation is hardly worth the effort.

Second approximation

For the second approximation, we once again distinguish simple expressions but broaden our definition of such.
We observe that many of the compiler-generated top-level definitions are the result of a primitive value application.
We classify primitive values according to their purity.
The purity of some primitive values, such as those which expose parameters, is sensitive to the number of arguments provided.
Thus, our classification must take all the arities of the procedure into account.
(We also assume that no primitive procedure will ever diverge.)
The extended definition of simple expressions includes branch, begin, install-value, let-void, let-one, and application where the operator is a primitive procedure and all the operands are simple.
This method includes 60% of top-level forms.

This result is a great improvement over the previous, but the elimination produced by manual inspection is over 90%.
We identify the modes of reasoning humans employ when eliminating such a large amount, and ...
We could easily choose a strategy which isn't sound, but is safe for the code admitted to the base language.

put in a table of size results


@section{Implementation}

The demodularization algorithm for the Racket module system operates on Racket bytecode. 
Racket's bytecode format is one step removed from the fully-expanded kernel language: instead of identifiers for bindings, it uses locations.
For toplevel bindings, these locations point to memory allocated for each module known as the module's prefix.
So, in \texttt{long-queue.rkt}, \scheme{make-long-queue} would be in prefix location 0 and \scheme{long-enqueue} would be in prefix location 1, and all the references to \scheme{make-long-queue} and \scheme{long-enqueue} are replaced with references to 0 and 1.
Like in the model, the algorithm combines all phase 0 code into a single module, but since the references are locations instead of identifiers, the locations of different modules overlap.
We solve this by extending the prefix of the main module to have locations for the required module's toplevel identifiers, and then adjusting the toplevel references in the required module to those new locations. 

After combining all the code for a program into a single module, we want to optimize it.
The existing optimizations for Racket operate on an intermediate form that is part way between fully-expanded code and bytecode. 
Therefore, to hook into the existing optimizations, we decompile the bytecode of the demodularized program into the intermediate form and then run it through the optimizer to produce bytecode once more.

Racket provides features that treat modules as first-class objects during runtime. 
For example, programs can load and evaluate modules at runtime through \scheme{dynamic-require}. 
These features can work with demodularization, but the onus is on the programmer to make sure to use the features in particular ways.
The main restriction is that the program cannot share a module that is part of the demodularized program and also part of a dynamically required module. 
This restriction may seem easy to follow in theory, but in practice it is hard because most modules rely on built-in Racket libraries that will be in both the static and dynamic parts of the program.

@section{Evaluation}

We tested our implementation of demodularization by selecting existing Racket programs and measuring their execution time before and after demodularization.
We also measured the memory usage and compiled bytecode size of the programs.
We ran the benchmarks on an Intel Core 2 Quad machine running Ubuntu and ran each program X times.
We expect programs to perform better based on how modular the program is, which we measure by counting the number of modules in a program's require graph and how many cross module references occur in the program.

Figure XXX shows the results of running this experiment on XXX Racket programs. 
On one end of the spectrum, there are programs like XXX which are already basically single module programs, so demodularization does little besides rerun the optimizer on the program. Running the optimizer again may have positive or negative effects on performance, it may unroll loops and inline definitions more aggressively the second time, but some of these ``optimizations" may hurt performance.
On the other end of the spectrum, highly modular programs like XXX perform much better after demodularization.
We expect performance to increase at a linear or even superlinear pace as modularity increases because of the extra information available to the optimizer.

This experiment uses only the existing Racket optimizations, which are intra-module optimizations.
Certain optimizations that are not worthwhile to do at the intra-module level have larger payoffs when applied to whole programs. 
With demodularization, we anticipate that new whole-program optimizations enabled by demodularization will increase performance even more.

@section{Related Work}
Prior work on whole-program optimization has come in two flavors, depending on how much access to the source code the optimizer has. The first approach assumes full access to the source code and is based on inlining. The second approach only has access to compiled modules and is based on combining modules.

The first approach is based on selectively inlining code across module boundaries because it has full access to the source code of the program \cite{258960,Chambers96whole-programoptimization}. Most of the focus of this approach is finding appropriate heuristics to inline certain functions without ballooning the size of the program and making sure the program still produces the same results. Resulting programs are not completely demodularized; they still have some calls to other modules. Specifically, Chambers et al. \cite{Chambers96whole-programoptimization} show how this approach applies to object-oriented languages like C++ and Java, where they are able to exploit properties of the class systems to choose what to inline. Blume and Appel \cite{258960} showed how to deal with inlining in the presence of higher order functions, to make sure the semantics of the program didn't change due to inlining. Their approach led to performance increases of around 8\%.

The second approach is taking already compiled modules, combining them into a single module, and optimizing the single module at link time \cite{sutter,727617}. Most of the work done with this approach optimized at the assembly code level, but because they were able to view the whole program, the performance increases were still valuable. 
The link-time optimization system by Sutter et al. \cite{sutter} achieves a 19\% speedup on C programs.
One of the reasons for starting with compiled modules is so that programs using multiple languages can be optimized in a common language, like the work done by Debray et al. \cite{727617} to combine a program written in both Scheme and Fortran. The main problem with this approach is that the common language has less information for optimization than the source code had. 
These approaches are similar to demodularization, but the operate at a lower level and work on languages without phased module systems.

@section{Conclusion}

Demodularization is a useful optimization for deploying modular programs. 
A programmer can write a modular program and get the benefits of separate compilation while devloping the program, and then get additional speedups by running the demodularizer on the completed program.
Demodularization also enables new optimizations that are not feasible to implement for modular programs.
Without module boundaries, inter-procedural analysis is much easier and worthwhile.
Also, dead code elmination works much better because the whole program is visible, while in a modular program, only dead code that is private to the module can be eliminated.

In the future, we would like to implement an aggressive dead code elimination algorithm for Racket.
We implemented a naive one that does not respect side effects, but shows the potential gains from this optimization; it is able to shrink Racket binaries down from about 2MB to about 100KB.
This promising result implies that other low-hanging optimizations should be possible on demodularized programs that can increase performance.


@section{Notes}

We need to explain that phased module systems make macros work, but our system doesn't have macros because it's not the focus. We can talk about how macros are gone in compiled code though.

Talk about how to handle name collisions between modules.
