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