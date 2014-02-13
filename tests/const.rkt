this strategy uses the const? and ready? flags of toplevels to determine
whether a toplevel is needed.

look at all the toplevel expressions. call the toplevels they reference the
root set. recur through all the toplevel references and determine the 
transitive closure of reference. if all /uses?/ of the toplevel are const?
we can safely prune the other expressions (as it is apparent they must not
alter any of them) and toplevels.

if there is a non-const? within the set, fail (keep everything).
