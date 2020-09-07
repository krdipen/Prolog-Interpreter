edge(one,two).
edge(two,three).
edge(three,four).
edge(three,five).
path(X,X).
path(X,Y): edge(X,Z), path(Z,Y).
