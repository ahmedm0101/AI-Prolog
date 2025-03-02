parent(pam, bob). 
parent( tom, bob).
parent( tom, liz).
parent( bob, ann).
parent( bob, pat).
parent( bat, jim).
female( pam).
female( liz).
female( ann).
female(pat).
male( tom).
male( bob).
male( jim).
mother(X,Y)      :- parent(X,Y),female(X).
grandparent(X,Y) :- parent(X,Z),parent(Z,Y).
grandfather(X,Y) :- grandparent(X,Y),male(X).
grandmother(X,Y) :- grandparent(X,Y),female(X).
sister(X,Y)      :- parent(Z,X),parent(Z,Y),female(X),X\=Y.
brother(X,Y)     :- parent(Z,X),parent(Z,Y),male(X),X\=Y.