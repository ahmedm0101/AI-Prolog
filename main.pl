% #The Sex of Arguments Relations
male(tom).
male(bob).
male(jim).
male(alli).
female(pam).
female(liz).
female(pat).
female(ann).
% #------------------------------------------------------------------------
% #The Parent Relations
parent(tom,liz).
parent(tom,bob).
parent(pam,bob).
parent(pam,alli).
parent(bob,pat).
parent(bob,ann).
parent(pat,jim).
% #------------------------------------------------------------------------
% #The Relation GrandFather & GrandMother
grandfather(X,Z):-parent(Y,Z),parent(X,Y),male(X).
grandfather1(X,Z):-parent(Y,Z),father(X,Y).
grandmother(X,Z):-parent(Y,Z),parent(X,Y),female(X).
grandmother1(X,Z):-parent(Y,Z),mother(X,Y).
% #------------------------------------------------------------------------
% #The Relation Father & Mother
father(X,Y):- parent(X,Y),male(X).
mother(X,Y):- parent(X,Y),female(X).
% #------------------------------------------------------------------------
% #The Relation Brother & Sister & Sibling
brother(X,Y):-parent(Z,X),parent(Z,Y),male(X),X\=Y.
sister(X,Y):-parent(Z,X),parent(Z,Y),female(X),X\=Y.
sibling(X, Y):- parent(Z, X) ,parent(Z,Y), X \= Y.
% #------------------------------------------------------------------------
% #The Relation Uncle & Auntie For Father
uncleFather(X,Z):-parent(Y,Z),male(Y),parent(K,X),parent(K,Y),male(X),X\=Y.
uncleFather1(X,Z):-parent(Y,Z),male(Y),brother(X,Y).
auntieFather(X,Z):-parent(Y,Z),male(Y),parent(K,X),parent(K,Y),female(X),X\=Y.
auntieFather1(X,Z):-parent(Y,Z),male(Y),sister(X,Y).
% #------------------------------------------------------------------------
% #The Relation Uncle & Auntie For Father
uncleMother(X,Z):-parent(Y,Z),female(Y),parent(K,X),parent(K,Y),male(X),X\=Y.
uncleMother1(X,Z):-parent(Y,Z),female(Y),brother(X,Y).
auntieMother(X,Z):-parent(Y,Z),female(Y),parent(K,X),parent(K,Y),female(X),X\=Y.
auntieMother1(X,Z):-parent(Y,Z),female(Y),sister(X,Y).
% #------------------------------------------------------------------------
% #The Relation of True and False Statement & Wife & Offspring
offspring(X,Y):- parent(Y,X).
has_child(X):-parent(X,_).
has_child1(X):-parent(X,_).
wife(X,Y):-parent(X,Z), parent(Y,Z),female(X),male(Y).
% #------------------------------------------------------------------------
% #Acestor & Recursive Rule 
ancestor(X,Y):-parent(X,Y).
ancestor(X,Y):-parent(X,Z),ancestor(Z,Y).
% #------------------------------------------------------------------------
% #Matching 
vertical( line( p1(X,Y1) , p2(X,Y2) ),Y1\=Y2).
horizontal( line( p1(X1,Y) , p2(X2,Y) ),X1\=X2).
% #-------------------------------------------------------------------------------------------------------
% #                        * important Problems in List *
% #-------------------------------------------------------------------------------------------------------
% #get The Length of The List 
size([],0).
size([_|T],X):-size(T,X1), X is X1+1.
% #------------------------------------------------------------------------
% #get The Index of The Element in The List
indexF([X|_],X,0):-!.
indexF([_|T],X,Index):-indexF(T,X,Index1),Index is Index1+1.
% #------------------------------------------------------------------------
% #get The Element opposite To Index of This Element in The List
indexV([X|_],0,X):-!.
indexV([_|T],Index,X):-Index>0,Index1 is Index-1,indexV(T,Index1,X).
% #------------------------------------------------------------------------
% #The Count of Repeated Element in The List
count([],_,0):-!.  
count([X|T],X,Count):-count(T,X,Count1),Count is Count1+1,!.
count([H|T],X,Count):-count(T,X,Count),H\=X.
% #------------------------------------------------------------------------
% #get The Sum of Elements in The List
sum([],0).
sum([H|T],X):-sum(T,X1), X is X1+H.
% #------------------------------------------------------------------------
% #get The Multiplication of Elements in The List
multi([],1).
multi([H|T],X):-multi(T,X1), X is X1*H.
% #------------------------------------------------------------------------
% #get The Max Element in The List
max([Max],Max):-!.
max([H1,H2|T],Max):-H1>H2,max([H1|T],Max),!.
max([H1,H2|T],Max):-H1<H2,max([H2|T],Max).
% #------------------------------------------------------------------------
% #get The Min Element in The List
min([Min],Min):-!.
min([H1,H2|T],Min):-H1>H2,min([H2|T],Min),!.
min([H1,H2|T],Min):-H1<H2,min([H1|T],Min).
% #------------------------------------------------------------------------
% #Even Elements in The List
even([], []):-!.
even([H|T], [H|E]) :- 0 is H mod 2 , even(T, E),!.
even([H|T], E) :-     1 is H mod 2 , even(T, E).
% #------------------------------------------------------------------------
% #Count of Even Elements in The List
countEven([], 0).
countEven([H|T], Count) :- 0 is H mod 2,countEven(T, Count1),Count is Count1 + 1,!.
countEven([_|T], Count) :-countEven(T, Count).
% #------------------------------------------------------------------------
% #Odd Elements in The List
odd([], []):-!.
odd([H|T], [H|O]):- 1 is H mod 2,odd(T, O),!.
odd([H|T], O):- 0 is H mod 2, odd(T, O).
% #------------------------------------------------------------------------
% #Count of Odd Elements in The List
countOdd([], 0).
countOdd([H|T], Count) :- 1 is H mod 2,countOdd(T, Count1),Count is Count1 + 1,!.
countOdd([_|T], Count) :-countOdd(T, Count).
% #------------------------------------------------------------------------
% #Separeted Positive and Negative Number From any List
splitF([], [], []):-!.
splitF([H|T], [H|X], Y) :- H >= 0 , splitF(T, X, Y),!.
splitF([H|T], X, [H|Y]) :- H < 0  , splitF(T, X, Y).
% #------------------------------------------------------------------------
% #The Positive Element in The List 
positive([],[]):-!.
positive([X|T],[X|P]):-X>=0,positive(T,P),!.
positive([_|T],P):-positive(T,P).
% #------------------------------------------------------------------------
% #The Count of Positive Element in The List 
countPositive([],0):-!.
countPositive([X|T],Count):-X>=0,countPositive(T,Count1),Count is Count1+1,!.
countPositive([_|T],Count):-countPositive(T,Count).
% #------------------------------------------------------------------------
% #The Negative Element in The List 
negative([],[]):-!.
negative([X|T],[X|P]):-X<0,negative(T,P),!.
negative([_|T],P):-negative(T,P).
% #------------------------------------------------------------------------
% #The Count of Negative Element in The List 
countNegative([],0):-!.
countNegative([X|T],Count):- X<0,countNegative(T,Count1),Count is Count1+1,!.
countNegative([_|T],Count):- countNegative(T,Count).
% #------------------------------------------------------------------------
% #Increment Every Element in The List
list1([],[]):-!.
list1([H1|T1],[H2|T2]):-H2 is H1+1,list1(T1,T2).
% #------------------------------------------------------------------------
% #Decrement Every Element in The List
list2([],[]):-!.
list2([H1|T1],[H2|T2]):-H2 is H1-1,list2(T1,T2).
% #------------------------------------------------------------------------
% #Multiplication Every Element By 2 in The List 
list3([],[]):-!.
list3([H1|T1],[H2|T2]):-H2 is H1*2,list3(T1,T2).
% #------------------------------------------------------------------------
% #Divisible Every Element By 2 in The List 
list4([],[]):-!.
list4([H1|T1],[H2|T2]):-H2 is H1/2,list4(T1,T2).
% #------------------------------------------------------------------------
% #Power Every Element By 2 in The List 
list5([],[]):-!.
list5([H1|T1],[H2|T2]):-H2 is H1**2,list5(T1,T2).
% #------------------------------------------------------------------------
% #Power Every Element By 2 in The List 
factorialM(0,1):-!.
factorialM(N,R):-N>0,N1 is N-1,factorialM(N1,R1),R is N*R1.
list6([],[]):-!.
list6([H1|T1],[H2|T2]):-factorialM(H1,H2),list6(T1,T2).
% #------------------------------------------------------------------------
% #Reverse The Element of The List
reverseList([],[]).
reverseList([H|T],R):-reverseList(T,T1),append(T1,[H],R). 
% #------------------------------------------------------------------------
% # Palindrome The Element of The List
palindrome(L):-reverseList(L,R),L=R.
% #------------------------------------------------------------------------
% #Delete an Element From The List
del([X|L],X,L).
del([H|T1],X,[H|T2]):-del(T1,X,T2).
% #------------------------------------------------------------------------
% #Member Element in The List method (1)
member1([X|_],X):-!.
member1([_|T],X):-member1(T,X).
% #------------------------------------------------------------------------
% #Member Element in The List method (2)
member2(L,X):-del(L,X,_).
% #------------------------------------------------------------------------
% #Member Element in The List method (3)
member3([X|_],X):-!.
member3(L,X):-del(L,X,_).
% #------------------------------------------------------------------------
% #Ordered Element in The List
ordered([]):-!.
ordered([_]):-!.
ordered([X,Y|T]):- X =< Y,ordered([Y|T]).
% #------------------------------------------------------------------------
% #add Element in The First Position in List
addFirst(L,X,[X|L]).
% #------------------------------------------------------------------------
% #add Element in The Second Position in List
addSecond([H|T],X,[H,X|T]).
% #------------------------------------------------------------------------
% #add Element in The Third Position in List
addThird([H1,H2|T],X,[H1,H2,X|T]).
% #------------------------------------------------------------------------
% #add Element in The Last Position in List
addLast([],X,[X]).
addLast([H|T1],X,[H|T2]):-addLast(T1,X,T2).
% #------------------------------------------------------------------------
% #add Element in The List With Deplication By use Del-Function
add1(L1,X,L2):-del(L2,X,L1),!.
add1([],X,[X]).
% #------------------------------------------------------------------------
% #add Element in The List Without Deplication by use Member1-Function
add2(L,X,L):-member1(L,X),!.
add2(L,X,[X|L]).
% #------------------------------------------------------------------------
% #get an insert an Element of Delete Without Backtracking of Del-Function
dele([X|T],X,T).
dele([H|T1],X,[H|T2]):-dele(T1,X,T2).
insert(L1,X,L2):-dele(L2,X,L1).
% #------------------------------------------------------------------------
% #get The Concatnation between Two List
concat([],X,X).
concat([H|T1],X,[H|T2]):-concat(T1,X,T2).
% #Member Element in The List method (4)
member4(L,X):-concat(_,[X|_],L).
% #------------------------------------------------------------------------
% #SubList of List 
subList(X,L):-concat(_,L2,L),concat(X,_,L2).
% #------------------------------------------------------------------------
% #SubSum of List 
subsum([], 0, []).
subsum([X|T], S, [X|S1]) :- S >= X , Sum is S - X , subsum(T, Sum, S1).
subsum([_|T], S, S1) :- subsum(T, S, S1).
% #------------------------------------------------------------------------
% #Sotring Elements List 
sortList([], []).
sortList([H|T], S) :-sortList(T, S1),sortF(H, S1, S).
sortF(X, [], [X]).
sortF(X, [H|T], [X,H|T]) :- X =< H.
sortF(X, [H|T1], [H|T2]) :- X > H,sortF(X, T1, T2).
% #------------------------------------------------------------------------
% #get The Elements Between Two Integer Number
between(N1, N2, X) :- N1 < N2 , Next is N1 + 1 , X = Next.
between(N1, N2, X) :- N1 < N2 , Next is N1 + 1 , between(Next, N2, X).
% #------------------------------------------------------------------------
% #get GCD of Two Number 
gcd(A, 0, A) :- A > 0.
gcd(A, B, G) :- B > 0 , Rem is A mod B , gcd(B, Rem, G).
% #------------------------------------------------------------------------
% #Power of Two Number 
power(_, 0, 1). 
power(X, N, Result) :- N > 0, N1 is N - 1 , power(X, N1, Result1) , 
                       Result is X * Result1.
% #------------------------------------------------------------------------
% #Square Root of The Number 
square(X,Y):- Y is sqrt(X).
% #------------------------------------------------------------------------
% #Fibonacci sequence
fib(0, 0).
fib(1, 1).
fib(N, R):- N > 1 , N1 is N - 1 , N2 is N - 2 , 
            fib(N1, R1) , fib(N2, R2) , R is R1 + R2.
% #------------------------------------------------------------------------
% #Factorial Method 
factorial(0,1):-!.
factorial(N,R):-N>0,N1 is N-1,factorial(N1,R1),R is N*R1.
% #-------------------------------------------------------------------------
% # Important Build-In Function for List 
% #----------------------------------------------
% # 1)length(List , Value_of_Length_List).
% # 2)sort(List , NewList).
% # 3)delete(List,element,NewList).
% # 4)member(Element , List).
% # 5)memberchk(Element , List).
% # 6)append(List1 , List2 , NewList).
% # 7)reverse(List , NewList).
% # 8)select(Element , List , NewList).
% # 9)sqrt(Element,NewElement).
% # 10)permutation(List , NewElement).
% #-------------------------------------------------------------------------
% # Important Operator notation 
% #----------------------------------------------
% # 1) equal =>        =:= 
% # 2) matching =>      = 
% # 3) not equal =>    \=
% # 4) And operator =>  ,
% # 5) if operator =>   :-
% # 6) Or operator =>   ;
% # 7) power  =>        **
% # 8) Not operator =>  not
% # 9) reminder  =>     mod
% # 10) Double Division =>           /
% # 11) Integer Division =>         //
% # 12) Addition operator =>        +
% # 13) Subtraction operator =>     -
% # 14) multiplication operator =>  *
% # 15) greater than or equal =>    >=
% # 16) less than or equal   =>     =<
% # 17) greater than   =>           >
% # 18) less than      =>           <
% #-------------------------------------------------------------------------------------------------------
% #                          * Backtracking Problems *
% #-------------------------------------------------------------------------------------------------------
% #Program 1 in Backtracking !
f(X,excellent):-X>=85,!.
f(X,very_good):-X>=75,!.
f(X,good):-X>=65,!.
f(X,aceptable):-X>=60,!.
f(X,fail):-X<60.
% #------------------------------------------------------------------------
% #Program 2 in Backtracking !
license(X,'You Can get The License'):-X>=18,!.
license(X,'You Can not get The License'):-X<18.
% #------------------------------------------------------------------------
% #Program 3 in Backtracking !
p(1).
p(2):-!.
p(3).
% #------------------------------------------------------------------------
% # Program 4 in Backtracking !
max2num1(X,Y,X):-X>=Y,!.
max2num1(_,Y,Y).
%#-------
max2num2( X, Y, X) :- X >= Y.
max2num2( X, Y, Y) :- X < Y.
% #!
max2num(X,Y,Z):-X>=Y,!,Z=X;Z=Y.
%#-------
min2num1(X,Y,X):-X=<Y,!.
min2num1(_,Y,Y).
%#-------
min2num2( X, Y, X) :- X =< Y.
min2num2( X, Y, Y) :- X > Y.
% #!
min2num(X,Y,Z):-X=<Y,!,Z=X;Z=Y.
% #------------------------------------------------------------------------
% #Program 5 in Backtracking !
fb(X,2):-X>6,!,X<20.
fb(X,4):-X>4.
fb(X,6):-X>5,!.
fb(X,8):-X>8.
% #------------------------------------------------------------------------
% #Program 6 in Backtracking !
beat( tom, jim). 
beat( ann, tom).
beat( pat, jim). 
winner(X):-beat(X,_),!.
fighter(X):-beat(X,_);beat(_,X),!.
sportsman(X):-beat(_,X).
class(X,winner):-beat(X,_),not(beat(_,X)),!.
class(X,fighter):-beat(X,_),beat(_,X),!.
class(X,sportsman):-not(beat(X,_)),beat(_,X),!.
% #------------------------------------------------------------------------
% #Program 6 in Backtracking !
different1(X,Y):-X\=Y,!,true;fail.
different2(X,Y):-X=:=Y,!,fail;true.
different3(X,Y):-X=Y,!,fail;true.
% #------------------------------------------------------------------------
% # bagof , setof , findall 
age(peter,7).
age(ann,5).
age(pat,8).
age(tom,5).
age(pater,10).
age(pat,6).
% #-------------------------------------------------------------------------------------------------------
% #                         Processing files of term Read and write
% #-------------------------------------------------------------------------------------------------------
cout(Text):-write(Text).
system_out_println(Text):-write(Text),nl.
printf(Text1,Text2):-write(Text1),nl,write(Text2).
% #-------------------------------------------------------------------------
% # Write 
% #--------
get_Method(X,Y,Z):- Sum is X+Y+Z,AVG is (X+Y+Z)/3,Multi is X*Y*Z,
                    write('The Sum of '),write(X),write(','),write(Y),write(','),write(Z),
                    write(' is : '),write(Sum),nl,
                    write('The Avg of '),write(X),write(','),write(Y),write(','),write(Z),
                    write(' is   : '),write(AVG),nl,
                    write('The Multi of '),write(X),write(','),write(Y),write(','),write(Z),
                    write(' is : '),write(Multi).

square(X):- Fact is X*X,write('The Square of '),write(X),write(' is : '),write(Fact).

cube(X):- Fact is X*X*X,write('The Cube of '),write(X),write(' is : '),write(Fact).

square_root(X):- Root is sqrt(X),write('The Square Root of '),write(X),write(' is : '),
                 write(Root).
max_number(X,Y):- X>=Y ,!, write('The Max is : '),write(X);write('The Max is '),write(Y).
max_number1(X,Y):-process(X,Y,Max),write("The Max is : "),write(Max).
                  process(X,Y,X):-X>=Y,!.
                  process(_,Y,Y).

min_number(X,Y):- X=<Y ,!, write('The Min is : '),write(X);write('The Min is '),write(Y).
min_number1(X,Y):-process1(X,Y,Min),write("The Min is : "),write(Min).
                  process1(X,Y,X):-X=<Y,!.
                  process1(_,Y,Y).

abs(X):-X<0, Abs is -1*X ,write('The Absolute of '),write(X),write(' is : '),write(Abs).
%#------------------------ Note
writelist([]).
writelist([X|L]) :- write( X), nl, writelist( L).
%#------------------------ Note
writelist2([]).
writelist2([L|LL]) :- doline( L), nl, writelist2( LL).
doline([]).
doline([X|L]) :- write( X), tab(1), doline( L).

%#------------------------ Note
bars([]).
bars([N|L]) :- stars( N), nl, bars( L).
stars( N) :- N > 0, write( *), N1 is N-1, stars(N1).
stars( N) :- N =<0.
% #-------------------------------------------------------------------------
% # Read
% #--------
get_MethodR:- write('Enter The First Number : '),read(X),
              write('Enter The Second Number : '),read(Y),
              write('Enter The Second Number : '),read(Z),processor(X,Y,Z).
              processor(X,Y,Z):-Sum is X+Y+Z,Avg is (X+Y+Z)/3,Multi is X*Y*Z,
              write("The Sum is : "),write(Sum),nl,
              write("The Avg is : "),write(Avg),nl,
              write("The Multi is : "),write(Multi).

gcdR:- write('Enter The First Number : '), read(A), write('Enter The Second Number : '), read(B), 
       processing(A, B, GCD),write('The gcd is : '),write(GCD).
       processing(A, 0, A):- A > 0.
       processing(A, B, GCD):- B > 0,Rem is A mod B,processing(B, Rem, GCD).

fibR:- write('Enter The Number : '),read(N),process(N, R), 
       write('The Fibonacci of '),write(N), write(' is : '),write(R).
       process(0, 0):- !.
       process(1, 1):- !.
       process(N, R):- N > 1,N1 is N - 1,N2 is N - 2,process(N1, R1),process(N2, R2),R is R1 + R2.

cubeR:- write('Enter The Number : '),read(X),processor(X).
        processor(0):-!.
        processor(X):-Cube is X*X*X ,write("The Cube of "),write(X),write(' is '),
        write(Cube),nl,cubeR.

squareR:- write("Enter The Number : "),read(X),processes(X).
          processes(0):-!.
          processes(X):-Square is X*X ,write("The Square of "),write(X),write(' is : '),
          write(Square),nl,squareR.
% #------------------------------------------------------------------------
% #Delete Spacing From Text
squeeze :- get0( C), put( C), dorest( C).
dorest( 46) :- !.
dorest( 32) :- !, get( C), put( C), dorest( C).
dorest( _) :- squeeze.



























