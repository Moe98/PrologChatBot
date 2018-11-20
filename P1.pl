:-include("C:/Users/Mohamed A. Abu Atala/Desktop/Prolog stuff/info_food.pl").
:-include("C:/Users/Mohamed A. Abu Atala/Desktop/Prolog stuff/read_sentence.pl").

%prop(dark_matter,contain,0,cal).

startCalories(1800).

readInputTillQuit :-
	ws(['>','Welcome',to,your,personal,assistant,'.','\n']),
	readInputTillQuit([],[]).

readInputTillQuit(PQ,PR) :-
	ws(['> ']),
	res(I),
	evaluate(I,PQ,PR).

evaluate(I,PQ,PR) :-
	I = [quit,'.'],
	append(PQ,PR,HL),
	ws(['>','You','had ']),
	eatenIn(breakfast,HL,LB),
	(ws(LB) ; LB = [] , ws(['-'])),
	ws([' for',breakfast,'\n','You','had ']),
	eatenIn(lunch,HL,LL),
	(ws(LL) ; LL = [] , ws(['-'])),
	ws([' for',lunch,'\n','You','had ']),
	eatenIn(dinner,HL,LD),
	(ws(LD) ; LD = [] , ws(['-'])),
	ws([' for',dinner,'\n','Bye']).
evaluate(I,PQ,PR) :-
	(append(Q,['?'],I);append(Q,['.'],I)),
	isValid(Q),
	append(PQ,[Q],NPQ),
	response(Q,PQ,PR,R),
	append(PR,[R],NPR),
	ws(['> ']),
	ws(R),
	ws(['\n']),
	readInputTillQuit(NPQ,NPR).
evaluate(I,PQ,PR) :-
	(append(Q,['?'],I);append(Q,['.'],I)),
	\+isValid(Q),
	ws(['>','I',can,not,understand,you,'\n']),
	readInputTillQuit(PQ,PR).

eatenIn(_,[],[]).
eatenIn(M,[H|T],[X|FL]) :-
	(H = [i,ate,X,for,M] ; H = ['You',can,have,X,for,M]),
	eatenIn(M,T,FL).
eatenIn(M,[H|T],FL) :-
	\+(H = [i,ate,X,for,M] ; H = ['You',can,have,X,for,M]),
	eatenIn(M,T,FL).

isValid([how,many,calories,does,_,contain]).
isValid([what,does,_,contain]).
isValid([can,i,have,_,for,M]) :-
	M = breakfast ; M = lunch ; M = dinner.
isValid([what,is,_]).
isValid([how,many,calories,do,i,have,left]).
isValid([what,kind,of,CT,does,_,contain]) :-
	prop(_,is,CT),!.
isValid([is,_,a,CT,in,_]) :-
	prop(_,is,CT),!.
isValid([what,can,i,have,for,M,that,contains,_]) :-
	M = breakfast ; M = lunch ; M = dinner.
isValid([i,ate,_,for,M]) :-
	M = breakfast ; M = lunch ; M = dinner.
isValid([i,do,not,eat,_]).

filterProp(Relation,Result) :-
	setof((X,Y) , prop(X,Relation,Y) , Result).

matchFirst(_,[],[]).
matchFirst(T1,LF,LM) :-
	LF = [(X,Y)|R],
	LM = [Y-H|T],
	(T1 = X , H = 1;
	 T1\= X , H = 0),
	matchFirst(T1,R,T).

matchSecond(_,[],[]).
matchSecond(T1,LF,LM) :-
	LF = [(Y,X)|R],
	LM = [Y-H|T],
	(T1 = X , H = 1;
	 T1\= X , H = 0),
	matchSecond(T1,R,T).

mergeMatchLists(ML1,ML2,R):-
	reduce(ML1,NML1),
	reduce(ML2,NML2),
	removeDup(NML1,L1),
	removeDup(NML2,L2),
	result(L1,L2,[],R).

reduceH(_,[],_-0).
reduceH(H-N,[H-N3|T],H-N1):-
	reduceH(H-N,T,H-N2),
	N1 is N2+N3.
reduceH(H-N,[H1-_|T],H-N1):-
	H\=H1,
	reduceH(H-N,T,H-N2),
	N1 is N2.

reduce([],[]).
reduce([H-N|T],[H1-N3|R]):-
	reduceH(H-N,T,H1-N1),
	N3 is N1+N,
	reduce(T,R).

removeDupH([],[]).
removeDupH([H-_|T],R):-
	member(H-_,T),
	removeDupH(T,R).
removeDupH([H-N|T],[H-N|R]):-
	\+member(H-N,T),
	removeDupH(T,R).
removeDup(L,R):-
	reverse(L,R1),
	removeDupH(R1,R2),
	reverse(R2,R).

result([],[],[],[]).
result([],R1,Temp,R):-
	append(Temp,R1,R).
result(R1,[],Temp,R):-
	append(Temp,R1,R).
result([H1-N1|T],L,Temp,[H1-N1|R]):-
	\+member(H1-_,L),
	result(T,L,Temp,R).
result([H1-N1|T1],[H2-N2|T2],[H2-N2|Temp],R):-
	H1\=H2,
	result([H1-N1|T1],T2,Temp,R).
result([H-N1|T1],[H-N2|T2],Temp,[H-N3|R]):-
	N3 is N1+N2,
	append(Temp,T2,L),
	result(T1,L,[],R).

bestMatches([H-N|T],R):-
	maxN([H-N|T],N1),
	bestH([H-N|T],N1,R).

bestMatchesMinH([],_,[]).
bestMatchesMinH([H-N|T],N1,[H|R]):-
	N>=N1,
	bestMatchesMinH(T,N1,R).
bestMatchesMinH([_-N|T],N1,R):-
	N<N1,
	bestMatchesMinH(T,N1,R).
bestMatchesMin(L,N,R):-
	maxN(L,N1),
	N1>=N,
	bestMatchesMinH(L,N1,R).
bestMatchesMin(L,N,R):-
	maxN(L,N1),
	N1<N,
	R=[].

maxN([],R,R).
maxN([_-N|T], L, R):-
	N >  L,
	maxN(T,N,R).
maxN([_-N|T], L, R):-
	N =< L,
	maxN(T,L,R).
maxN([_-N|T], R):-
	maxN(T,N,R).

bestH([],_,[]).
bestH([H-N|T],N,[H|R]):-
	bestH(T,N,R).
bestH([_-N|T],N1,R):-
	N\=N1,
	bestH(T,N1,R).

foodCal(F,C):-
	prop(F,contain,C,cal).
foodCal(F,C):-
	has(F,L),
	foodCalList(L,C).

has(F,L):-
	setof(T,prop(F,contain,T),L).

/*calculate([],0).
calculate([H|T],C):-
	prop(H,contain,N,cal),
	calculate(T,C1),
	C is N+C1.*/

foodCalList([],0).
foodCalList([H|T],C):-
	foodCal(H,N),
	foodCalList(T,C1),
	C is N+C1.

calcCalories(F1,PQ,PR,C):-
	foodCal(F1,C1),
	append(PQ,PR,HL),
	foodFromHistory(HL,FL),
	foodCalList(FL,C2),
	startCalories(S),
	C is S-C1-C2.

getDiffAnswer(Q,PQ,PR,CR,DR):-
	traverse(Q,PQ,PR,TR),
	setof(R,(member(R,CR),\+member(R,TR)),RL),
	reverse(RL,[DR|_]).

traverse(_,[],[],[]).
traverse(Q,[Q|PQ],[[R]|PR],[R|T]) :-
	traverse(Q,PQ,PR,T).
traverse(AQ,[Q|PQ],[_|PR],T) :-
	AQ \= Q,
	traverse(AQ,PQ,PR,T).

	/*PQ = [_|_],
	lengthQuestion(Q,PQ,X),
	element(X,CR,R).

element(0,[H|_],H).
element(X,[_|T],R):-
	X > 0,
	X1 is X-1,
	element(X1,T,R).

lengthQuestion(_,[],0).
lengthQuestion(H,[H|T],X):-
	lengthQuestion(H,T,X1),
	X is X1+1.
lengthQuestion(H,[Y|T],X):-
	H \= Y,
	lengthQuestion(H,T,X).*/

response(Q,_,_,['I',do,not,know]) :-
	Q = [how,many,calories,does,X,contain],
	\+foodCal(X,_),!.
response(Q,PQ,_,['I',told,you,that,before]) :-
	Q = [how,many,calories,does,_,contain],
	member(Q,PQ),!.
response(Q,_,_,[C,'Calories']) :-
	Q = [how,many,calories,does,X,contain],
	foodCal(X,C).

response(Q,_,_,['I',do,not,know]) :-
	Q = [what,does,TY,contain],
	\+prop(TY,contain,_).
response(Q,PQ,PR,[R]) :-
	Q = [what,does,TY,contain],
	prop(TY,contain,_),
	setof(X,prop(TY,contain,X),CR),
	getDiffAnswer(Q,PQ,PR,CR,R).
response(Q,PQ,PR,['I',told,you,that,before]):-
	Q = [what,does,TY,contain],
	setof(X,prop(TY,contain,X),CR),
	\+getDiffAnswer(Q,PQ,PR,CR,_).

response(Q,PQ,_,['I',do,not,know]) :-
	Q = [can,i,have,TY,for,_],
	\+prop(TY,_,_);
	(unknownFoods(PQ,MSG),
	MSG=[doesnt]).

response(Q,PQ,PR,['You',can,have,TY,for,M]) :-
	Q = [can,i,have,TY,for,M],
	\+prop(TY,not,M),
	calcCalories(TY,PQ,PR,C),
	C >= 0.
response(Q,_,_,[TY,is,not,suitable,for,M]) :-
	Q = [can,i,have,TY,for,M],
	prop(TY,not,M).
response(Q,PQ,PR,['No']) :-
	Q = [can,i,have,TY,for,M],
	\+prop(TY,not,M),
	calcCalories(TY,PQ,PR,C),
	C < 0.

response(Q,_,_,['I',do,not,know]):-
	Q=[what,is,X],
	\+ prop(X,is,_),!.
response(Q,PQ,_,['I',told,you,that,before]):-
	Q=[what,is,_],
	member(Q,PQ),!.
response(Q,_,_,[CT]) :-
	Q = [what,is,IN],
	prop(IN,is,CT).

response(Q,PQ,PR,[C,'Calories']) :-
	Q = [how,many,calories,do,i,have,left],
	calcCalories(pizza,PQ,PR,C1),
	foodCal(pizza,X),
	C is C1+X.
response(Q,PQ,PR,['I',do,not,know]) :-
	Q = [how,many,calories,do,i,have,left],
	\+calcCalories(pizza,PQ,PR,_).

response(Q,PQ,PR,['I',told,you,that,before]) :-
	Q = [what,kind,of,FC,does,F,contain],
	setof(IN,(prop(F,contain,IN),prop(IN,is,FC)),CR),
	\+getDiffAnswer(Q,PQ,PR,CR,_).
response(Q,_,_,['I',do,not,know]) :-
	Q = [what,kind,of,FC,does,F,contain],
	(\+prop(_,_,FC) ; \+prop(F,_,_)).
response(Q,PQ,PR,[R]) :-
	Q = [what,kind,of,FC,does,F,contain],
	setof(IN,(prop(F,contain,IN),prop(IN,is,FC)),C),
	reverse(C,CR),
	getDiffAnswer(Q,PQ,PR,CR,R).
response(Q,_,_,['Nothing',from,what,i,know]) :-
	Q = [what,kind,of,FC,does,F,contain],
	\+(\+prop(F,_,_)),
	\+((prop(F,contain,IN),prop(IN,is,FC))).

response(Q,_,_,['I',do,not,know]) :-
	Q = [is,IN,a,_,in,TY],
	(\+prop(TY,_,_) ; \+(prop(_,_,IN);prop(IN,contain,_,cal))),!.
response(Q,PQ,_,['I',told,you,before]) :-
	Q = [is,_,a,_,in,_],
	member(Q,PQ),!.
response(Q,_,_,['Yes']) :-
	Q = [is,IN,a,CT,in,TY],
	prop(TY,contain,IN),
	prop(IN,is,CT).
response(Q,_,_,['No']) :-
	Q = [is,IN,a,CT,in,TY],
	\+(\+prop(TY,_,_)),
	prop(IN,contain,_,cal),
	\+((prop(TY,contain,IN) , prop(IN,is,CT))).

response(Q,_,_,['Ok']) :-
	Q = [I,ate,_,for,M] , (M = breakfast ; M = lunch ; M = dinner);
	Q = [I,do,not,eat,_].

response(Q,PQ,PR,[R]) :-
	Q = [what,can,i,have,for,_,that,contains,_],
	responseO(Q,PQ,PR,LR),
	removeNum(LR,CR),
	getDiffAnswer(Q,PQ,PR,CR,R).
response(Q,_,_,['I',do,not,know]) :-
	Q = [what,can,i,have,for,_,that,contains,X],
	\+(prop(X,is,_)).
response(Q,PQ,PR,['Nothing',from,what,i,know]) :-
	Q = [what,can,i,have,for,_,that,contains,_],
	responseO(Q,PQ,PR,LR),
	removeNum(LR,[]),!.
response(Q,PQ,PR,['I',told,you,that,before]) :-
	Q = [what,can,i,have,for,_,that,contains,_],
	responseO(Q,PQ,PR,LR),
	removeNum(LR,CR),
	\+(getDiffAnswer(Q,PQ,PR,CR,_)).

responseO(Q,PQ,PR,LR):-
	Q=[what,can,i,have,for,M,that,contains,IN],
	setof(S-0,prop(S,contain,IN),FL),
	giveScore(FL,M,PQ,PR,IN,SL),
	listOrderDesc(SL,LR).

giveScore(FL,M,PQ,PR,IN,SL):-
	getUnlikedIngredients(PQ,UL),
	unlikedHelper(FL,UL,S1),
	mealHelper(S1,M,S2),
	calHelper(S2,PQ,PR,S3),
	inHelper(S3,IN,SL).

inHelper([],_,[]).
inHelper([F-N|T],IN,[F-N1|T1]):-
	prop(F,contain,IN),
	N1 is N+1,
	inHelper(T,IN,T1).
inHelper([F-N|T],IN,[F-N|T1]):-
	\+prop(F,contain,IN),
	inHelper(T,IN,T1).

unlikedHelper([],_,[]).
unlikedHelper([F-0|T],UL,[F-1|T1]):-
	setof(IN,prop(F,contain,IN),CN),
	\+((member(X,CN),member(X,UL))),
	unlikedHelper(T,UL,T1).

unlikedHelper([F-0|T],UL,[F-0|T1]):-
	setof(IN,prop(F,contain,IN),CN),
	\+ (\+((member(X,CN),member(X,UL)))),
	unlikedHelper(T,UL,T1).

mealHelper([],_,[]).
mealHelper([F-N|T],M,[F-N1|T1]):-
\+prop(F,not,M),
	N1 is N+1,
	mealHelper(T,M,T1).

mealHelper([F-N|T],M,[F-N|T1]):-
	prop(F,not,M),
	mealHelper(T,M,T1).

calHelper([],_,_,[]).
calHelper([F-N|T],PQ,PR,[F-N1|T1]):-
	calcCalories(F,PQ,PR,C),
	(C >= 0 , N1 is N+1;
	 C <  0 , N1 is N),
	calHelper(T,PQ,PR,T1).

removeNum([],[]).
removeNum([F-4|T],[F|T1]) :-
	removeNum(T,T1).
removeNum([_-N|T],T1) :-
	N < 4,
	removeNum(T,T1).

listOrderDesc([],[]).
listOrderDesc(LP,OLP):-
	insort(LP,X),
	reverse(X,OLP).

insert(X,[],[X]).
insert(X-N,[H-N1|T],[X-N,H-N1|T]):-
	N=<N1,!.
insert(X-N,[H-N1|T1],[H-N1|T2]):-
	insert(X-N,T1,T2).

insort([],[]).
insort([H|T],Sorted):-
	insort(T,Sorted2),
	insert(H,Sorted2,Sorted).

	foodFromHistory([],[]).
	foodFromHistory([H|T],[X|FL]):-
					                      (H=[i,ate,X,for,_];H=[you,can,have,X,for,_]),
				                        foodFromHistory(T,FL).
	foodFromHistory([H|T],FL):-
												\+  (H=[i,ate,_,for,_];H=[you,can,have,_,for,_]),
														foodFromHistory(T,FL).

getUnlikedIngredients([],[]).
getUnlikedIngredients([H|T],[X|FL]):-
	H=[i,do,not,eat,X],
	getUnlikedIngredients(T,FL).
getUnlikedIngredients([H|T],FL):-
	H \= [i,do,not,eat,_],
	getUnlikedIngredients(T,FL).
unknownFoods([],[exists]).
unknownFoods([Q|_],[doesnt]):-
	Q = [i,ate,TY,for,_],
	\+prop(TY,_,_).
unknownFoods([Q|T],MSG):-
	Q = [i,ate,TY,for,_],
	prop(TY,_,_),
	unknownFoods(T,MSG).
unknownFoods([Q|T],MSG):-
	\+ Q = [i,ate,_,for,_],
	unknownFoods(T,MSG).

/*qb -->
	hmc,ds,(ty;in),c;
	w,ds,ty,c;
	cn,i,h,ty,f,me;
	w,[is],in;
	hmc,d,i,h,[left];
	w,[kind,of],ct,ds,ty,c;
	[is],in,[a],ct,[in],ty;
	w,cn,i,h,f,me,[that],c,in.

q --> qb,[?].

w -->   [what].
hmc --> [how,many,calories].
i --> [i].
d --> [do].
ds -->[does].
h --> [have].
f --> [for].
cn -->[can].
c --> [contain].

in --> [I] , {prop(_,contains,I)}.
ty --> [T] , {prop(T,contains,_)}.
me --> [breakfast] ; [lunch] ; [dinner].
ct --> [C] , {prop(_,is,C)}.*/
