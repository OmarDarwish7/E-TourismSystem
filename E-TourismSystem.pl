offer(dahab, [diving, snorkeling, horseRiding], 10000, 2020-02-12, 2020-03-12,period(2020-03-15, 2020-04-15), 10, 5).
offer(taba, [diving], 1000, 2020-02-12, 2020-03-12, period(2020-06-01, 2020-08-31),10,1).
offerMean(offer(dahab, [diving, snorkeling, horseRiding], 10000, 2020-02-12, 2020-03-12,
period(2020-03-15, 2020-04-15), 10, 5), bus).
offerMean(offer(taba, [diving], 1000, 2020-02-12, 2020-03-12, period(2020-06-01, 2020-08-31), 10, 1),
bus).
offerAccommodation(offer(dahab, [diving, snorkeling, horseRiding], 10000, 2020-02-12, 2020-03-12,
period(2020-03-15, 2020-04-15), 10, 5), hotel).
offerAccommodation(offer(taba, [diving], 1000, 2020-02-12, 2020-03-12, period(2020-06-01, 2020-08-31),
10, 1), cabin).
customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), diving, 100).
customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), snorkeling, 100).
customerPreferredActivity(customer(ahmed, aly, 1993-01-30, single, 0, student), horseRiding, 20).
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), snorkeling, 60).
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), diving, 20).
customerPreferredActivity(customer(mohamed, elkasad, 1999-01-30, single, 0, student), horseRiding,
50).
customerPreferredMean(customer(ahmed, aly, 1993-01-30, single, 0, student), bus, 100).
customerPreferredMean(customer(mohamed, elkasad, 1999-01-30, single, 0, student), bus, 10).
customerPreferredAccommodation(customer(ahmed, aly, 1993-01-30, single, 0, student), hotel, 20).
customerPreferredAccommodation(customer(ahmed, aly, 1993-01-30, single, 0, student), cabin, 50).
customerPreferredAccommodation(customer(mohamed, elkasad, 1999-01-30, single, 0, student), hotel,
100).
customerPreferredAccommodation(customer(mohamed, elkasad, 1999-01-30, single, 0, student), cabin,
79).



splitPeriod(P,D,Y,Z):-
	format(string(A),"~w",[P]),
	split_string(A,"-","",Y1),
	format(string(B),"~w",[D]),
	split_string(B,"-","",Z1),
	parseList(Y1,Y),
	parseList(Z1,Z).



parseList([],[]).
parseList([H|T],L):-
	parseList(T,L1),
	number_string(X,H),
	append([X],L1,L).

notoverlapping(period(A1,B1),period(C1,D1)):-
	splitPeriod(A1,B1,A,B),
	splitPeriod(C1,D1,C,D),
	((before(A,C),
	before(B,C));
	(before(C,A),
	before(D,A))).

before([A1,A2,A3],[B1,B2,B3]):-
	(A1 < B1);
    (A1=B1 , A2 < B2);
	(A1 = B1,
	A2 = B2,
	A3 < B3).

overlapPeriod(P1,P2):-
	\+ notoverlapping(P1,P2).


getOffer(ChosenPrefs,O):-
	checkDest(ChosenPrefs,O),
	checkPeriod(ChosenPrefs,O),
	checkAccomm(ChosenPrefs,O),
	checkBudget(ChosenPrefs,O),
	checkActivity(ChosenPrefs,O),
	checkMean(ChosenPrefs,O).


checkDest(ChosenPrefs,O):-
	member(dest(A),ChosenPrefs),
	O =offer(A,_,_,_,_,_,_,_).

checkDest(ChosenPrefs,_):-
	\+ member(dest(_),ChosenPrefs).

checkAccomm(ChosenPrefs,O):-
	member(accommodation(B),ChosenPrefs),
	offerAccommodation(O,B).

checkAccomm(ChosenPrefs,O):-
	\+ member(accommodation(_),ChosenPrefs),
	offerAccommodation(O,_).

checkBudget(ChosenPrefs,_):-
	\+ member(budget(_),ChosenPrefs).

checkBudget(ChosenPrefs,O):-
	member(budget(B),ChosenPrefs),
	O =offer(_,_,A,_,_,_,_,_),
	lessThan(A,B).
lessThan(A,B):-
	A=<B.

checkActivity(ChosenPrefs,O):-
	O =offer(_,A,_,_,_,_,_,_),
	member(activity(B),ChosenPrefs),
	possibleSubset(A,B),
	B=[H|T].

checkActivity(ChosenPrefs,_):-
	\+ member(activity(_),ChosenPrefs).

checkPeriod(ChosenPrefs,O):-
	member(period(B),ChosenPrefs),
	overlapPeriod(period(A),period(B)),
	O =offer(_,_,_,_,_,period(A),_,_).

checkPeriod(ChosenPrefs,_):-
	\+ member(period(_),ChosenPrefs).

checkMean(ChosenPrefs,O):-
	member(means(B),ChosenPrefs),
	offerMean(O,B).
checkMean(ChosenPrefs,O):-
	\+ member(means(_),ChosenPrefs),
	offerMean(O,_).



possibleSubset(L,R) :-
    possibleSubsetHelper(L,_,L1),
    perm(L1,R).

possibleSubsetHelper([ ],[ ],[ ]).
possibleSubsetHelper([H|T],[H|L],R) :-
    possibleSubsetHelper(T,L,R).
possibleSubsetHelper([H|T],L,[H|R]) :-
    possibleSubsetHelper(T,L,R).

perm([],[]).
perm([H|T],L) :-
    perm(T,P),
    insert(H,P,L).

insert(X,L,[X|L]).
insert(X,[H|T],[H|T1]) :-
    insert(X,T,T1).

choosePreferences(Prefs, ChosenPreferences):-
    delete(Prefs,activity([H|T]),Prefs2),
    possibleSubset(Prefs2,ChosenPreferences).


choosePreferences(Prefs, ChosenPreferences):-
    member(activity([H|T]),Prefs),
    subset1([H|T],R),
    R = [H1|T1],
    delete(Prefs,activity([H|T]),Prefs2),
    append([activity(R)],Prefs2,PrefsFinal),
    possibleSubset(PrefsFinal,ChosenPreferences),
    member(activity(X),ChosenPreferences).



subset1([],[]).
subset1([X|S],[X|L]) :-
    subset1(S,L).
subset1([_|S],L) :-
    subset1(S,L).


preferenceSatisfaction(Offer, Customer, ChosenPrefs, S):-
    preferenceAccom(Offer,Customer,ChosenPrefs,S1),
    preferenceMean(Offer,Customer,ChosenPrefs,S2),
    preferenceActivity(Offer,Customer,ChosenPrefs,S3),
    S0 is S1 + S2,
    S is S3 + S0.


preferenceAccom(Offer,Customer, ChosenPrefs, S):-
    member(accommodation(X),ChosenPrefs),
    offerAccommodation(Offer,X),
    customerPreferredAccommodation(Customer,X,S).

preferenceAccom(Offer,Customer, ChosenPrefs, S):-
    member(accommodation(X),ChosenPrefs),
    \+ offerAccommodation(Offer,X),
    S = 0.

preferenceAccom(_,_,ChosenPrefs, S):-
    \+ member(accommodation(X),ChosenPrefs),
    S =0.

preferenceMean(Offer,Customer,ChosenPrefs, S):-
    member(means(X),ChosenPrefs),
    offerMean(Offer,X),
    customerPreferredMean(Customer,X,S).

preferenceMean(Offer,Customer,ChosenPrefs, S):-
    member(means(X),ChosenPrefs),
    \+ offerMean(Offer,X),
    S = 0.

preferenceMean(_,_,ChosenPrefs, S):-
    \+ member(mean(X),ChosenPrefs),
    S =0.


preferenceActivity(offer(_,LA,_,_,_,_,_,_),Customer,ChosenPrefs, S):-
    member(activity(X),ChosenPrefs),
    member(Activity,X),
    member(Activity,LA),
    customerPreferredActivity(Customer,Activity,S1),
    removeAc(Activity,X,ChosenPrefs,Prefs),
    preferenceActivity(offer(_,LA,_,_,_,_,_,_),Customer,Prefs, S2),
    S is S1 + S2.


removeAc(A,LA,L1,L):-
    delete(LA,A,LA2),
    LA2 = [H|T],
    delete(L1,activity(LA),L2),
    append([activity(LA2)],L2,L).

removeAc(A,LA,L1,L):-
    delete(LA,A,LA2),
    LA2 = [],
    delete(L1,activity(LA),L).


preferenceActivity(offer(_,LA,_,_,_,_,_,_),Customer,ChosenPrefs, S):-
    member(activity(X),ChosenPrefs),
    member(Activity,X),
    \+ member(Activity,LA),
    S = 0.

preferenceActivity(_,_,ChosenPrefs, S):-
    \+ member(activity(X),ChosenPrefs),
    S =0.





parseList([],[]).
parseList([H|T],L):-
    parseList(T,L1),
    number_string(X,H),
    append([X],L1,L).

recommendOfferForCustomer(Prefs,ChosenPrefs,O):-
	choosePreferences(Prefs,ChosenPrefs),
	getOffer(ChosenPrefs,O).

recommendOffer([H|T],[H1|T1],Offer,CustomersChosen):-
	Customers = [H|T],
	recommendOfferForCustomer(H1,ChosenPrefs,O),
	preferenceSatisfaction(O,H,ChosenPrefs,S1),
	Offer = O,
	append([S1],[],S),
	recommendOffer2(Customers,T,T1,O,CustomersChosen,S).


recommendOffer2(Customers,[],[],offer(A,B,C,D,E,F,G,H),CustomersChosen,S):-
	chooseCustomers(Customers,S,H,CustomersChosen).


recommendOffer2(Customers,[H|T],[H1|T1],O,CustomersChosen,S):-
	recommendOfferForCustomer(H1,ChosenPrefs,O),
	preferenceSatisfaction(O,H,ChosenPrefs,S1),
	add([S1],S,S2),
	recommendOffer2(Customers,T,T1,O,CustomersChosen,S2).

chooseCustomers(Customers,S,A,CustomersChosen):-
	chooseCustomers(Customers,S,A,Acc,CustomersChosen).

chooseCustomers([],[],_,Acc,Acc).
chooseCustomers(_,_,A,Acc,Acc):-
	length(Acc,A),
	Acc = [C|_],
	\+ var(C).
chooseCustomers(Customers,S,A,[],CustomersChosen):-
	maxElem(S,Max),
	nth0(I,S,Max),
	nth0(I,Customers,C),
	append([C],[],Acc),
	delete(S,Max,NewS),
	delete(Customers,C,NewCust),
	chooseCustomers(NewCust,NewS,A,Acc,CustomersChosen).
chooseCustomers(Customers,S,A,Acc,CustomersChosen):-
	\+ S =[],
	\+ Acc = [],
	\+ length(Acc,A),
	maxElem(S,Max),
	nth0(I,S,Max),
	nth0(I,Customers,C),
	append([C],Acc,NewAcc),
	delete(S,Max,NewS),
	delete(Customers,C,NewCust),
	chooseCustomers(NewCust,NewS,A,NewAcc,CustomersChosen).



add(X,[],[X]).
add(X,[Y|Tail],[Y|Tail1]):-
    add(X,Tail,Tail1).


maxElem([A],A).
maxElem([H1,H2|T],Max):-
	maxElem([H2|T],Max1),
	max(H1,Max1,Max).
max(A,B,A):-
	A >= B.
max(A,B,B):-
	B > A.

























