% declare dynamic predicate path which stores temporary results of
% computation.
:-dynamic path/3.

% There are three parameter in path(Source, Goal, Distance) which
% denotes the dynamic fact
% Welcome to Road Route System :-

hola:-
    % before creating the new facts remove all the previous facts.
    retractall(path(_,_,_)),

    %start our prolog program
    start,
    write('Welcome to Road Route System'),nl,
    write('........This system finds the route between Source and Destination........'),nl,

    write('Kindly enter your choice: By which technique you wish to find the route and distance between the cities.'),nl,
    write('1. Depth First Search'),nl,
    write('2. Best First Search'),nl,
    %read choice of user and then perform the the required technique to find the route and distance between source and destination.
    read(Choice),
    write('Kindly enter that from which city to which city you wish to find route and distance'),nl,
    write('Enter the source city'),nl,
    write('Source: '),
    read(Source),
    write('Enter the goal city'),nl,
    write('Destination/Goal: '),
    read(Goal),
    answer(Source,Goal,Path,Distance,Choice),
    write('Route/Path : '),
    write(Path),nl,
    write('Total Distance : '),
    write(Distance).



% read the csv file and convert each row and column to some facts.
start:-
    csv_read_file('roaddistances.csv',[Head1|Rest]),
     % converting the rows to list
    rowstolist(Head1,Head2),
    %converting each rows to list and then some facts.
    true(Head2,Rest).


%total cost of list which is stored in Cost.
%initially let the second parameter ie, Cost be 0.
cost([],0).

%recursively add value to Cost.
cost([H|Tail],Cost):-
    cost(Tail,Temp_Cost), Cost is H + Temp_Cost.


%rule to convert the Rows to List
rowstolist(H,Tail):-
    H =..[_|Tail].

%Converting each rows to list and declaring the facts as true.
% when Row List becomes empty
true(_H,[]):-true.


true(H,[Q1|Q2]):-
    rowstolist(Q1,Q3),
    lists:nth1(1,Q3,Temp1) , %takes three parameter
    % declaring the facts
    Fact1=(lists:nth1(N,Q3,LEN)) ,

    %declaring the fact that N should always greater than 1.
    Fact2=(N > 1) ,

    Fact3=(lists:nth1(N,H,Temp2)) ,

    PATH=(Fact1,Fact2,Fact3),


     %assigning Distance to variable NDistance
    NDistance = LEN,
      % assigning Temp1 and Temp2 to new variable NTemp1 and NTemp2 respectively.
    NTemp1 = Temp1,
    NTemp2 = Temp2,



    %adding facts to database.

    TRUE =((assertz(path(Temp1,Temp2,LEN))),
           assertz(path(NTemp2,NTemp1,NDistance))) ,

    forall(PATH,TRUE),

    retractall(path(A,A,-)),

    true(H,Q2).


% 'answer' calls the required technique as entered by user and
% find the corresponding path and distance.
answer(Source,Goal,Path,Distance,Choice):-
    (

     %if the choice entered by user is 1 , then call dfs.
    Choice =:= 1 ->
      (dfs(Source,Goal,[],A,[],B),cost(B,Distance),reverse(A,Path));

    %if the choice entered by user is 2 , then call bestfirstsearch.
    Choice =:= 2 ->
      (bestfirstsearch(Source,Goal,Distance,Path))

    ).
% dfs return true when Source City reaches to the Goal City
dfs(Goal,Goal,Trace,[Goal|Trace],Dist,Dist).



%dfs calls itself recursively until source city become goal city.

dfs(Source,Goal,Trace,Path,Dist,LEN):-
    path(Source,A,LEN1),
    \+ member(A,Trace),
    dfs(A,Goal,[Source|Trace],Path,[LEN1|Dist],LEN).



% Making the hueristic


%Create the two lists, ie of Trace and Distance
%Reverse List1 and List2 and store in T and D respectively
adjacentNode(_,_,[],LI1,LI2,List1,List2):-
    reverse(LI1,List1),
    reverse(LI2,List2).

%Create two listes which stores paths and distance
adjacentNode(Source, Goal,[Head1|Tail],LI1,LI2,List1,List2):-
    path(Source,Head1,LEN1),
    path(Head1,Goal,LEN2),
    %random assign any random integer between 20 to 30
    random(30,40,A),
    Cost is LEN1+LEN2-A,
    adjacentNode(Source,Goal,Tail,[Cost|LI1],[(Source,Head1,Goal)|LI2],List1,List2).


compute_logs([],[],H,Heuristic):-
    reverse(H,Heuristic).

compute_logs([Head1|Tail1],[Head2|Tail2],H,[(Head1:Head2)|Heuristic]):-
    compute_logs(Tail1,Tail2,H,Heuristic).


%compute_hueristic ,it creates heuristic from source to destinantion.
compute_heuristic(Source,Goal,Heuristic):-
    %read CSV FILE 'roaddistances.csv'
    csv_read_file('roaddistances.csv',[Head|_]),
    rowstolist(Head,H),
    H = [_|Tail],
    adjacentNode(Source,Goal,Tail,[],[],List1,List2),
    compute_logs(List1,List2,[],Heuristic).

%Compute the minimum distance path

%if there is only one element left.

min_distance([(X:Y)],(X:Y),X,Y).

%if there list head has minimum value.
min_distance([(X:Y)|Tail],(X:Y),X,Y):-
    min_distance(Tail,(A:B),A,B), X < A.

%if list head is greater value then next.
min_distance([(X:_)|Tail],(A:B),A,B):-
    min_distance(Tail,(A:B),A,B),X >= A.

%implementing best first search


% bestfirstsearch when there is no direct path exists between source and
% destination.
bestfirstsearch(Source,Goal,LEN,Path):-
    %find_heuristic calculates the hueristic dynamically.
    compute_heuristic(Source,Goal,Heuristic),
    %min_distance selects the best minimum distance among different paths
    min_distance(Heuristic,(_:_),LEN,Path).

%When there is a direct route between Source and Goal City
bestfirstsearch(Source,Goal,LEN1,[Source,Goal]):-
    path(Source,Goal,LEN),LEN1 is LEN-30,!.



