-module(p99).
-mode(compile).
-compile(export_all).
-include_lib("xmerl/include/xmerl.hrl").


%% <html>
%% <head>
%% <title>Ninety-Nine Erlang Problems</title>
%% </head>
%% <body>

%%% <h1>Ninety-Nine Erlang Problems</h1>



%%% <h2>Working with lists</h2>


%%% <dl>
%%% <dt>P01: Find the last element of a list</dt>
%%%
%%% <dd>
%%% Example:
%%%
%%% ```
%%% 1> p99:my_last([a,b,c,d]).
%%% d
%%% 2>
%%% '''
%%% </dd>

my_last([X]) ->
    X;
my_last([_|L]) ->
    my_last(L).


%%% <dt>P02: Find the last but one element of a list</dt>
%%%
%%% <dd>
%%% Example:
%%%
%%% ```
%%% 1> p99:last_but_one([a,b,c,d]).
%%% c
%%% 2>
%%% '''
%%% </dd>

last_but_one([X, _]) ->
    X;
last_but_one([_, Y|L]) ->
    last_but_one([Y|L]).


%%% <dt>P03: Find the K'th element of a list</dt>
%%%
%%% <dd>
%%% The first element in the list is number 1.
%%%
%%% Example:
%%%
%%% ```
%%% 1> p99:element_at([a,b,c,d,e],3).
%%% c
%%% 2>
%%% '''
%%% </dd>

element_at([X|_], 1) ->
    X;
element_at([_|L], K)
  when K > 1 ->
    element_at(L, K-1).


%%% <dt>P04: Find the number of elements of a list</dt>
%%%
%%% <dd>
%%% Example:
%%%
%%% ```
%%% 1> p99:my_length([a,b,c,d,e]).
%%% 5
%%% 2>
%%% '''
%%% </dd>

my_length([]) ->
    0;
my_length([_|T]) ->
    1 + my_length(T).


%%% <dt>P05: Reverse a list</dt>
%%%
%%% <dd>
%%% Example:
%%%
%%% ```
%%% 1> p99:my_reverse([a,b,c,d,e]).
%%% [e,d,c,b,a]
%%% 2>
%%% '''
%%% </dd>

my_reverse([], L2) ->
    L2;
my_reverse([H|L1], L2) ->
    my_reverse(L1, [H|L2]).

my_reverse(List) ->
    my_reverse(List, []).


%%% <dt>P06: Find out whether a list is a palindrome</dt>
%%%
%%% <dd>
%%% A palindrome can be read forward or backward; e.g. [x,a,m,a,x]
%%%
%%% Example:
%%%
%%% ```
%%% 1> p99:is_palindrome([x,a,m,a,x]).
%%% true
%%% 2>
%%% '''
%%% </dd>

is_palindrome(List) ->
    List =:= my_reverse(List).


%%% <dt>P07: Flatten a nested list structure</dt>
%%%
%%% <dd>
%%% Transform a list, possibly holding lists as elements into a `flat'
%%% list by replacing each list with its elements (recursively).
%%%
%%% Example:
%%%
%%% ```
%%% 1> p99:my_flatten([a, [b, [c, d], e]]).
%%% [a,b,c,d,e]
%%% 2>
%%% '''
%%% </dd>

my_flatten([H|T])
  when is_list(H) ->
    my_flatten(H) ++ my_flatten(T);
my_flatten([H|T]) ->
    [my_flatten(H)|my_flatten(T)];
my_flatten(Term) ->
    Term.


%%% <dt>P08: Eliminate consecutive duplicates of list elements</dt>
%%%
%%% <dd>
%%% If a list contains repeated elements they should be replaced with
%%% a single copy of the element. The order of the elements should not
%%% be changed.
%%%
%%% Example:
%%%
%%% ```
%%% 1> p99:compress([a,a,a,a,b,c,c,a,a,d,e,e,e,e]).
%%% [a,b,c,a,d,e]
%%% 2>
%%% '''
%%% </dd>

compress(_, []) ->
    [];
compress(H, [H|T]) ->
    compress(H, T);
compress(_, [H|T]) ->
    [H|compress(H, T)].

compress(List) ->
    compress([], List).


%%% <dt>P09: Pack consecutive duplicates of list elements into sublists</dt>
%%%
%%% <dd>
%%% If a list contains repeated elements they should be placed in
%%% separate sublists.
%%%
%%% Example:
%%%
%%% ```
%%% 1> p99:pack([a,a,a,a,b,c,c,a,a,d,e,e,e,e]).
%%% [[a,a,a,a],[b],[c,c],[a,a],[d],[e,e,e,e]]
%%% 2>
%%% '''
%%% </dd>

pack([], []) ->
    [];
pack([], [H|T]) ->
    pack([H], T);
pack([H|_]=L, [H|T]) ->
    pack([H|L], T);
pack(L, List) ->
    [L|pack([], List)].

pack(List) ->
    pack([], List).


%%% <dt>P10:  Run-length encoding of a list</dt>
%%%
%%% <dd>
%%% Use the result of problem P09 to implement the so-called
%%% run-length encoding data compression method. Consecutive
%%% duplicates of elements are encoded as terms {N,E} where N is the
%%% number of duplicates of the element E.
%%%
%%% Example:
%%%
%%% ```
%%% 1> p99:encode([a,a,a,a,b,c,c,a,a,d,e,e,e,e]).
%%% [{4,a},{1,b},{2,c},{2,a},{1,d},{4,e}]
%%% 2>
%%% '''
%%% </dd>

encode(List) ->
    [{my_length(Elems), H} || [H|_]=Elems <- pack(List)].


%%% <dt>P11:  Modified run-length encoding</dt>
%%%
%%% <dd>
%%% Modify the result of problem P10 in such a way that if an element
%%% has no duplicates it is simply copied into the result list. Only
%%% elements with duplicates are transferred as [N,E] terms.
%%%
%%% Example:
%%%
%%% ```
%%% 1> p99:encode_modified([a,a,a,a,b,c,c,a,a,d,e,e,e,e]).
%%% [{4,a},b,{2,c},{2,a},d,{4,e}]
%%% 2>
%%% '''
%%% </dd>

modify_encode([]) ->
    [];
modify_encode([{1, E}|T]) ->
    [E|modify_encode(T)];
modify_encode([H|T]) ->
    [H|modify_encode(T)].

encode_modified(List) ->
    modify_encode(encode(List)).


%%% <dt>P12: Decode a run-length compressed list</dt>
%%%
%%% <dd>
%%% Given a run-length code list generated as specified in problem
%%% P11. Construct its uncompressed version.
%%%
%%% Example:
%%%
%%% ```
%%% 1> p99:decode([{4,a},b,{2,c},{2,a},d,{4,e}]).
%%% [a,a,a,a,b,c,c,a,a,d,e,e,e,e]
%%% 2>
%%% '''
%%% </dd>

decode([]) ->
    [];
decode([{0,_}|T]) ->
    decode(T);
decode([{N,E}|T]) ->
    [E|decode([{N-1,E}|T])];
decode([E|T]) ->
    [E|decode(T)].


%%% <dt>P13: Run-length encoding of a list (direct solution)</dt>
%%%
%%% <dd>
%%% Implement the so-called run-length encoding data compression
%%% method directly. I.e. don't explicitly create the sublists
%%% containing the duplicates, as in problem P09, but only count
%%% them. As in problem P11, simplify the result list by replacing the
%%% singleton terms [1,X] by X.
%%%
%%% Example:
%%%
%%% ```
%%% 1> p99:encode_direct([a,a,a,a,b,c,c,a,a,d,e,e,e,e]).
%%% [{4,a},b,{2,c},{2,a},d,{4,e}]
%%% 2>
%%% '''
%%% </dd>

encode_direct([], []) ->
    [];
encode_direct([], [H|T]) ->
    encode_direct({1,H}, T);
encode_direct({N,H}, [H|T]) ->
    encode_direct({N+1, H}, T);
encode_direct({1,E}, List) ->
    [E|encode_direct([], List)];
encode_direct({N,E}, List) ->
    [{N,E}|encode_direct([], List)].

encode_direct(List) ->
    encode_direct([], List).


%%% <dt>P14: Duplicate the elements of a list</dt>
%%%
%%% <dd>
%%% Example:
%%%
%%% ```
%%% 1> p99:dupli([a,b,c,c,d]).
%%% [a,a,b,b,c,c,c,c,d,d]
%%% 2>
%%% '''
%%% </dd>

dupli(List) ->
    dupli(List, 2).


%%% <dt>P15: Duplicate the elements of a list agiven number of times</dt>
%%%
%%% <dd>
%%% Example:
%%%
%%% ```
%%% 1> p99:dupli([a,b,c],3).
%%% [a,a,a,b,b,b,c,c,c]
%%% 2>
%%% '''
%%% </dd>

dupli([], _, _) ->
    [];
dupli([_|T], N, 0) ->
    dupli(T, N, N);
dupli([H|_]=L, N, K) ->
    [H|dupli(L, N, K-1)].

dupli(List, N) ->
    dupli(List, N, N).


%%% <dt>P16: Drop every N'th element from a list</dt>
%%%
%%% <dd>
%%% Example:
%%%
%%% ```
%%% 1> p99:drop([a,b,c,d,e,f,g,h,i,k],3).
%%% [a,b,d,e,g,h,k]
%%% 2>
%%% '''
%%% </dd>

drop([], _, _) ->
    [];
drop([_|T], N, 1) ->
    drop(T, N, N);
drop([H|T], N, K) ->
    [H|drop(T, N, K-1)].

drop(List, N) ->
    drop(List, N, N).


%%% <dt>P17: Split a list into two parts</dt>
%%%
%%% <dd>
%%% Example:
%%%
%%% ```
%%% 1> p99:split([a,b,c,d,e,f,g,h,i,k],3).
%%% {[a,b,c],[d,e,f,g,h,i,k]}
%%% 2>
%%% '''
%%% </dd>

split(List, 0) ->
    {[], List};
split([H|T], N) ->
    {Front, Rear} = split(T, N-1),
    {[H|Front], Rear}.


%%% <dt>P18:  Extract a slice from a list</dt>
%%%
%%% <dd>
%%% Given two indices, I and K, the slice is the list containing the
%%% elements between the I'th and K'th element of the original list
%%% (both limits included). Start counting the elements with 1.
%%%
%%% Example:
%%%
%%% ```
%%% 1> p99:slice([a,b,c,d,e,f,g,h,i,k],3,7).
%%% [c,d,e,f,g]
%%% 2>
%%% '''
%%% </dd>

slice([H|_], 1, 1) ->
    [H];
slice([H|T], 1, To) ->
    [H|slice(T, 1, To-1)];
slice([_|T], From, To) ->
    slice(T, From-1, To-1).


%%% <dt>P19: Rotate a list N places to the left</dt>
%%%
%%% <dd>
%%% Example:
%%%
%%% ```
%%% 1> p99:rotate([a,b,c,d,e,f,g,h],3).
%%% [d,e,f,g,h,a,b,c]
%%% 2> p99:rotate([a,b,c,d,e,f,g,h],-2).
%%% [g,h,a,b,c,d,e,f]
%%% 3>
%%% '''
%%% </dd>

rotate(List, N) ->
    Length = my_length(List),
    Times =
        case N rem Length of
            R when R >= 0 ->
                R;
            R when R < 0 ->
                Length + R
        end,
    {Left, Right} = split(List, Times),
    Right ++ Left.


%%% <dt>P20: Remove the K'th element from a list</dt>
%%%
%%% <dd>
%%% Example:
%%%
%%% ```
%%% 1> p99:remove_at([a,b,c,d],2).
%%% {b,[a,c,d]}
%%% 2>
%%% '''
%%% </dd>

remove_at([H|T], 1) ->
    {H, T};
remove_at([H|T], N) ->
    {Elem, List} = remove_at(T, N-1),
    {Elem, [H|List]}.


%%% <dt>P21: Insert an element at a given position into a list</dt>
%%%
%%% <dd>
%%% Example:
%%%
%%% ```
%%% 1> p99:insert_at(alfa,[a,b,c,d],2).
%%% [a,alfa,b,c,d]
%%% 2>
%%% '''
%%% </dd>

insert_at(Elem, List, 1) ->
    [Elem|List];
insert_at(Elem, [H|T], N) ->
    [H|insert_at(Elem, T, N-1)].


%%% <dt>P22: Create a list containing all integers within a given range</dt>
%%%
%%% <dd>
%%% Example:
%%%
%%% ```
%%% 1> p99:range(4,9).
%%% [4,5,6,7,8,9]
%%% 2>
%%% '''
%%% </dd>

range(To, To) ->
    [To];
range(From, To) ->
    [From|range(From+1, To)].


%%% <dt>P23: Extract a given number of randomly selected elements from a list</dt>
%%%
%%% <dd>
%%% The selected items shall be put into a result list.
%%%
%%% Example:
%%%
%%% ```
%%% 1> p99:reset_seed().
%%% ok
%%% 2> p99:rnd_select([a,b,c,d,e,f,g,h],3).
%%% [f,d,c]
%%% 3>
%%% '''
%%%
%%% Hint: Use the built-in random number generator random/2 and the
%%% result of problem P20.
%%% </dd>

reset_seed() ->
    random:seed(random:seed0()),
    ok.

rnd_select([], Choice, _) ->
    Choice;
rnd_select([H|T], Choice, N) ->
    I = random:uniform(N),
    Choice1 =
        case I =< size(Choice) of
            true ->
                setelement(I, Choice, H);
            false ->
                Choice
        end,
    rnd_select(T, Choice1, N+1).

rnd_select(List, N) ->
    {Choice, List1} = split(List, N),
    Choice1 = list_to_tuple(Choice),
    Choice2 = rnd_select(List1, Choice1, N+1),
    tuple_to_list(Choice2).


%%% <dt>P24: Lotto: Draw N different random numbers from the set 1..M</dt>
%%%
%%% <dd>
%%% The selected numbers shall be put into a result list.
%%%
%%% Example:
%%%
%%% ```
%%% 1> p99:reset_seed().
%%% ok
%%% 2> p99:lotto(6,49).
%%% [17,2,28,27,11,43]
%%% 3>
%%% '''
%%%
%%% Hint: Combine the solutions of problems P22 and P23.
%%% </dd>

lotto(N, M) ->
    rnd_select(range(1, M), N).


%%% <dt>P25: Generate a random permutation of the elements of a list</dt>
%%%
%%% <dd>
%%% Example:
%%%
%%% ```
%%% 1> p99:reset_seed().
%%% ok
%%% 2> p99:rnd_permu([a,b,c,d,e,f]).
%%% [c,d,e,b,f,a]
%%% 3>
%%% '''
%%%
%%% Hint: Use the solution of problem P23.
%%% </dd>

%% {tree, N, Left, Right}
%% {leaf, Leaf}
%% empty

merge_tree_lists([{leaf, X}, empty]) ->
    [{leaf, X}];
merge_tree_lists([{leaf, _}=H1, {leaf, _}=H2|T]) ->
    merge_tree_lists([{tree, 2, H2, H1}|T]);
merge_tree_lists([{tree, N, _, _}=H1, {tree, N, _, _}=H2|T]) ->
    merge_tree_lists([{tree, 2*N, H2, H1}|T]);
merge_tree_lists(Stack) ->
    Stack.

list_to_tree_list([], [E]) ->
    E;
list_to_tree_list([], [{leaf, _}=H1, {tree, N, _, _}=H2|T]) ->
    list_to_tree_list([], [{tree, N+1, H2, H1}|T]);
list_to_tree_list([], [{tree, N1, _, _}=H1, {tree, N2, _, _}=H2|T]) ->
    list_to_tree_list([], [{tree, N1+N2, H2, H1}|T]);
list_to_tree_list([H|T], Stack) ->
    Stack1 = merge_tree_lists([{leaf, H}|Stack]),
    list_to_tree_list(T, Stack1).

list_to_tree_list(List) ->
    list_to_tree_list(List, [empty]).

tree_list_to_list({leaf, Leaf}, List) ->
    [Leaf|List];
tree_list_to_list({tree, _, Left, Right}, List) ->
    List1 = tree_list_to_list(Right, List),
    tree_list_to_list(Left, List1).

tree_list_to_list(empty) ->
    [];
tree_list_to_list(Tree) ->
    tree_list_to_list(Tree, []).

tree_list_append(Elem, empty) ->
    {leaf, Elem};
tree_list_append(Elem, {leaf, _}=Tree) ->
    {tree, 2, Tree, {leaf, Elem}};
tree_list_append(Elem, {tree, 2, {leaf, _}, {leaf, _}}=Tree) ->
    {tree, 3, Tree, {leaf, Elem}};
tree_list_append(Elem, {tree, N, {tree, N1, _, _}, {tree, N1, _, _}}=Tree) ->
    {tree, N+1, Tree, {leaf, Elem}};
tree_list_append(Elem, {tree, N, Left, Right}) ->
    Right1 = tree_list_append(Elem, Right),
    {tree, N+1, Left, Right1}.

tree_list_replace({leaf, _}, 1, Elem) ->
    {leaf, Elem};
tree_list_replace({tree, N, {leaf, _}, Right}, 1, Elem) ->
    {tree, N, {leaf, Elem}, Right};
tree_list_replace({tree, N, {leaf, _}=Left, Right}, Index, Elem) ->
    Right1 = tree_list_replace(Right, Index-1, Elem),
    {tree, N, Left, Right1};
tree_list_replace({tree, N, {tree, N1, _, _}=Left, Right}, Index, Elem)
  when Index =< N1 ->
    Left1 = tree_list_replace(Left, Index, Elem),
    {tree, N, Left1, Right};
tree_list_replace({tree, N, {tree, N1, _, _}=Left, Right}, Index, Elem) ->
    Right1 = tree_list_replace(Right, Index-N1, Elem),
    {tree, N, Left, Right1}.

tree_list_get({leaf, Elem}, 1) ->
    Elem;
tree_list_get({tree, _, {leaf, Elem}, _}, 1) ->
    Elem;
tree_list_get({tree, _, {leaf, _}, Right}, Index) ->
    tree_list_get(Right, Index-1);
tree_list_get({tree, _, {tree, N1, _, _}=Left, _}, Index)
  when Index =< N1 ->
    tree_list_get(Left, Index);
tree_list_get({tree, _, {tree, N1, _, _}, Right}, Index) ->
    tree_list_get(Right, Index-N1).

rnd_permu([], _, Tree) ->
    Tree;
rnd_permu([H|T], N, Tree) ->
    Index = random:uniform(N),
    case Index of
        N ->
            Tree1 = tree_list_append(H, Tree),
            rnd_permu(T, N+1, Tree1);
        _ ->
            Elem = tree_list_get(Tree, Index),
            Tree1 = tree_list_replace(Tree, Index, H),
            Tree2 = tree_list_append(Elem, Tree1),
            rnd_permu(T, N+1, Tree2)
    end.

rnd_permu(List) ->
    Tree = rnd_permu(List, 1, empty),
    tree_list_to_list(Tree).


%%% <dt>P26: Generate the combinations of K distinct objects chosen from the N elements of a list</dt>
%%%
%%% <dd>
%%% In how many ways can a committee of 3 be chosen from a group of 12
%%% people? We all know that there are C(12,3) = 220 possibilities
%%% (C(N,K) denotes the well-known binomial coefficients). For pure
%%% mathematicians, this result may be great. But we want to really
%%% generate all the possibilities (via backtracking).
%%%
%%% Example:
%%%
%%% ```
%%% 1> p99:combination(3,[a,b,c,d,e,f]).
%%% [[a,b,c],
%%%  [a,b,d],
%%%  [a,b,e],
%%%  [a,b,f],
%%%  [a,c,d],
%%%  [a,c,e],
%%%  [a,c,f],
%%%  [a,d,e],
%%%  [a,d,f],
%%%  [a,e,f],
%%%  [b,c,d],
%%%  [b,c,e],
%%%  [b,c,f],
%%%  [b,d,e],
%%%  [b,d,f],
%%%  [b,e,f],
%%%  [c,d,e],
%%%  [c,d,f],
%%%  [c,e,f],
%%%  [d,e,f]]
%%% 2>
%%% '''
%%% </dd>


combination(0, _) ->
    [[]];
combination(_, []) ->
    [];
combination(N, [H|T]) ->
    [[H|Choice] || Choice <- combination(N-1, T)] ++ combination(N, T).


%%% <dt>P27: Group the elements of a set into disjoint subsets</dt>
%%%
%%% <dd>
%%% a) In how many ways can a group of 9 people work in 3 disjoint
%%% subgroups of 2, 3 and 4 persons? Write a predicate that generates
%%% all the possibilities via backtracking.
%%%
%%% Example:
%%%
%%% ```
%%% 1> p99:group3([aldo,beat,carla,david,evi,flip,gary,hugo,ida]).
%%% [[[aldo,beat],[carla,david,evi],[flip,gary,hugo,ida]],
%%%  [[aldo,beat],[carla,david,flip],[evi,gary,hugo,ida]],
%%%  [[aldo,beat],[carla,david,gary],[evi,flip,hugo,ida]],
%%%  [[aldo,beat],[carla,david,hugo],[evi,flip,gary,ida]],
%%%  [[aldo,beat],[carla,david,ida],[evi,flip,gary,hugo]],
%%%  [[aldo,beat],[carla,evi,flip],[david,gary,hugo,ida]],
%%%  [[aldo,beat],[carla,evi,gary],[david,flip,hugo,ida]],
%%%  [[aldo,beat],[carla,evi,hugo],[david,flip,gary,ida]],
%%%  [[aldo,beat],[carla,evi,ida],[david,flip,gary,hugo]],
%%%  [[aldo,beat],[carla,flip,gary],[david,evi,hugo,ida]],
%%%  [[aldo,beat],[carla,flip,hugo],[david,evi,gary,ida]],
%%%  [[aldo,beat],[carla,flip,ida],[david,evi,gary,hugo]],
%%%  [[aldo,beat],[carla,gary,hugo],[david,evi,flip,ida]],
%%%  [[aldo,beat],[carla,gary,ida],[david,evi,flip,hugo]],
%%%  [[aldo,beat],[carla,hugo,ida],[david,evi,flip,gary]],
%%%  [[aldo,beat],[david,evi,flip],[carla,gary,hugo,ida]],
%%%  [[aldo,beat],[david,evi,gary],[carla,flip,hugo,ida]],
%%%  [[aldo,beat],[david,evi,hugo],[carla,flip,gary,ida]],
%%%  [[aldo,beat],[david,evi,ida],[carla,flip,gary,hugo]],
%%%  [[aldo,beat],[david,flip,gary],[carla,evi,hugo,ida]],
%%%  [[aldo,beat],[david,flip,hugo],[carla,evi,gary,ida]],
%%%  [[aldo,beat],[david,flip,ida],[carla,evi,gary,hugo]],
%%%  [[aldo,beat],[david,gary,hugo],[carla,evi,flip|...]],
%%%  [[aldo,beat],[david,gary,ida],[carla,evi|...]],
%%%  [[aldo,beat],[david,hugo|...],[carla|...]],
%%%  [[aldo,beat],[evi|...],[...]],
%%%  [[aldo|...],[...]|...],
%%%  [[...]|...],
%%%  [...]|...]
%%% 2>
%%% '''
%%%
%%% b) Generalize the above predicate in a way that we can specify a
%%% list of group sizes and the predicate will return a list of
%%% groups.
%%%
%%% Example:
%%%
%%% ```
%%% 1> p99:group([aldo,beat,carla,david,evi,flip,gary,hugo,ida],[2,2,5]).
%%% [[[aldo,beat],[carla,david],[evi,flip,gary,hugo,ida]],
%%%  [[aldo,beat],[carla,evi],[david,flip,gary,hugo,ida]],
%%%  [[aldo,beat],[carla,flip],[david,evi,gary,hugo,ida]],
%%%  [[aldo,beat],[carla,gary],[david,evi,flip,hugo,ida]],
%%%  [[aldo,beat],[carla,hugo],[david,evi,flip,gary,ida]],
%%%  [[aldo,beat],[carla,ida],[david,evi,flip,gary,hugo]],
%%%  [[aldo,beat],[david,evi],[carla,flip,gary,hugo,ida]],
%%%  [[aldo,beat],[david,flip],[carla,evi,gary,hugo,ida]],
%%%  [[aldo,beat],[david,gary],[carla,evi,flip,hugo,ida]],
%%%  [[aldo,beat],[david,hugo],[carla,evi,flip,gary,ida]],
%%%  [[aldo,beat],[david,ida],[carla,evi,flip,gary,hugo]],
%%%  [[aldo,beat],[evi,flip],[carla,david,gary,hugo,ida]],
%%%  [[aldo,beat],[evi,gary],[carla,david,flip,hugo,ida]],
%%%  [[aldo,beat],[evi,hugo],[carla,david,flip,gary,ida]],
%%%  [[aldo,beat],[evi,ida],[carla,david,flip,gary,hugo]],
%%%  [[aldo,beat],[flip,gary],[carla,david,evi,hugo,ida]],
%%%  [[aldo,beat],[flip,hugo],[carla,david,evi,gary,ida]],
%%%  [[aldo,beat],[flip,ida],[carla,david,evi,gary,hugo]],
%%%  [[aldo,beat],[gary,hugo],[carla,david,evi,flip,ida]],
%%%  [[aldo,beat],[gary,ida],[carla,david,evi,flip,hugo]],
%%%  [[aldo,beat],[hugo,ida],[carla,david,evi,flip,gary]],
%%%  [[aldo,carla],[beat,david],[evi,flip,gary,hugo|...]],
%%%  [[aldo,carla],[beat,evi],[david,flip,gary|...]],
%%%  [[aldo,carla],[beat,flip],[david,evi|...]],
%%%  [[aldo,carla],[beat,gary],[david|...]],
%%%  [[aldo,carla],[beat|...],[...]],
%%%  [[aldo|...],[...]|...],
%%%  [[...]|...],
%%%  [...]|...]
%%% 2>
%%% '''
%%%
%%% Note that we do not want permutations of the group members;
%%% i.e. [[aldo,beat],...] is the same solution as
%%% [[beat,aldo],...]. However, we make a difference between
%%% [[aldo,beat],[carla,david],...] and
%%% [[carla,david],[aldo,beat],...].
%%%
%%% You may find more about this combinatorial problem in a good book
%%% on discrete mathematics under the term "multinomial coefficients".
%%%
%%% </dd>

combination1(0, L) ->
    [{[], L}];
combination1(_, []) ->
    [];
combination1(N, [H|T]) ->
   [ {[H|Choice], Rest} || {Choice, Rest} <- combination1(N-1, T) ]
   ++
   [ {Choice, [H|Rest]} || {Choice, Rest} <- combination1(N, T)].


group(_, []) ->
    [[]];
group(List, [H|T]) ->
    [ [Choice|Group]
      || {Choice, Rest} <- combination1(H, List),
         Group <- group(Rest, T)].

group3(List) ->
    group(List, [2,3,4]).


%%% <dt>P28: Sorting a list of lists according to length of sublists</dt>
%%%
%%% <dd>
%%% a) We suppose that a list (InList) contains elements that are
%%% lists themselves. The objective is to sort the elements of InList
%%% according to their length. E.g. short lists first, longer lists
%%% later, or vice versa.
%%%
%%% Example:
%%%
%%% ```
%%% 1> p99:lsort([[a,b,c],[d,e],[f,g,h],[d,e],[i,j,k,l],[m,n],[o]]).
%%% [[o],[d,e],[d,e],[m,n],[a,b,c],[f,g,h],[i,j,k,l]]
%%% 2>
%%% '''
%%%
%%% b) Again, we suppose that a list (InList) contains elements that
%%% are lists themselves. But this time the objective is to sort the
%%% elements of InList according to their length frequency; i.e. in
%%% the default, where sorting is done ascendingly, lists with rare
%%% lengths are placed first, others with a more frequent length come
%%% later.
%%%
%%% Example:
%%%
%%% ```
%%% '''
%%%
%%% Note that in the above example, the first two lists in the result
%%% L have length 4 and 1, both lengths appear just once. The third
%%% and forth list have length 3 which appears, there are two list of
%%% this length. And finally, the last three lists have length 2. This
%%% is the most frequent length.
%%%
%%% </dd>


merge([], [], _Key) ->
    [];
merge([], L2, _Key) ->
    L2;
merge(L1, [], _Key) ->
    L1;
merge([H1|T1]=L1, [H2|T2]=L2, Key) ->
    case Key(H1) =< Key(H2) of
        true ->
            [H1|merge(T1, L2, Key)];
        false ->
            [H2|merge(L1, T2, Key)]
    end.


mergebypair([], _Key) ->
    [];
mergebypair([L], _Key) ->
    [L];
mergebypair([H1,H2|L], Key) ->
    [merge(H1,H2,Key)|mergebypair(L,Key)].


mergelists([], _Key) ->
    [];
mergelists([L], _Key) ->
    L;
mergelists(Lists, Key) ->
    mergelists(mergebypair(Lists, Key), Key).

mergesort(List, Key) ->
    mergelists([[E] || E <- List], Key).


lsort(List) ->
    mergesort(List, fun my_length/1).


%%% <dt></dt>
%%%
%%% <dd>
%%% Example:
%%%
%%% ```
%%% '''
%%% </dd>

%%% </dl>


%% </body>
%% </html>





format_prompt(Prompt, Encoding) ->
    lists:flatten(io_lib:format_prompt(Prompt, Encoding)).


skip_banner() ->
    receive
        {io_request, From, ReplyAs,
         {put_chars, unicode, _M, _F, _A}} ->
            From ! {io_reply, ReplyAs, ok}
    end.


consume([], Text) ->
    Text;
consume([H|T1], [H|T2]) ->
    consume(T1, T2);
consume(" ", "") ->
    "\n";
consume(Got, Expected) ->
    throw({output_mismatch, [{expected, Expected}, {got, Got}]}).


read_input(Text, Prompt, Module, Function, Continuation, ExtraArgs) ->
    Text1 = consume(format_prompt(Prompt, unicode), Text),
    case Text1 of
        "\n" ->
            {eof, []};
        _ ->
            {Line, Text2} =
                case string:str(Text1, "\n") of
                    0 ->
                        {Text1, []};
                    Index ->
                        {lists:sublist(Text1, 1, Index),
                         lists:nthtail(Index, Text1)}
                end,

            case apply(Module, Function, [Continuation, Line|ExtraArgs]) of
                {done, Result, []} ->
                    {Result, Text2};
                {more, Continuation1} ->
                    read_input(Text2, Prompt, Module, Function, Continuation1, ExtraArgs)
            end
    end.


wait_terminate() ->
    receive
        {io_request, _From, _ReplyAs, {put_chars, unicode, M, F, A}} ->
            "*** Terminating erlang (nonode@nohost)\n" = lists:flatten(apply(M, F, A))
    end.


session_loop(Text) ->
    receive
        {io_request, From, ReplyAs,
         {get_until, unicode, Prompt, Module, Function, ExtraArgs}} ->
            {Result, Text1} = read_input(Text, Prompt, Module, Function, [], ExtraArgs),
            From ! {io_reply, ReplyAs, Result},
            case Result of
                eof ->
                    wait_terminate();
                _ ->
                    session_loop(Text1)
            end;
        {io_request, From, ReplyAs, getopts} ->
            From ! {io_reply, ReplyAs, []},
            session_loop(Text);
        {io_request, From, ReplyAs, {get_geometry, columns}} ->
            From ! {io_reply, ReplyAs, 80},
            session_loop(Text);
        {io_request, From, ReplyAs, {requests, Requests}} ->
            Chars =
                lists:flatten(
                  [unicode:characters_to_list(Chars, Encoding)
                   || {put_chars, Encoding, Chars} <- Requests]),
            Text1 = consume(Chars, Text),
            From ! {io_reply, ReplyAs, ok},
            session_loop(Text1);
        {io_request, From, ReplyAs, {put_chars, unicode, Module, Function, Args}} ->
            Text1 = consume(unicode:characters_to_list(apply(Module, Function, Args)), Text),
            From ! {io_reply, ReplyAs, ok},
            session_loop(Text1);
        Data ->
            io:format("Unknown Data: ~w~n~n", [Data])
    end.


start_verify_session(Text) ->
    process_flag(trap_exit, true),
    G = group_leader(),
    group_leader(self(), self()),
    Shell = shell:start(),
    group_leader(G, self()),
    link(Shell),

    skip_banner(), %% Restricted
    skip_banner(),
    session_loop(Text),
    exit(Shell, kill),
    receive
        {'EXIT', _, killed} ->
            ok
    end.


verify_session(Text) ->
    process_flag(trap_exit, true),
    Pid = spawn_link(fun() -> start_verify_session(Text) end),
    receive
        {'EXIT', From, Reason}
          when From =:= Pid ->
            Reason
    end.


verify_sessions(Sessions) ->
    lists:map(
      fun(Session) ->
              {Session, verify_session(Session)}
      end,
      Sessions).


format_exception(Reason, StackTrace) ->
    lib:format_exception(
      1, error, Reason, StackTrace,
      fun(M, _F, _A) ->
              (M =:= erl_eval) or (M =:= ?MODULE)
      end,
      fun(V, I) ->
              io_lib_pretty:print(
                V,
                [{column, I},
                 {line_length, 80},
                 {strings, true},
                 {depth, 30},
                 {max_chars, 60}])
      end,
      unicode).


local_allowed(Func, ArgList, State) ->
    shell:local_allowed(Func, ArgList, State).


non_local_allowed({p99, Func}, ArgList, State) ->
    {{redirect, {?MODULE, Func}, ArgList}, State};
non_local_allowed({erlang, _}, _, State) -> %% for '-'
    {true, State};
non_local_allowed(Func, ArgList, State) ->
    shell:non_local_allowed(Func, ArgList, State).


parse_document() ->
    Comments = edoc:read_comments(?FILE, []),
    Content =
        string:join(
          [
           string:join([ case Line of [$\s|T] -> T; [] -> [] end || [$%, $%|Line] <- Comment], "\n")
           || {_,_,_,Comment} <- Comments], "\n\n"),

    XmlDoc = edoc_wiki:parse_xml(string:strip(Content), 1),
    lists:flatten(
      string:join(
        ["<html>",
         "<head>",
         "<title>Ninety-Nine Erlang Problems</title>",
         "</head>",
         "<body>",
         xmerl:export_simple(XmlDoc, xmerl_sgml),
         "</body>",
         "</html>"],
        "\n")).


test_document(XML) ->
    Sessions =
        [lists:flatten([ Text || #xmlText{value=Text} <- Content ])
         || #xmlElement{content=Content} <- xmerl_xpath:string("//pre", XML)],

    application:set_env(stdlib, restricted_shell, ?MODULE),
    Results = verify_sessions([Session || "1> "++_=Session <- Sessions]),
    application:unset_env(stdlib, restricted_shell),
    Results.



main(["html"]) ->
    HTML = parse_document(),
    io:format("~s~n", [["<!DOCTYPE html>\n"|HTML]]),
    ok;
main(["test"]) ->
    HTML = parse_document(),
    {XmlElem, _} = xmerl_scan:string(HTML),
    Results = test_document(XmlElem),

    lists:foreach(
      fun
          ({_, normal}) ->
              ok;
          ({Session, {Reason, StackTrace}}) ->
              Ex = format_exception(Reason, StackTrace),
              io:format("=SHELL SESSION ERROR====~nError occurs in ~s:~n~s~n~s~n~n", [?FILE, Session, Ex])
      end,
      Results),

    io:format("~nRESULT: ~w/~w~n", [length([ok ||{_, normal} <- Results]), length(Results)]),
    ok.



test() ->
    io:format("~p~n", [my_flatten([a, [b, [c, d], e]])]).
