:- module(manipulation, [switch_if/2]).

:- use_module(jsprinter).
:- use_module(jsparser).



sample_if("if (x == 1) {
    print('x is 1');
 } else if (x == 2) {
    print('x is 2');
}else if (x == 3) {
    print('x is 3');
} else{
   print('x is another value');
}").

sample_switch("switch(w) {
   case 1:
     print('first');
     break;
   case 2:
     print('second');
     break;
   default:
     print('Nothing');
     break;
}").

sample_block("{
   var i = 0;
   if (i > 10) {
     while(i > 20) {
        printf('voy');
        if (i > 29) {
          return 39;
        }
        i = i + 1;
     }
   } else {
      return 20;   
   }       
}").

flow_graph(js_block(Stats,_), graph(Arcs, Nodes)) :-
    Current = start,
    StartNodes = [node(start,start), node(exit,exit)],
    creating_flow_graph(
        Stats,
        [],
        Arcs,
        StartNodes,
        Nodes,
        Current).

creating_flow_graph([],
                    CurrentArcs,
                    [arc(Current, exit)|CurrentArcs],
                    Nodes,
                    Nodes,
                    Current).


creating_flow_graph([Any|Rest],
                    CurrentArcs,
                    NewArcs,
                    CurrentNodes,
                    NewNodes,
                    Current) :-
    gensym("node", NodeId),
    TmpNewNodes = [node(NodeId, Any)| CurrentNodes],
    TmpNewArcs = [ arc(Current, Any)| CurrentArcs ],
    creating_flow_graph(Rest,
                        TmpNewArcs,
                        NewArcs,
                        TmpNewNodes,
                        NewNodes,
                        Any).

switch_if_cases(Variable,
                [js_case(Value, Body, _)|Rest],
                js_if(js_binary_operation(
                          equals_op,
                          js_identifier(Variable, _),
                          Value,
                          _),
                      js_block(NoBreakBody, _),
                      RestIf,
                      _)) :-
     body_with_no_break(Body, NoBreakBody),
     switch_if_cases(Variable, Rest, RestIf).

body_with_no_break([js_break(_)], []).
body_with_no_break([Stat|Rest1], [Stat|Rest2]) :-
    body_with_no_break(Rest1, Rest2).

switch_if(js_switch(js_identifier(Variable, _),
                    Cases,
                   _),
          IfStat) :-
       switch_if_cases(Variable, Cases, IfStat).
