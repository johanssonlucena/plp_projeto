:-use_module(library(csv)).
:-include('Arquivos.pl').

cadastraVisitante(Numero, Nome, Cpf) :-
    open('../arquivos/Visitantes.csv', append, File),
    writeln(File, (Numero, Nome, Cpf)),
    close(File).


%--------------------------------------------------------------------------------
mostraColunasVisitantes:-
    write('┌─────────────────────Visitantes───────────────────────┐\n').


atributosVisitante(['││ Número: ', '││ Nome: ', '││ Cpf: ']).

pegaAtributosVisitante(I, Atributos):-
    atributosVisitante(X), nth1(I, X, Atributos).
   
mostraListaVisitantes([], _).
mostraListaVisitantes([H|T], X):-
    Next is X + 1,
    pegaAtributosVisitante(X, Atributos),
    write(Atributos),
    write(H), write('\n'),
    mostraListaVisitantes(T, Next).


mostraListaVisitante([],_).
mostraListaVisitante([H|T], X):-
    mostraListaVisitantes(H, X),
    write('├──────────────────────────────────────────────────────┤\n'),
    mostraListaVisitante(T,X).

mostraVisitantes(Visitantes):-
    mostraColunasVisitantes,
    mostraListaVisitante(Visitantes, 1).
