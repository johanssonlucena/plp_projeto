:-use_module(library(csv)).
:-include('Arquivos.pl').

cadastraVeiculo(Numero, Placa, Modelo, Cor) :-
    open('../arquivos/Veiculos.csv', append, File),
    writeln(File, (Numero, Placa, Modelo, Cor)),
    close(File).


%--------------------------------------------------------------------------------
mostraColunasVeiculos:-
    write('┌──────────────────────Veiculos────────────────────────┐\n').


atributosVeiculo(['││ Número: ', '││ Placa: ', '││ Modelo: ', '││ Cor: ']).

pegaAtributosVeiculo(I, Atributos):-
    atributosVeiculo(X), nth1(I, X, Atributos).
   
mostraListaVeiculos([], _).
mostraListaVeiculos([H|T], X):-
    Next is X + 1,
    pegaAtributosVeiculo(X, Atributos),
    write(Atributos),
    write(H), write('\n'),
    mostraListaVeiculos(T, Next).


mostraListaVeiculo([],_).
mostraListaVeiculo([H|T], X):-
    mostraListaVeiculos(H, X),
    write('├──────────────────────────────────────────────────────┤\n'),
    mostraListaVeiculo(T,X).

mostraVeiculos(Veiculos):-
    mostraColunasVeiculos,
    mostraListaVeiculo(Veiculos, 1).
