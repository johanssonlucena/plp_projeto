:-use_module(library(csv)).
:-include('Arquivos.pl').

cadastraCasa(Numero, Rua, Nome, Cpf) :-
    open('../arquivos/Casas.csv', append, File),
    writeln(File, (Numero, Rua, Nome, Cpf)),
    close(File).


%--------------------------------------------------------------------------------
mostraColunasCasas:-
    write('┌───────────────────────Casas──────────────────────────┐\n').


atributosCasa(['││ Número: ', '││ Rua: ', '││ Responsável: ', '││ Cpf: ']).

pegaAtributosCasa(I, Atributos):-
    atributosCasa(X), nth1(I, X, Atributos).
   
mostraListaCasas([], _).
mostraListaCasas([H|T], X):-
    Next is X + 1,
    pegaAtributosCasa(X, Atributos),
    write(Atributos),
    write(H), write('\n'),
    mostraListaCasas(T, Next).


mostraListaCasa([],_).
mostraListaCasa([H|T], X):-
    mostraListaCasas(H, X),
    write('├──────────────────────────────────────────────────────┤\n'),
    mostraListaCasa(T,X).

mostraCasas(Casas):-
    mostraColunasCasas,
    mostraListaCasa(Casas, 1).

























%--------------------------------------------------------------------------------

mostraColunasSintomas:-
    write('┌─────────────────Visualização por Sintoma─────────────────┐\n').
    % write('|                    1. Id, 2. Sintomas                    |\n'),
    % write('────────────────────────────────────────────────────────────\n').
    
caracteristicasProduto2(['││ Id: ', '││ Sintomas: ']).

pegaCaracteristica2(I, Caracteristica):-
    caracteristicasProduto2(X), nth1(I, X, Caracteristica).

mostraListaSintomaProduto([], _).
mostraListaSintomaProduto([H|T], X):-
    Next is X + 1,
    pegaCaracteristica2(X, Caracteristica),
    write(Caracteristica),
    write(H), write('\n'),
    mostraListaSintomaProduto(T, Next).


mostraListaSintoma([],_).
mostraListaSintoma([H|T], X):-
    mostraListaSintomaProduto(H, X),
    write('├──────────────────────────────────────────────────────────┤\n'),
    mostraListaSintoma(T,X).

mostraSintomaProduto(SintomaProduto):-
    mostraColunasSintomas,
    mostraListaSintoma(SintomaProduto, 1).

getProdutoPorId(_, [], false).
getProdutoPorId(IdProduto, [H|[]], P):- getProduto(IdProduto, H, P).
getProdutoPorId(IdProduto, [H|T], P):- getProdutoPorId(IdProduto, T, P).

getProduto(_, [], false).
getProduto(IdProduto, [IdProduto|Resto], [IdProduto|Resto]).
getProduto(IdProduto, [_|T], R):- getProduto(IdProduto, T, R).
