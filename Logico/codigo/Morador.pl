:-use_module(library(csv)).
:-include('Arquivos.pl').

cadastraMorador(Casa, Nome, Cpf, Nascimento, Sexo) :-
    open('../arquivos/Moradores.csv', append, File),
    writeln(File, (Casa, Nome, Cpf, Nascimento, Sexo)),
    close(File).


%--------------------------------------------------------------------------------
mostraColunasMoradores:-
    write('┌─────────────────────Moradores────────────────────────┐\n').


atributosMorador(['|| Casa: ', '││ Nome: ', '││ Cpf: ', '││ Nascimento: ', '││ Sexo: ']).

pegaAtributosMorador(I, Atributos):-
    atributosMorador(X), nth1(I, X, Atributos).
   
mostraListaMoradores([], _).
mostraListaMoradores([H|T], X):-
    Next is X + 1,
    pegaAtributosMorador(X, Atributos),
    write(Atributos),
    write(H), write('\n'),
    mostraListaMoradores(T, Next).


mostraListaMorador([],_).
mostraListaMorador([H|T], X):-
    mostraListaMoradores(H, X),
    write('├──────────────────────────────────────────────────────┤\n'),
    mostraListaMorador(T,X).

mostraMoradores(Moradores):-
    mostraColunasMoradores,
    mostraListaMorador(Moradores, 1).









