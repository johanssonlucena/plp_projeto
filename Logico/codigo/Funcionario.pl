:-use_module(library(csv)).
:-include('Arquivos.pl').

cadastraFuncPrest(Nome, Cpf, Funcao) :-
    open('../arquivos/Funcionarios.csv', append, File),
    writeln(File, (Nome, Cpf, Funcao)),
    close(File).


%--------------------------------------------------------------------------------
mostraColunasFuncionarios:-
    write('┌────────────────────Funcionarios──────────────────────┐\n').


atributosFuncionario(['││ Nome: ', '││ Cpf: ', '││ Função: ']).

pegaAtributosFuncionario(I, Atributos):-
    atributosFuncionario(X), nth1(I, X, Atributos).
   
mostraListaFuncionarios([], _).
mostraListaFuncionarios([H|T], X):-
    Next is X + 1,
    pegaAtributosFuncionario(X, Atributos),
    write(Atributos),
    write(H), write('\n'),
    mostraListaFuncionarios(T, Next).


mostraListaFuncionario([],_).
mostraListaFuncionario([H|T], X):-
    mostraListaFuncionarios(H, X),
    write('├──────────────────────────────────────────────────────┤\n'),
    mostraListaFuncionario(T,X).

mostraFuncionarios(Funcionarios):-
    mostraColunasFuncionarios,
    mostraListaFuncionario(Funcionarios, 1).
