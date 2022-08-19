:- initialization(main).
:- include('Casa.pl').
:- include('Morador.pl').
:- include('Veiculo.pl').
:- include('Visitante.pl').
:- include('Funcionario.pl').



/* UTILITARIOS */

up(119).
down(115).
left(97).
right(100).
select(113).
remotion(101).

/* -------------------- */
upAction(0, Limit, Limit).
upAction(Cursor, _, NewCursor) :-
    NewCursor is Cursor - 1.

downAction(Cursor, Limit, NewCursor) :-
    Max is Limit + 1,
    PC is Cursor + 1,
    NewCursor is PC mod Max.

remotionExitAction(Mensagem, Resultado) :-
    shell(clear),
    writeln(Mensagem),
    get_single_char(Input),
    (remotion(Input) -> Resultado is 1;
        Resultado is 0).

mostrarOpcoes([], _, _).
mostrarOpcoes([A|As], Cursor, Cursor) :- 
    write('-> '),
    writeln(A),
    N is Cursor + 1,
    mostrarOpcoes(As, Cursor, N).
    
mostrarOpcoes([A|As], Cursor, N) :- 
    write('   '),
    writeln(A),
    NewN is N + 1,
    mostrarOpcoes(As, Cursor, NewN).




/* -> FUNCOES PARA IO */

getString(FinalInput, Mensagem) :-
    write('\n'),
    writeln(Mensagem),
    read_line_to_codes(user_input, Entrada), atom_string(Entrada, Return),
    FinalInput = Return.

getInt(FinalInput, Mensagem) :-
    write('\n'),
    writeln(Mensagem),
    read_line_to_codes(user_input, Entrada), atom_string(Entrada, Return),
    (number_string(Number, Return), Number >= 0 -> FinalInput = Number;
        getDouble(NewF, 'Entrada invalida! Tente digitar um número.\n digite novamente!'), FinalInput is NewF).





/*  TELA PRINCIPAL */

opcaoMenuPrincipal(['Acesso como Administrador', 'Acesso como Portaria', 'Sair']).
limitMain(2).

/* ------------------------------------------------------------------------------ */

mensagemSaida() :-
    shell(clear),
    writeln('----------------------------------------------'),
    writeln('* OBRIGADO POR UTILIZAR O SISTEMA DE CONDOMINIO SC *'),
    writeln('----------------------------------------------\n').

telaPrincipal(Cursor, Action) :-
    limitMain(Limit),
    (up(Action) -> upAction(Cursor, Limit, NewCursor), menuPrincipal(NewCursor);
     down(Action) -> downAction(Cursor, Limit, NewCursor), menuPrincipal(NewCursor);
     left(Action) -> mensagemSaida();
     right(Action) -> (Cursor =:= 0 -> administradorTelaPrincipal(0);
                       Cursor =:= 1 -> portariaTelaPrincipal(0);
                       Cursor =:= 2 -> mensagemSaida());
                
     menuPrincipal(Cursor)).

menu('\nBEM VINDO AO SISTEMA DE CONDOMINIO - SC.
  
╔═════════════════════════════════════════╗
║  (w,s) Para mover o cursor Cima e Baixo ║
║  (a)   Para retornar a tela anterior    ║
║  (d)   Para entrar na opção desejada    ║
╚═════════════════════════════════════════╝\n').

menuPrincipal(Cursor) :-
    shell(clear),
    menu(X),
    writeln(X),
    opcaoMenuPrincipal(ListaOpcoes),
    mostrarOpcoes(ListaOpcoes, Cursor, 0),
    get_single_char(Action),
    telaPrincipal(Cursor, Action).

% ---------------------------------------TELA OPCOES ADMINISTRAÇÃO------------------------------------------------------

opcaoAdministrador(['Cadastrar Casa', 'Cadastrar Moradores', 'Cadastrar Veiculos', 'Cadastrar Visitantes Autorizados', 'Cadastrar Funcionarios e Prestadores de Serviço']).
limitMaster(4).

telaAdministrador(Cursor, Action) :-
    limitMaster(Limit),
    (up(Action) -> upAction(Cursor, Limit, NewCursor), administradorTelaPrincipal(NewCursor);
     down(Action) -> downAction(Cursor, Limit, NewCursor), administradorTelaPrincipal(NewCursor);
     left(Action) -> menuPrincipal(Cursor);
     right(Action) -> (Cursor =:= 0 -> cadastrarCasa();
                       Cursor =:= 1 -> cadastrarMorador();
                       Cursor =:= 2 -> cadastrarVeiculo();
                       Cursor =:= 3 -> cadastrarVisitantes();
                       Cursor =:= 4 -> cadastrarFuncPrest();
                       Cursor =:= 5 -> cadastrar());
     administradorTelaPrincipal(Cursor)).

administradorTelaPrincipal(Cursor) :-
    shell(clear),
    menu(X),
    writeln(X),
    opcaoAdministrador(ListaOpcoes),
    mostrarOpcoes(ListaOpcoes, Cursor, 0),
    get_single_char(Action),
    telaAdministrador(Cursor, Action).

% ---------------------------------------- TELA CADASTRAR CASA ----------------------------------------
cadastrarCasa() :-
    shell(clear),
    getInt(Numero, 'Digite o Numero da Casa'),
    getString(Nome, 'Digite o nome do Responsável'),
    getString(Cpf, 'Digite o CPF do Responsavel'),
    getString(Rua, 'Digite a rua da Casa'),            
    
    cadastraCasa(Numero, Rua, Nome, Cpf),            
    
    write('\nCasa cadastrado com sucesso!'),
    get_single_char(Action),
    administradorTelaPrincipal(0).


% ---------------------------------------- TELA CADASTRAR MORADOR ----------------------------------------
cadastrarMorador() :-
    shell(clear),
    getInt(Casa, 'Digite o Numero da Casa do Morador'),
    getString(Nome, 'Digite o nome do Morador'),
    getString(Cpf, 'Digite o CPF do Morador'),
    getString(Nascimento, 'Digite a data de Nascimento do Morador'),
    getString(Sexo, 'Digite o sexo do Morador'),                        
    
    cadastraMorador(Casa, Nome, Cpf, Nascimento, Sexo),            
   
    write('\nMorador cadastrado com sucesso!\n'),
    get_single_char(Action),
    administradorTelaPrincipal(0).


% ---------------------------------------- TELA CADASTRAR VEICULO ----------------------------------------
cadastrarVeiculo() :-
    shell(clear),
    getInt(Casa, 'Digite o Numero da Casa do Veiculo'),
    getString(Placa, 'Digite a placa do Veículo'),
    getString(Modelo, 'Digite o modelo do Veículo'),
    getString(Cor, 'Digite a cor do Veículo'),                        
    
    cadastraVeiculo(Casa, Placa, Modelo, Cor),            
   
    write('\nVeiculo cadastrado com sucesso!\n'),
    get_single_char(Action),
    administradorTelaPrincipal(0).


% ---------------------------------------- TELA CADASTRAR VISITANTES ----------------------------------------
cadastrarVisitantes() :-
    shell(clear),
    getInt(Casa, 'Digite o Numero da Casa do Visitante'),
    getString(Nome, 'Digite o nome do Visitante'),
    getString(Cpf, 'Digite o CPF do Visitante'),                       
    
    cadastraVisitante(Casa, Nome, Cpf),            
   
    write('\nVisitante cadastrado com sucesso!\n'),
    get_single_char(Action),
    administradorTelaPrincipal(0).


% --------------------------------- TELA CADASTRAR FUNCIONÁRIO E PRESTADOR DE SERVIÇO------------------------------------
cadastrarFuncPrest() :-
    shell(clear),
    getString(Nome, 'Digite o nome do Funcionario ou Prestador de Serviço'),
    getString(Cpf, 'Digite o CPF do Funcionario ou Prestador de Serviço'), 
    getString(Funcao, 'Digite a Função do Funcionario ou Prestador de Serviço'),                       
    
    cadastraFuncPrest(Nome, Cpf, Funcao),            
   
    write('\nFuncionario ou Prestador de Serviço cadastrado com sucesso!\n'),
    get_single_char(Action),
    administradorTelaPrincipal(0).






% ---------------------------------TELA OPCOES PORTARIA--------------------------------------------
opcaoPortaria(['Visualizar Casas', 'Visualizar Moradores', 'Visualizar Veiculos', 'Visualizar Visitantes Autorizados', 'Visualizar Funcionarios e Prestadores de Serviço']).
limitePortaria(4).

opcaoPortaria(Cursor, Action) :-
    limitePortaria(Limit),
    (up(Action) -> upAction(Cursor, Limit, NewCursor), portariaTelaPrincipal(NewCursor);
     down(Action) -> downAction(Cursor, Limit, NewCursor), portariaTelaPrincipal(NewCursor);
     left(Action) -> menuPrincipal(Cursor);
     right(Action) -> (Cursor =:= 0 -> visualizarCasas();
                       Cursor =:= 1 -> visualizarMoradores();
                       Cursor =:= 2 -> visualizarVeiculos();
                       Cursor =:= 3 -> visualizarVisitantes();
                       Cursor =:= 4 -> visualizarFuncionarios());
     portariaTelaPrincipal(Cursor)).

portariaTelaPrincipal(Cursor) :-
    shell(clear),
    menu(X),
    writeln(X),
    opcaoPortaria(ListaOpcoes),
    mostrarOpcoes(ListaOpcoes, Cursor, 0),
    get_single_char(Action),
    opcaoPortaria(Cursor, Action).


% ---------------------------------------- TELA VISUALIZAR CASAS ---------------------------------------
visualizarCasas() :-
    shell(clear),
    lerCsvRowList('Casas.csv', Casas),
    mostraCasas(Casas),
    get_single_char(Action),
    menuPrincipal(0).


% ---------------------------------------- TELA VISUALIZAR MORADORES ---------------------------------------
visualizarMoradores() :-
    shell(clear),
    lerCsvRowList('Moradores.csv', Moradores),
    mostraMoradores(Moradores),
    get_single_char(Action),
    menuPrincipal(0).


% ---------------------------------------- TELA VISUALIZAR VEICULOS ---------------------------------------
visualizarVeiculos() :-
    shell(clear),
    lerCsvRowList('Veiculos.csv', Veiculos),
    mostraVeiculos(Veiculos),
    get_single_char(Action),
    menuPrincipal(0).


% ---------------------------------------- TELA VISUALIZAR VISITANTES ---------------------------------------
visualizarVisitantes() :-
    shell(clear),
    lerCsvRowList('Visitantes.csv', Visitantes),
    mostraVisitantes(Visitantes),
    get_single_char(Action),
    menuPrincipal(0).





% ---------------------------------------- TELA VISUALIZAR FUNCIONARIOS ---------------------------------------
visualizarFuncionarios() :-
    shell(clear),
    lerCsvRowList('Funcionarios.csv', Funcionarios),
    mostraFuncionarios(Funcionarios),
    get_single_char(Action),
    menuPrincipal(0).




% -----------------------------------------TELA SAIR------------------------------------------------------
   
main :-
    menuPrincipal(0),
    halt(0).
