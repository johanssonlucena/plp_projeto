import System.IO 
import System.IO.Unsafe
import Control.Exception
import System.IO.Error 
import System.Process
import Control.Monad (when)
import Text.Printf
import System.IO.Unsafe

--      --> IMPORTS DAS ENTIDADES <--

import Morador
import Casa
import Veiculo
import VisitanteAutorizado
import Funcionario

--       -->  METODOS AUXILIARES  <--

getKey :: IO [Char]
getKey = reverse <$> getKey' ""
  where getKey' chars = do
          char <- getChar
          more <- hReady stdin
          (if more then getKey' else return) (char:chars)

showSimpleScreen :: [String] -> Integer -> Integer -> IO()
showSimpleScreen [] cursor contador = return ()
showSimpleScreen (o:os) cursor contador = do
   if contador == cursor
      then 
      putStrLn("->" ++ o)
   else
      putStrLn("  " ++ o)
   showSimpleScreen os cursor (contador+1)

lerEntradaString :: IO String
lerEntradaString = do
         hSetBuffering stdin LineBuffering
         hSetEcho stdin True
         x <- getLine
         return x

lerEntradaInt :: IO Int
lerEntradaInt = do
         hSetBuffering stdin LineBuffering
         hSetEcho stdin True
         x <- readLn
         return x

lerEntradaDouble :: IO Double
lerEntradaDouble = do
         hSetBuffering stdin LineBuffering
         hSetEcho stdin True
         x <- readLn
         return x



--      -->  TELA INICIAL  <--

opcoesTelaInicial :: [String]
opcoesTelaInicial = ["Acesso como Administracao", "Acesso como Portaria", "Sair"]

doTelaInicial :: Integer -> [Char] -> IO ()
doTelaInicial cursor action | action == "\ESC[B" = telaInicial ((cursor+1) `mod` 4)
                                                    | action == "\ESC[A" && cursor /= 0 = telaInicial (cursor-1)
                                                    | action == "\ESC[A" && cursor == 0 = telaInicial 3
                                                    | action == "\ESC[C" = mudarTelaInicial cursor
                                                    | action == "\ESC[D" = putStrLn("\n           Sistema Encerrado.           \n")
                                                    | otherwise = telaInicial cursor

mudarTelaInicial :: Integer -> IO()
mudarTelaInicial cursor                          | cursor == 0 = do telaOpcoesAdministracao 0
                                                 | cursor == 1 = do telaOpcoesPortaria 0            
                                                 | cursor == 2 = do sair


telaInicial :: Integer -> IO ()
telaInicial cursor = do
   
   system "clear"
   putStrLn("Bem-vindo ao Gerenciamento de Condomínio Residencial!")
   putStrLn("Como desejas acessar?\n")
   putStrLn("|| Utilize os direcionais do teclado (cima, baixo) para mover o cursor ||\n")
   putStrLn("|| Utilize o direcional frente para selecionar a opcao desejada ||\n")
   showSimpleScreen opcoesTelaInicial cursor 0
   
   hSetBuffering stdin NoBuffering
   hSetEcho stdin False
   action <- getKey
   doTelaInicial cursor action




---------------------------------------------------------------------------------------------------------------------------
--      -->  ACESSO ADMINISTRACAO  <--

opcoesTelaAdm :: [String]
opcoesTelaAdm = ["Cadastrar Casa", "Cadastrar Moradores", "Cadastrar Veiculos", "Cadastrar Visitantes Autorizados", "Cadastrar Funcionarios e Prestadores de Servico"]

mudarTelaOpcoesAdm :: Integer -> IO ()
mudarTelaOpcoesAdm cursor
   | cursor == 0 = cadastroCasaTela
   | cursor == 1 = cadastroMoradorTela
   | cursor == 2 = cadastroVeiculoTela          
   | cursor == 3 = cadastroVisitanteTela                   
   | cursor == 4 = cadastroFuncionarioeOutros

doOpcoesAdm :: Integer -> [Char] -> IO ()
doOpcoesAdm cursor action | action == "\ESC[B" = telaOpcoesAdministracao ((cursor+1) `mod` 5)
                                                    | action == "\ESC[A" && cursor /= 0 = telaOpcoesAdministracao (cursor-1)
                                                    | action == "\ESC[A" && cursor == 0 = telaOpcoesAdministracao 4
                                                    | action == "\ESC[C" = mudarTelaOpcoesAdm cursor
                                                    | action == "\ESC[D" = telaInicial 0
                                                    | otherwise = telaOpcoesAdministracao cursor


telaOpcoesAdministracao :: Integer -> IO ()
telaOpcoesAdministracao cursor = do
   
   system "clear"
   putStrLn ("Bem vindo, Sr(a). Administrador! \n|| Tecle (Seta Direita) para escolher qual opcão acessar e (Seta Esquerda) para voltar à tela anterior. ||\n")
   showSimpleScreen opcoesTelaAdm cursor 0
   hSetBuffering stdin NoBuffering
   hSetEcho stdin False
   action <- getKey
   doOpcoesAdm cursor action



--   > OPCAO CADASTRAR CASA  <--

listaInicial :: [Morador]
listaInicial = []

cadastroCasaTela :: IO ()
cadastroCasaTela = do
   system "clear"

   putStrLn ("Digite o Nome do responsável da Casa")
   responsavel <- lerEntradaString

   putStrLn ("Digite o Cpf do responsável da Casa")
   cpf <- lerEntradaString

   putStrLn ("Digite o numeral da Casa:")
   numero <- lerEntradaString

   putStrLn ("Digite qual rua está localizado")
   rua <- lerEntradaString

   putStrLn ("Digite a data do cadastro (formato DD/MM/AAAA):")
   dataCadastro <- lerEntradaString

   putStrLn("\nCasa Cadastrada!\n")
   
   hSetBuffering stdin NoBuffering
   hSetEcho stdin False



   let casa = Casa numero rua responsavel cpf dataCadastro listaInicial
   let listaCasa = [casa]
   let adicionarCasa = listaCasa
   escreverArquivoCasa adicionarCasa
   
   action <- getKey
   telaInicial 0



--   > OPCAO CADASTRAR MORADOR  <--

cadastroMoradorTela :: IO ()
cadastroMoradorTela = do
   system "clear"

   putStrLn ("Digite a unidade residencial do morador:")
   casa <- lerEntradaString

   putStrLn ("Digite o nome do morador:")
   nome <- lerEntradaString

   putStrLn ("Digite o cpf do morador:")
   cpf <- lerEntradaString

   putStrLn ("Digite a data de Nascimento:")
   dataNascimento <- lerEntradaString

   putStrLn ("Digite o sexo do morador:")
   sexo <- lerEntradaString

   let morador = Morador casa nome cpf dataNascimento sexo 
   let listaMoradores = [morador]
   let adicionarMorador = listaMoradores
   escreverArquivoMorador adicionarMorador

   putStrLn("\nMorador cadastrado!\n")
   
   hSetBuffering stdin NoBuffering
   hSetEcho stdin False
   action <- getKey
   putStrLn (" ")

   telaInicial 0


--   > OPCAO CADASTRAR VEICULO  <--

cadastroVeiculoTela :: IO ()
cadastroVeiculoTela = do
   system "clear"

   putStrLn ("Digite a unidade residencial:")
   casa <- lerEntradaString

   putStrLn ("Digite a placa:")
   placa <- lerEntradaString

   putStrLn ("Digite a cor:")
   cor <- lerEntradaString

   putStrLn ("Digite o modelo:")
   modelo <- lerEntradaString

   let veiculo = Veiculo casa placa cor modelo 
   let listaVeiculo = [veiculo]
   let adicionarVeiculo = listaVeiculo
   escreverArquivoVeiculo adicionarVeiculo
   
   putStrLn("\nVeiculo cadastrado!\n")
   
   hSetBuffering stdin NoBuffering
   hSetEcho stdin False
   action <- getKey
   putStrLn (" ")

   telaInicial 0


--   > OPCAO CADASTRAR VISITANTE AUTORIZADO  <--

cadastroVisitanteTela :: IO ()
cadastroVisitanteTela = do
   system "clear"

   putStrLn ("Digite a unidade residencial:")
   casa <- lerEntradaString

   putStrLn ("Digite o cpf do visitante:")
   cpf <- lerEntradaString

   putStrLn ("Digite o nome do visitante:")
   nome <- lerEntradaString

   let visitante = VisitanteAutorizado casa cpf nome
   let listaVisitantes = [visitante]
   let adicionarVisitante = listaVisitantes
   escreverArquivoVisitante adicionarVisitante
   
   putStrLn("\nVisitante cadastrado!\n")
   
   hSetBuffering stdin NoBuffering
   hSetEcho stdin False
   action <- getKey
   putStrLn (" ")

   telaInicial 0


--   > OPCAO CADASTRAR FUNCIONARIO E PRESTADORES DE SERVICO  <--

cadastroFuncionarioeOutros :: IO ()
cadastroFuncionarioeOutros = do
   system "clear"

   putStrLn ("Digite o nome do Funcionario ou Prestador de Servico:")
   nome <- lerEntradaString

   putStrLn ("Digite o cpf:")
   cpf <- lerEntradaString

   putStrLn ("Digite a funcao:")
   funcao <- lerEntradaString


   let funcionario = Funcionario nome cpf funcao
   let listaFuncionarios = [funcionario]
   let adicionarFuncionario = listaFuncionarios
   escreverArquivoFuncionario adicionarFuncionario
   
   putStrLn("\nFuncionario cadastrado!\n")
   
   hSetBuffering stdin NoBuffering
   hSetEcho stdin False
   action <- getKey
   putStrLn (" ")

   telaInicial 0

---------------------------------------------------------------------------------------------------------------------------
--      -->  ACESSO PORTARIA  <--

opcoesTelaPortaria :: [String]
opcoesTelaPortaria = ["Listar Detalhes de Casa", "listar Veiculos", "listar Visitante", "listar FuncionarioseOutros"]

mudarTelaOpcoesPortaria :: Integer -> IO ()
mudarTelaOpcoesPortaria cursor
   | cursor == 0 = visualizarCasas
   | cursor == 1 = visualizarVeiculos
   | cursor == 2 = visualizarVisitante
   | cursor == 3 = visualizarFuncionarios

doOpcoesPortaria :: Integer -> [Char] -> IO ()
doOpcoesPortaria cursor action | action == "\ESC[B" = telaOpcoesPortaria ((cursor+1) `mod` 4)
                                                    | action == "\ESC[A" && cursor /= 0 = telaOpcoesPortaria (cursor-1)
                                                    | action == "\ESC[A" && cursor == 0 = telaOpcoesPortaria 3
                                                    | action == "\ESC[C" = mudarTelaOpcoesPortaria cursor
                                                    | action == "\ESC[D" = telaInicial 0
                                                    | otherwise = telaOpcoesPortaria cursor

telaOpcoesPortaria :: Integer -> IO ()
telaOpcoesPortaria cursor = do
   
   system "clear"
   putStrLn ("Bem vindo! \n\n|| Aperte (Seta Direita) para escolher qual opcão acessar e (Seta Esquerda) para voltar à tela anterior. ||\n")
   showSimpleScreen opcoesTelaPortaria cursor 0
   hSetBuffering stdin NoBuffering
   hSetEcho stdin False
   action <- getKey
   doOpcoesPortaria cursor action


--   > OPCAO LISTAR DETALHES CASA  <--

listarDetalhesCasa :: IO ()
listarDetalhesCasa = do
   system "clear"

   putStrLn ("Digite a unidade residencial:")
   casa <- lerEntradaString
   
   putStrLn("\nDetalhes da Casa: " ++ casa ++ "\n")
   
   hSetBuffering stdin NoBuffering
   hSetEcho stdin False
   action <- getKey
   putStrLn (" ")

   telaInicial 0


--   > OPCAO VERIFICAR VEICULO  <--

verificarVeiculo :: IO ()
verificarVeiculo = do
   system "clear"

   putStrLn ("Digite a placa do Veículo: (Formato: LLLNNN)")
   veiculo <- lerEntradaString
   
   putStrLn("\nVeículo: " ++ veiculo ++ "  Pertence a Unidade xx\n")
   
   hSetBuffering stdin NoBuffering
   hSetEcho stdin False
   action <- getKey
   putStrLn (" ")

   telaInicial 0


--   > OPCAO VERIFICAR VISITANTE  <--

verificarVisitante :: IO ()
verificarVisitante = do
   system "clear"

   putStrLn ("Digite a Unidade Residencial")
   casa <- lerEntradaString

   putStrLn ("Digite o nome do visitante")
   nome <- lerEntradaString
   
   putStrLn("\nVisitante: " ++ nome ++ "Autorizado pela Residencia: " ++ casa ++ "\n")
   
   hSetBuffering stdin NoBuffering
   hSetEcho stdin False
   action <- getKey
   putStrLn (" ")

   telaInicial 0


--   > OPCAO VERIFICAR FUNCIONARIO OU PRESTADOR DE SERVICO  <--

verificarFuncionario :: IO ()
verificarFuncionario = do
   system "clear"

   putStrLn ("Digite o nome do funcionario")
   nome <- lerEntradaString

   putStrLn ("Digite o cpf do funcionario")
   cpf <- lerEntradaString
   
   putStrLn("\nFuncionario: " ++ nome ++ " Autorizado\n")
   
   hSetBuffering stdin NoBuffering
   hSetEcho stdin False
   action <- getKey
   putStrLn (" ")

   telaInicial 0

--------------------------------------------

-- Visualizar Casas
visualizarCasas :: IO ()
visualizarCasas = do
      system "clear"
      casas <- openFile "../arquivos/Casas.csv" ReadMode
      listaCasas <- lines <$> hGetContents casas
      print (listaCasas)
      
      action <- getKey
      putStrLn (" ")
      telaInicial 0

-- Visualizar veiculos
visualizarVeiculos :: IO ()
visualizarVeiculos = do
      system "clear"
      veiculos <- openFile "../arquivos/Veiculo.csv" ReadMode
      listaVeiculo <- lines <$> hGetContents veiculos
      print (listaVeiculo)
      
      action <- getKey
      putStrLn (" ")
      telaInicial 0


-- Visualizar visitante
visualizarVisitante :: IO ()
visualizarVisitante = do
      system "clear"
      visitante <- openFile "../arquivos/VisitantesAutorizados.csv" ReadMode
      listaVisitante <- lines <$> hGetContents visitante
      print (listaVisitante)
      
      action <- getKey
      putStrLn (" ")
      telaInicial 0

-- Visualizar Funcionarios
visualizarFuncionarios :: IO ()
visualizarFuncionarios = do
      system "clear"
      funcionarios <- openFile "../arquivos/Funcionarios.csv" ReadMode
      listaFuncionarios <- lines <$> hGetContents funcionarios
      print (listaFuncionarios)
      
      action <- getKey
      putStrLn (" ")
      telaInicial 0

--------------------------------------------------------------------------------------------------
--      -->   TELA SAIR  <--

doTelaSair :: String -> IO ()
doTelaSair action    | action == "s" = return() 
                     | otherwise = telaInicial 0
    
sair :: IO ()
sair = do
   system "clear"
   putStrLn("Digite (s) para encerrar a execução ou (Outra tecla) para voltar para o menu");
   
   hSetBuffering stdin NoBuffering
   hSetEcho stdin False
   action <- getKey
   doTelaSair action


--     -->  EXECUTAR  <--
run :: IO ()
run = do
   {catch (iniciar) error;}
   where
      iniciar = do
      {
         telaInicial 0;
         return ()
      }
      error = ioError 

main :: IO ()
main = do
   run
