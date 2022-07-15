import System.IO 
import System.IO.Unsafe
import Control.Exception
import System.IO.Error 
import System.Process
import Control.Monad (when)
import Text.Printf
import System.IO.Unsafe

--      --> IMPORTS DAS ENTIDADES <--

--import Morador
--import Casa


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
   putStrLn ("Bem vindo, Sr(a). Administrador! \n\n|| Tecle (Seta Direita) para escolher qual opcão acessar e (Seta Esquerda) para voltar à tela anterior. ||")
   showSimpleScreen opcoesTelaAdm cursor 0
   hSetBuffering stdin NoBuffering
   hSetEcho stdin False
   action <- getKey
   doOpcoesAdm cursor action



--   > OPCAO CADASTRAR CASA  <--

listaMoradoresInicial:: [String]
listaMoradoresInicial = []

cadastroCasaTela :: IO ()
cadastroCasaTela = do
   system "clear"

   putStrLn ("Digite o numeral da Casa:")
   numero <- lerEntradaString

   putStrLn ("\nDigite qual rua está localizado")
   rua <- lerEntradaString

   putStrLn ("\nDigite o Nome do responsável da Casa")
   responsavel <- lerEntradaString

   putStrLn ("\nDigite o Cpf do responsável da Casa")
   cpf <- lerEntradaString

   putStrLn ("\nDigite a data do cadastro (formato DD/MM/AAAA):")
   dataCadastro <- lerEntradaString

   putStrLn("\nCasa Cadastrada com sucesso!\n")
   
   hSetBuffering stdin NoBuffering
   hSetEcho stdin False

  -- let casa = Casa numero rua responsavel cpf dataCadastro listaMoradoresInicial
  -- let listaCasa = [casa]
  -- let adicionarCasa = listaCasa
   --escreverArquivoCasa adicionarCasa
   
   action <- getKey
   telaInicial 0



--   > OPCAO CADASTRAR MORADOR  <--

cadastroMoradorTela :: IO ()
cadastroMoradorTela = do
   system "clear"

   putStrLn ("Digite a unidade residencial:")
   casa <- lerEntradaString

   putStrLn ("Digite o cpf do morador:")
   cpf <- lerEntradaString

   putStrLn ("\nDigite o nome do morador:")
   nome <- lerEntradaString

   putStrLn ("\nDigite a idade do morador:")
   idade <- lerEntradaInt

   putStrLn ("\nDigite o sexo do morador:")
   sexo <- lerEntradaString

  -- let morador = Morador casa cpf nome idade sexo 
  -- let listaMoradores = [produto]
  -- let adicionarMorador = listaMoradores
  -- escreverArquivo adicionarMorador
   
   putStrLn("\nMorador cadastrado com sucesso!\n")
   
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

   putStrLn ("\nDigite a cor:")
   cor <- lerEntradaString

   putStrLn ("\nDigite o modelo:")
   modelo <- lerEntradaInt

  -- let veiculo = Veiculo casa placa cor modelo 
  -- let listaVeiculo = [veiculo]
  -- let adicionarVeiculo = listaveiculos
  -- escreverArquivo adicionarVeiculo
   
   putStrLn("\nVeiculo cadastrado com sucesso!\n")
   
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

   putStrLn ("\nDigite o nome do visitante:")
   nome <- lerEntradaString


  -- let visitante = Visitante casa cpf nome
  -- let listaVisitantes = [visitante]
  -- let adicionarVisitante = listaVisitantes
  -- escreverArquivo adicionarVisitante
   
   putStrLn("\nVisitante cadastrado com sucesso!\n")
   
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

   putStrLn ("\nDigite a funcao:")
   funcao <- lerEntradaString


  -- let funcionario = Funcionario nome cpf funcao
  -- let listaVFuncionarios = [funcionario]
  -- let adicionarFuncionario = listaFuncionarios
  -- escreverArquivo adicionarFuncionario
   
   putStrLn("\nVisitante cadastrado com sucesso!\n")
   
   hSetBuffering stdin NoBuffering
   hSetEcho stdin False
   action <- getKey
   putStrLn (" ")

   telaInicial 0

---------------------------------------------------------------------------------------------------------------------------
--      -->  ACESSO PORTARIA  <--

opcoesTelaPortaria :: [String]
opcoesTelaPortaria = ["Listar Detalhes de Casa", "verificar Veiculo", "Verificar Visitante", "Verificar FuncionarioseOutros"]

mudarTelaOpcoesPortaria :: Integer -> IO ()
mudarTelaOpcoesPortaria cursor
   | cursor == 0 = listarDetalhesCasa
   | cursor == 1 = verificarVeiculo
   | cursor == 2 = verificarVisitante
   | cursor == 3 = verificarFuncionario

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


------------------------------------------------------------------------------------------------------------------
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
