module Funcionario (getFuncionarioPelocpf, getCpfFuncionario, fromIO,
    funcionarioToString,
    getFuncionarios,
    formataParaEscrita,
    escreverArquivoFuncionario,
    Funcionarios(Funcionarios),
    Funcionario(Funcionario)
) where

import System.IO
import System.Directory
import Util
import System.IO.Unsafe

data Funcionario = Funcionario {
    nome :: String,    
    cpf :: String,
    funcao :: String
} deriving (Show, Read)

data Funcionarios = Funcionarios {
    funcionario :: [(Int, Funcionario)]
} deriving Show

----------------------------FuncionarioGetters--------------------

getFuncionarios :: Funcionarios -> [Funcionario]
getFuncionarios (Funcionarios {funcionario = v}) = getFuncionariosFromTuple v

getFuncionariosFromTuple :: [(Int, Funcionario)] -> [Funcionario]
getFuncionariosFromTuple [] = []
getFuncionariosFromTuple ((_,c): cs) = c : getFuncionariosFromTuple cs

getCpfFuncionario :: Funcionario -> String
getCpfFuncionario Funcionario {cpf = c} = c

getNomeFuncionario :: Funcionario -> String
getNomeFuncionario Funcionario {nome = n} = n

getFuncionarioPelocpf :: String -> [Funcionario] -> Maybe Funcionario
getFuncionarioPelocpf cpf [] = Nothing
getFuncionarioPelocpf cpf (p:ps) = if cpf == getCpfFuncionario p then Just p
    else getFuncionarioPelocpf cpf ps

funcionarioToString :: Funcionario -> String
funcionarioToString Funcionario {nome = n, cpf = c, funcao = f} = show c ++", " ++ n ++ ", " ++ f

getAtributosFuncionario :: Funcionario -> String
getAtributosFuncionario (Funcionario {nome = n, cpf = c, funcao = f}) = n ++ ", " ++ c ++ ", " ++ f

funcionariosToString :: [Funcionario] -> String
funcionariosToString [] = []
funcionariosToString (p:ps) = if length ps > 0 then do "["++funcionarioToString p ++"]," ++ funcionariosToString ps
    else do "[" ++ funcionarioToString p ++ "]"


-----------------------------IOProduto---------------------------------

escreverArquivoFuncionario :: [Funcionario] -> IO ()
escreverArquivoFuncionario funcionario = do
    arq <- openFile "../arquivos/Funcionarios.csv" AppendMode
    
    print (funcionario)

    hPutStr arq (formataParaEscrita funcionario)
    hClose arq


formataParaEscrita :: [Funcionario] -> String
formataParaEscrita [] = []
formataParaEscrita (c:cs) = getAtributosFuncionario c ++ "\n" ++ formataParaEscrita cs

----------

-- Converte IO em puro
fromIO :: IO[String] -> [String]
fromIO x = (unsafePerformIO x :: [String])

