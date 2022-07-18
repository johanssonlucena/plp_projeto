module Casa (
    Casa(Casa),
    Casas(Casas),
    escreverArquivoCasa
) where

import Morador
import System.IO
import Util
import System.IO.Unsafe


data Casa = Casa{
    numero :: String,
    rua :: String,
    responsavel :: String,
    cpf :: String,
    dataCadastro :: String,
    listaMoradores :: [Morador]
} deriving (Show, Read)


data Casas  = Casas {
    casas :: [(String, Casa)]
} deriving Show


-- -->  CasaGetters  <--

getAtributosCasa :: Casa -> String
getAtributosCasa (Casa {responsavel = r, cpf = c, numero = n, rua = ru, dataCadastro = d, listaMoradores = l}) = r ++ ", " ++ c ++ ", " ++ n ++ ", " ++ ru ++ ", " ++ d 

getCasas :: Casas -> [Casa]
getCasas (Casas {casas = c}) = getCasasFromTuple c

getCasasFromTuple :: [(String, Casa)] -> [Casa]
getCasasFromTuple [] = []
getCasasFromTuple ((_,c): cs) = c : getCasasFromTuple cs

getResponsavel :: Casa -> String
getResponsavel Casa {responsavel = r} = r

getNumeral :: Casa -> String
getNumeral Casa {cpf = c} = c


--------------------------IOCLIENTES---------------------------

escreverArquivoCasa :: [Casa] -> IO ()
escreverArquivoCasa casa = do
    arq <- openFile "../arquivos/Casas.csv" AppendMode

    print (casa)
    
    hPutStr arq (formataParaEscritaCasas casa)
    hClose arq

---------------------------UTIL------------------------------



formataParaEscritaCasas :: [Casa] -> String
formataParaEscritaCasas [] = []
formataParaEscritaCasas (c:cs) = getAtributosCasa c ++ "\n" ++ formataParaEscritaCasas cs



