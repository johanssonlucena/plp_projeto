module VisitanteAutorizado (getVisitantePelocpf, getCpfVisitante, fromIO,
    visitanteToString,
    getVisitantesAutorizados,
    formataParaEscrita,
    escreverArquivoVisitante,
    VisitanteAutorizados(VisitanteAutorizados),
    VisitanteAutorizado(VisitanteAutorizado)
) where

import System.IO
import System.Directory
import Util
import System.IO.Unsafe

data VisitanteAutorizado = VisitanteAutorizado {
    casa :: String,    
    cpf :: String,
    nome :: String
} deriving (Show, Read)

data VisitanteAutorizados = VisitanteAutorizados {
    visitanteAutorizados :: [(Int, VisitanteAutorizado)]
} deriving Show

----------------------------VisitanteGetters--------------------

getVisitantesAutorizados :: VisitanteAutorizados -> [VisitanteAutorizado]
getVisitantesAutorizados (VisitanteAutorizados {visitanteAutorizados = v}) = getVisitantesFromTuple v

getVisitantesFromTuple :: [(Int, VisitanteAutorizado)] -> [VisitanteAutorizado]
getVisitantesFromTuple [] = []
getVisitantesFromTuple ((_,c): cs) = c : getVisitantesFromTuple cs

getCpfVisitante :: VisitanteAutorizado -> String
getCpfVisitante VisitanteAutorizado {cpf = c} = c

getNomeVisitante :: VisitanteAutorizado -> String
getNomeVisitante VisitanteAutorizado {nome = n} = n

getVisitantePelocpf :: String -> [VisitanteAutorizado] -> Maybe VisitanteAutorizado
getVisitantePelocpf cpf [] = Nothing
getVisitantePelocpf cpf (p:ps) = if cpf == getCpfVisitante p then Just p
    else getVisitantePelocpf cpf ps

visitanteToString :: VisitanteAutorizado -> String
visitanteToString VisitanteAutorizado {casa = ca, cpf = c, nome = n} = show c ++", " ++ n

getAtributosVisitante :: VisitanteAutorizado -> String
getAtributosVisitante (VisitanteAutorizado {casa = ca, cpf = c, nome = n}) = ca ++ ", " ++ c ++ ", " ++ n

visitantesToString :: [VisitanteAutorizado] -> String
visitantesToString [] = []
visitantesToString (p:ps) = if length ps > 0 then do "["++visitanteToString p ++"]," ++ visitantesToString ps
    else do "[" ++ visitanteToString p ++ "]"


-----------------------------IOProduto---------------------------------

escreverArquivoVisitante :: [VisitanteAutorizado] -> IO ()
escreverArquivoVisitante visitanteAutorizado = do
    arq <- openFile "../arquivos/VisitantesAutorizados.csv" AppendMode
    
    print (visitanteAutorizado)

    hPutStr arq (formataParaEscrita visitanteAutorizado)
    hClose arq


formataParaEscrita :: [VisitanteAutorizado] -> String
formataParaEscrita [] = []
formataParaEscrita (c:cs) = getAtributosVisitante c ++ "\n" ++ formataParaEscrita cs


-- Converte IO em puro
fromIO :: IO[String] -> [String]
fromIO x = (unsafePerformIO x :: [String])

