
module Morador (getMoradorPelocpf, getCpfMorador, fromIO,
    moradorToString,
    getMoradores,
    formataParaEscrita,
    escreverArquivoMorador,
    Moradores(Moradores),
    Morador(Morador)
) where
import System.IO
import System.Directory
import Util
import System.IO.Unsafe

data Morador = Morador {
    casa :: String,    
    cpf :: String,
    nome :: String,
    dataNascimento :: String,
    sexo :: String
} deriving (Show, Read)

data Moradores = Moradores {
    moradores :: [(Int, Morador)]
} deriving Show

----------------------------MoradorGetters--------------------

getMoradores :: Moradores -> [Morador]
getMoradores (Moradores {moradores = m}) = getMoradoresFromTuple m

getMoradoresFromTuple :: [(Int, Morador)] -> [Morador]
getMoradoresFromTuple [] = []
getMoradoresFromTuple ((_,c): cs) = c : getMoradoresFromTuple cs

getCpfMorador :: Morador -> String
getCpfMorador Morador {cpf = i} = i

getNomeMorador :: Morador -> String
getNomeMorador Morador {nome = n} = n

getMoradorPelocpf :: String -> [Morador] -> Maybe Morador
getMoradorPelocpf cpf [] = Nothing
getMoradorPelocpf cpf (p:ps) = if cpf == getCpfMorador p then Just p
    else getMoradorPelocpf cpf ps

moradorToString :: Morador -> String
moradorToString Morador {casa = ca, cpf = c, nome = n, dataNascimento = i, sexo = s} = show c ++", " ++ n ++ ", " ++ show i ++ ", " ++ s

getAtributosMorador :: Morador -> String
getAtributosMorador (Morador {casa = ca, cpf = c, nome = n, dataNascimento = d, sexo = s}) = ca ++ ", " ++ c ++ ", " ++ n ++ ", " ++ d ++ ", " ++ s

moradoresToString :: [Morador] -> String
moradoresToString [] = []
moradoresToString (p:ps) = if length ps > 0 then do "["++moradorToString p ++"]," ++ moradoresToString ps
    else do "[" ++ moradorToString p ++ "]"


-----------------------------IOProduto---------------------------------

escreverArquivoMorador :: [Morador] -> IO ()
escreverArquivoMorador morador = do
    arq <- openFile "../arquivos/Moradores.csv" AppendMode
    
    print (morador)

    hPutStr arq (formataParaEscrita morador)
    hClose arq


formataParaEscrita :: [Morador] -> String
formataParaEscrita [] = []
formataParaEscrita (c:cs) = getAtributosMorador c ++ "\n" ++ formataParaEscrita cs

-- Converte IO em puro
fromIO :: IO[String] -> [String]
fromIO x = (unsafePerformIO x :: [String])

