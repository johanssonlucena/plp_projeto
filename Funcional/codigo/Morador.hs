
module Morador (getMoradorPelocpf, getCpfMorador, fromIO, getMoradoresEmLista,
    moradorToString,
    getMoradores,
    formataParaEscrita,
    escreverArquivo,
    converteEmLista,
    getMoradoresPuros,
    Moradores(Moradores),
    Morador(Morador)
) where
import System.IO
import System.Directory
import Util
import System.IO.Unsafe

data Morador = Morador {
    cpf :: String,
    nome :: String,
    idade :: Int,
    sexo :: String
} deriving (Show, Read)

data Moradores = Moradores {
    moradores :: [(Int, Morador)]
} deriving Show

----------------------------MoradorGetters--------------------

getMoradores :: Moradores -> [Morador]
getMoradores (Moradores {moradores = p}) = getMoradoresFromTuple p

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
moradorToString Morador {cpf = c, nome = n, idade = i, sexo = s} = show c ++", " ++ n ++ ", " ++ show i ++ ", " ++ s



moradoresToString :: [Morador] -> String
moradoresToString [] = []
moradoresToString (p:ps) = if length ps > 0 then do "["++moradorToString p ++"]," ++ moradoresToString ps
    else do "[" ++ moradorToString p ++ "]"
