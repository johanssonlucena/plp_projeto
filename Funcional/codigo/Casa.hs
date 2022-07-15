module Casa (
    Casa(Casa),
    Casas(Casas),
    escreverArquivoCliente,
    getClientesEmLista,
    escreverComprasCliente,
    iteraComprasClientes,
) where

import Morador(getMoradorPelocpf, getCpfMorador, fromIO, getMoradoresEmLista,
    moradorToString,
    getMoradores,
    formataParaEscrita,
    escreverArquivo,
    converteEmLista,
    getMoradoresPuros,
    Moradores(Moradores),
    Morador(Morador))

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
    casas :: [(Int, Casa)]
} deriving Show


---------------------CasaGetters----------------------------

getAtributosCasa :: Casa -> String
getAtributosCasa (Casa {responsavel = r, cpf = c, numero = n, rua = ru}) = r++", "++c ++", "++ n ", " ++ ru

getCasas :: Casas -> [Casa]
getCasas (Casas {casas = c}) = getCasasFromTuple c

getCasasFromTuple :: [(String, Casa)] -> [Casa]
getCasasFromTuple [] = []
getCasasFromTuple ((_,c): cs) = c : getCasasFromTuple cs

getMoradoresCasa :: Casa -> [Morador]
getMoradoresCasa Casa {listaMoradores = m} = m

getResponsavel :: Casa -> String
getResponsavel Casa {responsavel = r} = r

getNumeral :: Casa -> String
getNumeral Casa {cpf = c} = c


----------------------CADASTRACASA----------------------------

mapeiaNumero :: [Casa] -> [(String, Casa)]
mapeiaNumero [] = []
mapeiaNumero (c:cs)= (getNumeral c, c) : mapeiaNumero cs


adicionaMoradorCasa :: [Casa] -> String -> Morador -> Maybe [Casa]
adicionaMoradorCasa [] numeroCasa novoMorador = Nothing
adicionaMoradorCasa (Casa {numero = n, rua = r, responsavel = re, cpf = c, dataCadastro = d, listaMoradores = mor}:cs) numeroCasa novoMorador
    | n == numeroCasa = Just ([Casa n r re c d (comp ++[novoMorador])] ++ cs)
    | otherwise = adicionaMoradorCasa cs numeroCasa novoMorador

getMoradoresCasasToCsv :: [Casa] -> String
getMoradoresCasasToCsv [] = []
getMoradoresCasasToCsv (c:cs) = if length listaMoradores > 0 then getNumeral c ++ ", " ++ getMoradoresCasaToString (listaMoradores) ++ getMoradoresCasasToCsv cs
    else []
    where
        listaMoradores = getMoradoresCasa c

getMoradoresCasaToString :: [Morador] -> String
getMoradoresCasaToString [] = []





