
module Veiculo (getVeiculoPelaPlaca, getPlacaVeiculo, fromIO,
    veiculoToString,
    getVeiculos,
    escreverArquivoVeiculo,
    formataParaEscrita,
    Veiculos(Veiculos),
    Veiculo(Veiculo)
) where
import System.IO
import System.Directory
import Util
import System.IO.Unsafe

data Veiculo = Veiculo {
    casa :: String,
    placa :: String,    
    modelo :: String,
    cor :: String
} deriving (Show, Read)

data Veiculos = Veiculos {
    veiculos :: [(String, Veiculo)]
} deriving Show

------------------------------------------------

getVeiculos :: Veiculos -> [Veiculo]
getVeiculos (Veiculos {veiculos = v}) = getVeiculosFromTuple v

getVeiculosFromTuple :: [(String, Veiculo)] -> [Veiculo]
getVeiculosFromTuple [] = []
getVeiculosFromTuple ((_,c): cs) = c : getVeiculosFromTuple cs

getPlacaVeiculo :: Veiculo -> String
getPlacaVeiculo Veiculo {placa = p} = p

getModeloVeiculo :: Veiculo -> String
getModeloVeiculo Veiculo {modelo = m} = m

getVeiculoPelaPlaca :: String -> [Veiculo] -> Maybe Veiculo
getVeiculoPelaPlaca placa [] = Nothing
getVeiculoPelaPlaca placa (p:ps) = if placa == getPlacaVeiculo p then Just p
    else getVeiculoPelaPlaca placa ps

veiculoToString :: Veiculo -> String
veiculoToString Veiculo {placa = p, modelo = m, cor = c} = show p ++", " ++ m ++ ", " ++ show c

getAtributosVeiculo :: Veiculo -> String
getAtributosVeiculo (Veiculo {placa = p, modelo = m, cor = c}) = p ++ ", " ++ m ++ ", " ++ c

veiculosToString :: [Veiculo] -> String
veiculosToString [] = []
veiculosToString (p:ps) = if length ps > 0 then do "["++veiculoToString p ++"]," ++ veiculosToString ps
    else do "[" ++ veiculoToString p ++ "]"


-------------------------------------------------------------

escreverArquivoVeiculo :: [Veiculo] -> IO ()
escreverArquivoVeiculo veiculo = do
    arq <- openFile "../arquivos/Veiculo.csv" AppendMode
    
    print (veiculo)

    hPutStr arq (formataParaEscrita veiculo)
    hClose arq


formataParaEscrita :: [Veiculo] -> String
formataParaEscrita [] = []
formataParaEscrita (c:cs) = getAtributosVeiculo c ++ "\n" ++ formataParaEscrita cs

-- Converte IO em puro
fromIO :: IO[String] -> [String]
fromIO x = (unsafePerformIO x :: [String])

