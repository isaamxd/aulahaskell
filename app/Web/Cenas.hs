-- | Roteiro das cutscenes (os "filmes" do jogo). Cada cena tem uma data,
--   um texto que aparece datilografado e, opcionalmente, um mapa mostrando
--   quem controla o quê naquele momento da história.
--
--   Pra mudar os textos da abertura, é só editar a lista 'abertura'.
module Web.Cenas
  ( Cena (..)
  , abertura
  , cenasFinais
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Game.Map (paisesIniciais)
import Game.Rules (controlador, dataDoTurno)
import Game.Types
import qualified Data.Map.Strict as Map

data Cena = Cena
  { cenaData   :: Text                -- ^ ex.: "Junho de 1940" (pode ser vazia)
  , cenaTexto  :: Text
  , cenaMapa   :: Maybe [(Char, Lado)] -- ^ quem controla cada país nesta cena
  , cenaClasse :: Text                -- ^ estilo extra: "titulo", "clarao", "alerta"
  }

-- ---------------------------------------------------------------------------
-- Abertura: como o mundo chegou a 1962 nesta história alternativa

abertura :: [Cena]
abertura =
  [ Cena "" "Esta é uma história alternativa." Nothing "titulo"
  , Cena "1º de setembro de 1939"
      "A Alemanha invade a Polônia. Começa a Segunda Guerra Mundial."
      (Just m1939) ""
  , Cena "Junho de 1940"
      "Em seis semanas, a França cai. A Itália entra na guerra ao lado de Berlim."
      (Just m1940) ""
  , Cena "Outubro de 1941"
      "Nesta história, Moscou cai antes do inverno. A União Soviética desmorona e recua para além dos Urais."
      (Just m1941) ""
  , Cena "6 de junho de 1944"
      "O desembarque aliado na Normandia fracassa. A Europa continental fica sob o domínio do Reich."
      (Just m1941) ""
  , Cena "16 de julho de 1945"
      "No deserto do Novo México, os Estados Unidos detonam a primeira bomba atômica."
      (Just m1945) "clarao"
  , Cena "Março de 1946"
      "Meses depois, Berlim detona a sua. Sem vencedor possível, as potências assinam o Armistício de Lisboa."
      (Just m1946) ""
  , Cena "1946 – 1961"
      "O mundo se divide em dois blocos. A Europa ocupada vive sob repressão, e o Atlântico vira a fronteira mais vigiada da Terra."
      (Just m1962) ""
  , Cena "Janeiro de 1962"
      "Uma guerra direta destruiria o planeta. A disputa agora é por cada país do mundo."
      (Just m1962) ""
  , Cena "" "GUERRA FRIA 1962" Nothing "titulo grande"
  ]
  where
    m1939 = [('D', Reich)]
    m1940 = m1939 ++ [('F', Reich), ('n', Reich), ('L', Reich)]
    m1941 = m1940 ++ [('w', Reich), ('K', Reich), ('x', Reich)]
    m1945 = m1941 ++ [('U', EUA), ('C', EUA), ('R', EUA), ('O', EUA)]
    m1946 = m1945 ++ [('j', Reich), ('m', Reich), ('3', Reich), ('2', EUA)]
    m1962 = [ (paisCodigo p, lado) | p <- paisesIniciais, Just lado <- [controlador p] ]

-- ---------------------------------------------------------------------------
-- Finais

cenasFinais :: GameState -> Fim -> [Cena]
cenasFinais st f
  | gsModo st == Resistencia = finaisResistencia st f
  | otherwise = case f of
        Vitoria lado ->
          [ Cena quando ("Os aliados de " <> ladoCapital (adversario lado) <> " mudam de lado, um a um.") mapa ""
          , Cena "" (ladoCapital lado <> " vence a Guerra Fria sem disparar um único míssil.") mapa ""
          , Cena "" (if lado == gsJogador st then "VITÓRIA" else "DERROTA") Nothing "titulo grande"
          ]
        Empate ->
          [ Cena quando "Nenhuma das potências consegue uma vantagem decisiva." mapa ""
          , Cena "" "A Guerra Fria continua, e o mundo segue à beira do abismo." mapa ""
          , Cena "" "IMPASSE" Nothing "titulo grande"
          ]
        GuerraNuclear _ ->
          [ Cena quando "Os radares disparam dos dois lados do Atlântico." mapa "alerta"
          , Cena "" "Em trinta minutos, a Guerra Fria acaba." Nothing "clarao"
          , Cena "" "Não há vencedores." Nothing "titulo grande"
          ]
  where
    quando = dataDoTurno (gsTurno st)
    mapa   = mapaAtual st

finaisResistencia :: GameState -> Fim -> [Cena]
finaisResistencia st f = case f of
  Vitoria EUA ->
    [ Cena quando "Os sinos tocam em Paris, Oslo e Roma. Os ocupantes recuam, cidade por cidade." mapa ""
    , Cena "" refugiados mapa ""
    , Cena "" "A EUROPA SE LEVANTA" Nothing "titulo grande"
    ]
  GuerraNuclear _ ->
    [ Cena quando "Batidas na porta, no meio da noite, em dezenas de cidades ao mesmo tempo." mapa "alerta"
    , Cena "" "A Gestapo desmantela a rede. Mas alguém, em algum lugar, ainda guarda um rádio." Nothing ""
    , Cena "" "A REDE CAIU" Nothing "titulo grande"
    ]
  _ ->
    [ Cena quando "A ocupação continua. A Europa ainda espera pela liberdade." mapa ""
    , Cena "" refugiados mapa ""
    , Cena "" "A LUTA CONTINUA" Nothing "titulo grande"
    ]
  where
    quando     = dataDoTurno (gsTurno st)
    mapa       = mapaAtual st
    refugiados = "Pelas rotas de fuga da Resistência, " <> T.pack (show (gsRefugiados st)) <> " pessoas foram salvas."

mapaAtual :: GameState -> Maybe [(Char, Lado)]
mapaAtual st = Just [ (c, lado) | (c, p) <- Map.toList (gsPaises st), Just lado <- [controlador p] ]
