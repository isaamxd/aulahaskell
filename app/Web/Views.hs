-- | HTML do jogo (com Lucid) e o mapa-múndi em SVG.
module Web.Views
  ( Notice (..)
  , page
  , gameView
  ) where

import Control.Monad (forM_, unless, when)
import qualified Data.Map.Strict as Map
import Data.Maybe (isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Game.Geografia
import Game.Map (turnoFinal)
import Game.Rules
import Game.Types
import Lucid
import Text.Printf (printf)

-- | Aviso mostrado depois de uma ação.
data Notice = Info Text | Erro Text

-- | Página completa (só é usada no primeiro carregamento).
page :: GameState -> Html ()
page st = doctypehtml_ $ do
  head_ $ do
    meta_ [charset_ "utf-8"]
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
    title_ "Guerra Fria 1962"
    link_ [rel_ "preconnect", href_ "https://fonts.googleapis.com"]
    link_ [ rel_ "stylesheet"
          , href_ "https://fonts.googleapis.com/css2?family=Oswald:wght@400;600&family=Special+Elite&display=swap" ]
    link_ [rel_ "stylesheet", href_ "/style.css"]
    script_ [src_ "https://unpkg.com/htmx.org@2.0.4"] ("" :: Text)
  body_ $ do
    header_ [class_ "topo"] $ do
      h1_ "Guerra Fria 1962"
      p_ [class_ "subtitulo"] "Estados Unidos × Reich Alemão · escrito em Haskell"
      nav_ [class_ "menu"] $ do
        a_ [href_ "/", class_ "ativo"] "Jogo"
        a_ [href_ "/historico.html"] "Histórico"
    div_ [id_ "jogo"] (gameView st Nothing Nothing)

-- | A área do jogo. O HTMX troca só este pedaço a cada clique.
gameView :: GameState -> Maybe Char -> Maybe Notice -> Html ()
gameView st sel notice
  | not (gsIniciado st) = telaEscolha
  | otherwise =
      div_ [class_ "layout"] $ do
        div_ [class_ "coluna-mapa"] $ do
          div_ [class_ "mapa"] (toHtmlRaw (mapSvg st sel))
          legenda
        aside_ [class_ "lateral"] $ do
          mapM_ fimBox (gsFim st)
          statusBox st
          tensaoBox st
          mapM_ noticeBox notice
          paisBox st sel
          when (isNothing (gsFim st)) $
            button_ [ class_ "btn btn-turno"
                    , data_ "hx-post" "/turno"
                    , data_ "hx-target" "#jogo" ] "Encerrar trimestre ▶"
          saveBox st
          logBox st
          button_ [ class_ "btn btn-secundario"
                  , data_ "hx-post" "/reiniciar"
                  , data_ "hx-target" "#jogo"
                  , data_ "hx-confirm" "Abandonar esta partida e começar outra?" ] "Nova partida"

-- ---------------------------------------------------------------------------
-- Tela inicial: escolha do lado

telaEscolha :: Html ()
telaEscolha = div_ [class_ "escolha"] $ do
  div_ [class_ "intro caixa"] $ do
    h2_ "Janeiro de 1962"
    p_ "Numa história alternativa, o Eixo venceu a Segunda Guerra na Europa. \
       \O Reich Alemão domina o continente; os Estados Unidos lideram o outro lado do Atlântico. \
       \As duas potências têm armas nucleares, e um confronto direto destruiria o planeta."
    p_ "A disputa agora é pelo resto do mundo: influência, golpes de Estado, \
       \guerras por procuração e a corrida espacial. Quem controlar mais países \
       \até 1966, sem provocar uma guerra nuclear, vence."
  div_ [class_ "cartas"] $ do
    carta EUA "Economia mais forte e aliados estáveis, mas poucos países sob controle no começo."
    carta Reich "Domina a Europa e tem aliados na Ásia, mas seus territórios ocupados são instáveis."
  where
    carta :: Lado -> Text -> Html ()
    carta lado desc =
      div_ [class_ "carta", style_ ("border-top-color:" <> ladoCor lado)] $ do
        h3_ [style_ ("color:" <> ladoCor lado)] (toHtml (ladoNome lado))
        p_ (toHtml desc)
        button_ [ class_ "btn"
                , data_ "hx-post" ("/novo/" <> ladoSlug lado)
                , data_ "hx-target" "#jogo" ] "Assumir o comando"

-- ---------------------------------------------------------------------------
-- Painel lateral

fimBox :: Fim -> Html ()
fimBox f = div_ [class_ ("caixa fim " <> classe)] $ do
  h2_ titulo
  p_ (toHtml (mensagemFim f))
  p_ [class_ "dica"] "A partida foi salva no histórico."
  where
    titulo :: Html ()
    classe :: Text
    (titulo, classe) = case f of
      Vitoria _       -> ("Fim da Guerra Fria", "fim-normal")
      Empate          -> ("Impasse", "fim-normal")
      GuerraNuclear _ -> ("Guerra nuclear", "fim-nuclear")

statusBox :: GameState -> Html ()
statusBox st = div_ [class_ "caixa"] $ do
  h2_ (toHtml (dataDoTurno (gsTurno st)))
  p_ $ do
    swatch eu
    strong_ (toHtml (ladoNome eu))
    span_ [class_ "voce"] "você"
  p_ ("Orçamento: " <> toHtml (show (orcamento eu st))
      <> " (+" <> toHtml (show (renda eu st)) <> " por trimestre)")
  placar
  div_ [class_ "espacial"] $ do
    span_ "Corrida espacial"
    pips eu
    pips ia
  when (isNothing (gsFim st) && nivelEspacial eu st < 4) $
    botaoAcao st CorridaEspacial "Investir na corrida espacial" "+1 de renda por trimestre"
  where
    eu = gsJogador st
    ia = adversario eu
    placar :: Html ()
    placar = div_ [class_ "placar"] $ do
      linhaPlacar eu
      linhaPlacar ia
      p_ [class_ "dica"] ("Vence quem chegar a " <> toHtml (show pontosVitoria)
                          <> " pontos, ou quem tiver mais em " <> toHtml (dataDoTurno turnoFinal) <> ".")
    linhaPlacar :: Lado -> Html ()
    linhaPlacar lado = div_ [class_ "linha-placar"] $ do
      span_ (swatch lado >> toHtml (ladoNome lado))
      strong_ (toHtml (show (pontos lado st)) <> " pts")
    pips :: Lado -> Html ()
    pips lado = span_ [class_ "pips", title_ (ladoNome lado)] $
      forM_ [1 .. 4 :: Int] $ \i ->
        span_ [ class_ "pip"
              , style_ (if i <= nivelEspacial lado st then "background:" <> ladoCor lado else "") ] ""

tensaoBox :: GameState -> Html ()
tensaoBox st = div_ [class_ "caixa tensao"] $ do
  h2_ "Tensão nuclear"
  div_ [class_ "medidor"] $
    forM_ [1 .. tensaoMaxima] $ \i ->
      span_ [class_ (if i <= t then "nivel aceso nivel-" <> tshow i else "nivel")] ""
  p_ [class_ "dica"] (toHtml (descricao t))
  where
    t = gsTensao st
    descricao :: Int -> Text
    descricao n = case n of
      0 -> "Calma. A diplomacia funciona."
      1 -> "Baixa."
      2 -> "Moderada. Golpes e guerras aumentam a tensão."
      3 -> "Alta. Cuidado com o próximo passo."
      4 -> "CRÍTICA. Mais uma crise e o mundo acaba."
      _ -> "Guerra nuclear."

noticeBox :: Notice -> Html ()
noticeBox (Info t) = div_ [class_ "aviso aviso-ok"] (toHtml t)
noticeBox (Erro t) = div_ [class_ "aviso aviso-erro"] (toHtml t)

paisBox :: GameState -> Maybe Char -> Html ()
paisBox st sel = case sel >>= (`Map.lookup` gsPaises st) of
  Nothing ->
    div_ [class_ "caixa"] $
      p_ [class_ "dica"] "Clique num país do mapa para ver os detalhes e agir."
  Just p -> div_ [class_ "caixa"] $ do
    h2_ (toHtml (paisNome p))
    p_ [class_ "dica"] $ do
      toHtml (regiaoNome (paisRegiao p))
      when (paisEstrategico p) " · ★ estratégico (vale 2 pontos)"
    case paisNucleo p of
      Just lado ->
        p_ ("Território de " <> toHtml (ladoNome lado) <> ". Não pode ser disputado.")
      Nothing -> do
        barraAlinhamento p
        p_ ("Controle: " <> toHtml (textoControle p))
        p_ ("Estabilidade: " <> toHtml (T.replicate (paisEstabilidade p) "■" <> T.replicate (4 - paisEstabilidade p) "□"))
        when (isNothing (gsFim st)) $
          div_ [class_ "acoes-pais"] $ do
            let c = paisCodigo p
            botaoAcao st (Influenciar c) "Influenciar" "+15 a seu favor"
            unless (controlador p == Just (gsJogador st)) $
              botaoAcao st (Golpe c) "Golpe de Estado"
                (chanceTexto (Golpe c) <> " · sucesso +40, fracasso −10 · tensão +" <> tshow (tensaoDaAcao (Golpe c) st))
            when (isNothing (controlador p)) $
              botaoAcao st (GuerraProcuracao c) "Guerra por procuração"
                (chanceTexto (GuerraProcuracao c) <> " · vitória dá o controle · tensão +2")
  where
    chanceTexto :: Acao -> Text
    chanceTexto acao = maybe "" (\pct -> tshow pct <> "% de chance") (chanceSucesso acao st)

textoControle :: Pais -> Text
textoControle p = case controlador p of
  Just lado -> ladoNome lado
  Nothing
    | paisAlinhamento p > 0 -> "em disputa, inclinado aos EUA"
    | paisAlinhamento p < 0 -> "em disputa, inclinado ao Reich"
    | otherwise             -> "em disputa, neutro"

-- | Barra de -100 (Reich, à esquerda) a +100 (EUA, à direita).
barraAlinhamento :: Pais -> Html ()
barraAlinhamento p = div_ [class_ "alinhamento"] $ do
  div_ [class_ "barra"] $ do
    span_ [class_ "zona zona-reich"] ""
    span_ [class_ "zona zona-eua"] ""
    span_ [class_ "marcador", style_ ("left:" <> tshow pct <> "%")] ""
  div_ [class_ "rotulos"] $ do
    span_ "Reich"
    span_ (toHtml (tshow (paisAlinhamento p)))
    span_ "EUA"
  where
    pct = (paisAlinhamento p + 100) `div` 2

botaoAcao :: GameState -> Acao -> Text -> Text -> Html ()
botaoAcao st acao rotulo detalhe =
  button_ ([ class_ "btn btn-acao"
           , data_ "hx-post" (urlAcao acao)
           , data_ "hx-target" "#jogo" ] ++ [ disabled_ "disabled" | not podePagar ]) $ do
    span_ [class_ "acao-nome"] (toHtml rotulo)
    span_ [class_ "acao-custo"] (toHtml (tshow preco))
    span_ [class_ "acao-detalhe"] (toHtml detalhe)
  where
    preco     = custo acao st
    podePagar = orcamento (gsJogador st) st >= preco

urlAcao :: Acao -> Text
urlAcao (Influenciar c)      = "/influenciar/" <> T.singleton c
urlAcao (Golpe c)            = "/golpe/" <> T.singleton c
urlAcao (GuerraProcuracao c) = "/guerra/" <> T.singleton c
urlAcao CorridaEspacial      = "/espacial"

-- | Formulário que salva a partida no banco (POST /salvar).
saveBox :: GameState -> Html ()
saveBox st = div_ [class_ "caixa"] $ do
  h2_ "Salvar campanha"
  form_ [data_ "hx-post" "/salvar", data_ "hx-target" "#jogo", class_ "form-salvar"] $ do
    input_ [ type_ "text", name_ "nomeComandante", class_ "campo", maxlength_ "40"
           , placeholder_ "Nome do comandante", value_ (gsComandante st) ]
    button_ [type_ "submit", class_ "btn"] "Salvar"
  case gsPartidaId st of
    Just pid -> p_ [class_ "dica"] ("Salva como partida #" <> toHtml (show pid))
    Nothing  -> p_ [class_ "dica"] "Ainda não salva."
  p_ (a_ [href_ "/historico.html", class_ "link"] "Ver histórico de campanhas →")

logBox :: GameState -> Html ()
logBox st = div_ [class_ "caixa log"] $ do
  h2_ "Relatórios"
  ul_ (mapM_ (li_ . toHtml) (take 10 (gsLog st)))

legenda :: Html ()
legenda = div_ [class_ "legenda"] $ do
  item (corHex (rgbNucleo EUA)) "EUA"
  item (corHex (rgbLado EUA)) "aliado dos EUA"
  item (corHex rgbNeutro) "em disputa"
  item (corHex (rgbLado Reich)) "aliado do Reich"
  item (corHex (rgbNucleo Reich)) "Reich"
  where
    item :: Text -> Html () -> Html ()
    item cor txt = span_ [class_ "item-legenda"] $ do
      span_ [class_ "bandeira", style_ ("background:" <> cor)] ""
      txt

swatch :: Lado -> Html ()
swatch lado = span_ [class_ "bandeira", style_ ("background:" <> ladoCor lado)] ""

-- ---------------------------------------------------------------------------
-- Cores

type RGB = (Int, Int, Int)

rgbLado :: Lado -> RGB
rgbLado EUA   = (61, 106, 158)
rgbLado Reich = (140, 47, 47)

rgbNucleo :: Lado -> RGB
rgbNucleo EUA   = (44, 82, 130)
rgbNucleo Reich = (110, 31, 31)

rgbNeutro :: RGB
rgbNeutro = (138, 132, 112)

misturar :: Double -> RGB -> RGB -> RGB
misturar t (r1, g1, b1) (r2, g2, b2) = (f r1 r2, f g1 g2, f b1 b2)
  where
    f a b = round (fromIntegral a + t * fromIntegral (b - a) :: Double)

corHex :: RGB -> Text
corHex (r, g, b) = T.pack (printf "#%02x%02x%02x" r g b)

-- | Cor do país: forte se controlado, tons intermediários se em disputa.
corDoPais :: Pais -> Text
corDoPais p = corHex $ case (paisNucleo p, controlador p) of
  (Just lado, _) -> rgbNucleo lado
  (_, Just lado) -> rgbLado lado
  _
    | a > 0     -> misturar t rgbNeutro (rgbLado EUA)
    | a < 0     -> misturar t rgbNeutro (rgbLado Reich)
    | otherwise -> rgbNeutro
  where
    a = paisAlinhamento p
    t = 0.65 * fromIntegral (abs a) / fromIntegral limiteControle

-- ---------------------------------------------------------------------------
-- Mapa SVG (gerado como texto: simples e sem dependência extra)
-- Ordem das camadas: grade, terras neutras, países, fronteiras, seleção, siglas.

mapSvg :: GameState -> Maybe Char -> Text
mapSvg st sel = T.concat $
  [ "<svg xmlns=\"http://www.w3.org/2000/svg\" class=\"mapa-svg\" viewBox=\"0 0 "
  , num larguraMapa, " ", num alturaMapa, "\">"
  , caminho "grade-geo" gradeGeografica
  , caminho "terra-neutra" terrasNeutras ]
  ++ map territorio formas
  ++ [ caminho "fronteira-fraca" fronteirasFracas
     , caminho "fronteira" fronteirasFortes ]
  ++ [ caminho "fronteira-sel" (formaBorda f) | f <- formas, Just (formaCodigo f) == sel ]
  ++ map rotulo formas
  ++ ["</svg>"]
  where
    territorio f = case Map.lookup (formaCodigo f) (gsPaises st) of
      Nothing -> ""
      Just p ->
        let cor = corDoPais p
        in T.concat
             [ "<path class=\"pais\" d=\"", formaD f
             , "\" fill=\"", cor, "\" stroke=\"", cor
             , "\" data-hx-get=\"/pais/", T.singleton (formaCodigo f)
             , "\" data-hx-target=\"#jogo\"><title>", paisNome p, "</title></path>" ]

    caminho cls d = T.concat ["<path class=\"", cls, "\" d=\"", d, "\"/>"]

    rotulo f = case Map.lookup (formaCodigo f) (gsPaises st) of
      Nothing -> ""
      Just p ->
        let cls = if paisEstrategico p then "sigla estrategico" else "sigla"
        in T.concat
             [ "<text class=\"", cls, "\" x=\"", num (formaX f), "\" y=\"", num (formaY f + 3)
             , "\">", paisSigla p, "</text>" ]

-- ---------------------------------------------------------------------------
-- Utilitários

tshow :: Show a => a -> Text
tshow = T.pack . show

-- | Número com uma casa decimal, pra deixar o SVG enxuto.
num :: Double -> Text
num d = tshow (fromIntegral (round (d * 10) :: Int) / 10 :: Double)
