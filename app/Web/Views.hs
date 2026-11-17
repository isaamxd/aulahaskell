-- | HTML do jogo (com Lucid), o mapa-múndi em SVG, o jornal das crises e
--   as cutscenes. As animações, os sons, o zoom e a janela do país ficam em
--   static/efeitos.js e static/mapa.js; aqui só deixamos "sinais" no HTML
--   dizendo o que tocar e mostrar.
--
--   Visual: dossiê escuro (pastas, memorandos, telegramas) com o mapa no
--   estilo Hearts of Iron IV (relevo, brilho nas bordas e nomes grandes).
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
import Game.Crises
import Game.Geografia
import Game.Map (turnoFinal)
import Game.Rules
import Game.Types
import Lucid
import Text.Printf (printf)
import Web.Bandeiras
import Web.Cenas

-- | Muda a cada versão: força o navegador a baixar o CSS e o JavaScript novos
--   em vez de usar os antigos guardados em cache.
versaoArquivos :: Text
versaoArquivos = "6"

-- | Aviso mostrado depois de uma ação.
data Notice = Info Text | Erro Text

-- ---------------------------------------------------------------------------
-- Página

-- | Página completa (só é usada no primeiro carregamento).
page :: GameState -> Html ()
page st = doctypehtml_ $ do
  head_ $ do
    meta_ [charset_ "utf-8"]
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
    title_ "Guerra Fria 1962"
    link_ [rel_ "preconnect", href_ "https://fonts.googleapis.com"]
    link_ [ rel_ "stylesheet"
          , href_ "https://fonts.googleapis.com/css2?family=EB+Garamond:ital,wght@0,500;0,600;0,700;0,800;1,500;1,600&family=IBM+Plex+Mono:wght@500;600;700&family=Oswald:wght@500;600;700&family=Special+Elite&display=swap" ]
    link_ [rel_ "stylesheet", href_ ("/style.css?v=" <> versaoArquivos)]
    script_ [src_ "https://unpkg.com/htmx.org@2.0.4"] ("" :: Text)
    script_ [src_ ("/efeitos.js?v=" <> versaoArquivos), defer_ "defer"] ("" :: Text)
    script_ [src_ ("/mapa.js?v=" <> versaoArquivos), defer_ "defer"] ("" :: Text)
  body_ $ do
    cabecalho
    div_ [id_ "jogo"] (gameView st Nothing Nothing)
    janelaAjuda
    div_ [id_ "dica"] ""
    p_ [class_ "creditos typed"] $ do
      "Música: “Legionnaire” by Scott Buckley – released under CC-BY 4.0. "
      a_ [href_ "https://www.scottbuckley.com.au", target_ "_blank"] "www.scottbuckley.com.au"

-- | Cabeçalho: a pasta com o título e o menu (histórico, como jogar, som).
cabecalho :: Html ()
cabecalho = header_ [class_ "topo"] $ do
  a_ [class_ "pasta", href_ "/"] $ do
    p_ [class_ "ref typed"] "PASTA Nº 62/114 · DEP. DE ESTADO"
    h1_ $ do
      "Guerra Fria, "
      i_ "1962"
  nav_ [class_ "menu typed"] $ do
    a_ [href_ "/historico.html"] "Histórico"
    button_ [type_ "button", data_ "abrir" "ajuda"] "Como jogar"
    button_ [id_ "botao-som", type_ "button", title_ "Ligar ou desligar o som"] "Som: ligado"

-- | Janela "Como jogar" (fica escondida até clicar no menu).
janelaAjuda :: Html ()
janelaAjuda = div_ [class_ "modal", id_ "m-ajuda"] $ div_ [class_ "folha papel"] $ do
  button_ [type_ "button", class_ "fechar-m", title_ "Fechar"] "×"
  p_ [class_ "nome-m"] "Instruções de campo"
  p_ [class_ "ed typed"] "LEIA ANTES DE ASSUMIR O COMANDO"
  comoJogar

comoJogar :: Html ()
comoJogar = ol_ [class_ "regras"] $ do
  li_ $ do
    b_ "Objetivo: "
    "chegar a 36 pontos, ou ter mais pontos que o Reich em outubro de 1966. \
    \Na Resistência, libertar 6 países europeus até 1966."
  li_ $ do
    b_ "Clique num país "
    "para abrir a ficha dele e escolher uma ação. Cada país vai de −100 (Reich) a +100 (EUA); \
    \a partir de ±50 ele é controlado e dá pontos. Os estratégicos valem 2."
  li_ $ do
    b_ "Influenciar "
    "é barato e seguro. "
    b_ "Golpe "
    "e "
    b_ "guerra por procuração "
    "dependem do dado e aumentam o alerta nuclear. A corrida espacial aumenta a renda."
  li_ $ do
    b_ "Encerre o trimestre "
    "para receber orçamento. O Reich joga e chega um jornal com uma crise para você decidir."
  li_ $ do
    b_ "Alerta nuclear: "
    "se chegar a 5, é guerra nuclear e quem causou perde. Na Resistência, vira o risco de a Gestapo descobrir a rede."
  li_ $ do
    b_ "Na Resistência "
    "as ações viram rádio clandestino, sabotagem, levante e rotas de fuga, e só valem na Europa."
  li_ $ do
    b_ "Mapa: "
    "roda do mouse ou botões para dar zoom; arraste para mover. As bandeiras mudam quando um país troca de lado."

-- ---------------------------------------------------------------------------
-- Área do jogo

-- | A área do jogo. O HTMX troca só este pedaço a cada clique.
gameView :: GameState -> Maybe Char -> Maybe Notice -> Html ()
gameView st sel notice
  | not (gsIniciado st) = telaEscolha
  | otherwise =
      div_ [class_ classeLayout] $ do
        sinais st
        mapM_ (cutscene "final" (gsCenaFinal st) . cenasFinais st) (gsFim st)
        when (gsJornal st) (jornal st)
        barraTopo st
        div_ [class_ "area"] $ do
          section_ [class_ "coluna-mapa"] $ div_ [class_ "papel prancha"] $ do
            div_ [class_ "cab"] $ do
              b_ (if resistencia then "Prancha I · A Europa ocupada" else "Prancha I · Situação mundial")
              controlesZoom
            div_ [class_ "moldura", id_ "moldura"] $ do
              div_ [class_ "mapa"] (toHtmlRaw (mapSvg st sel))
              mapM_ (janelaPais st) (sel >>= (`Map.lookup` gsPaises st))
            div_ [class_ "rodape typed"] $ do
              legenda st
              span_ "Clique num país · arraste para mover · roda do mouse para zoom"
          aside_ [class_ "lateral"] $ do
            mapM_ (fimBox st) (gsFim st)
            memoBox st
            saveBox st
            logBox st
            button_ [ class_ "btn btn-secundario"
                    , data_ "hx-post" "/reiniciar"
                    , data_ "hx-target" "#jogo"
                    , data_ "hx-confirm" "Abandonar esta partida e começar outra?" ] "Nova partida"
        mapM_ (janelaEnigma st) (gsEnigma st)
        mapM_ noticeBox notice
        mapM_ dadoBox (gsDado st)
  where
    resistencia :: Bool
    resistencia = gsModo st == Resistencia
    classeLayout :: Text
    classeLayout = T.unwords $
      [ "layout" ]
      ++ [ "nuclear" | Just (GuerraNuclear _) <- [gsFim st] ]
      ++ [ "modo-resistencia" | resistencia ]

-- | Informações escondidas que o efeitos.js e o mapa.js leem.
sinais :: GameState -> Html ()
sinais st =
  div_ [ id_ "sinais", style_ "display:none"
       , data_ "sons" (T.unwords sons)
       , data_ "modo" (if gsModo st == Resistencia then "resistencia" else "guerra-fria")
       , data_ "tensao" (tshow (gsTensao st)) ] ""
  where
    sons :: [Text]
    sons = gsSons st ++ [ "alarme" | gsTensao st == tensaoMaxima - 1, not (null (gsSons st)) ]

-- | Barra de recursos no topo, como no HOI4: data, orçamento, pontos, alerta.
barraTopo :: GameState -> Html ()
barraTopo st = div_ [class_ "recursos papel typed"] $ do
  recurso "data" "Data" (toHtml (dataDoTurno (gsTurno st)))
  recurso "" (if resistencia then "Recursos" else "Orçamento") $ do
    toHtml (tshow (orcamento eu st))
    em_ ("+" <> toHtml (tshow (renda eu st)) <> "/trim.")
  if resistencia
    then recurso "" "Libertados" $ do
      toHtml (tshow (libertados st))
      em_ ("de " <> toHtml (tshow metaLibertacao))
    else recurso "" "Pontos" $ do
      toHtml (tshow (pontos eu st))
      em_ ("de " <> toHtml (tshow pontosVitoria))
  div_ [class_ ("recurso alerta" <> if t >= 4 then " critico" else "")] $ do
    small_ (if resistencia then "Risco de descoberta" else "Alerta nuclear")
    div_ [class_ "niveis"] $
      forM_ [1 .. tensaoMaxima] $ \i ->
        i_ [class_ (if i <= t then "on" else "")] ""
  when (isNothing (gsFim st)) $
    button_ [ class_ "btn-turno"
            , data_ "hx-post" "/turno"
            , data_ "hx-target" "#jogo" ] "Encerrar trimestre"
  where
    eu = gsJogador st
    t = gsTensao st
    resistencia :: Bool
    resistencia = gsModo st == Resistencia
    recurso :: Text -> Text -> Html () -> Html ()
    recurso classe rotulo valor = div_ [class_ ("recurso " <> classe)] $ do
      small_ (toHtml rotulo)
      b_ valor

-- ---------------------------------------------------------------------------
-- Tela inicial: abertura e escolha do modo

telaEscolha :: Html ()
telaEscolha = div_ [class_ "escolha"] $ do
  cutscene "intro" False abertura
  div_ [class_ "papel memo intro"] $ do
    div_ [class_ "clipe"] ""
    h3_ "Memorando · Janeiro de 1962"
    hr_ []
    p_ "Numa história alternativa, o Eixo venceu a Segunda Guerra na Europa. \
       \O Reich Alemão domina o continente; os Estados Unidos lideram o outro lado do Atlântico. \
       \As duas potências têm armas nucleares, e um confronto direto destruiria o planeta."
    p_ "Escolha como lutar: comandando os Estados Unidos na disputa pelo mundo, \
       \ou liderando a Resistência que tenta libertar a Europa ocupada por dentro."
    div_ [class_ "intro-botoes"] $ do
      button_ [id_ "ver-intro", class_ "btn btn-secundario", type_ "button"] "▶ Assistir à introdução"
      button_ [class_ "btn btn-secundario", type_ "button", data_ "abrir" "ajuda"] "Como jogar"
  div_ [class_ "cartas"] $ do
    carta "eua" "Estados Unidos" "Guerra Fria" bandeiraEUA
      "Dispute cada país do mundo com Berlim, com influência, golpes, \
      \guerras por procuração e a corrida espacial, sem provocar uma guerra nuclear."
    carta "resistencia" "Resistência Europeia" "Boletim clandestino" bandeiraResistencia
      "Liberte a Europa por dentro: rádio clandestino, sabotagem, levantes e rotas de fuga \
      \para salvar refugiados. Se a Gestapo descobrir a rede, tudo acaba."
  where
    carta :: Text -> Text -> Text -> Text -> Text -> Html ()
    carta slug titulo sub bandeira desc =
      div_ [class_ ("papel carta carta-" <> slug)] $ do
        div_ [class_ "carta-topo"] $ do
          toHtmlRaw bandeira
          div_ $ do
            p_ [class_ "carta-sub typed"] (toHtml sub)
            h3_ (toHtml titulo)
        p_ (toHtml desc)
        button_ [ class_ "btn btn-comando"
                , data_ "hx-post" ("/novo/" <> slug)
                , data_ "hx-target" "#jogo" ] "Assumir o comando"
    bandeiraEUA :: Text
    bandeiraEUA = maybe "" (bandeiraDoPais 48) (Map.lookup 'U' (gsPaises initialStateDaTela))
    bandeiraResistencia :: Text
    bandeiraResistencia =
      "<svg class=\"bandeira\" viewBox=\"0 0 30 20\" width=\"48\" height=\"32\" aria-hidden=\"true\">\
      \<rect width=\"30\" height=\"20\" fill=\"#1f3b28\"/>\
      \<path d=\"M15 3.5L16.6 8.4H21.8L17.6 11.4L19.2 16.3L15 13.3L10.8 16.3L12.4 11.4L8.2 8.4H13.4Z\" fill=\"#e6dcc3\"/>\
      \<rect x=\".25\" y=\".25\" width=\"29.5\" height=\"19.5\" fill=\"none\" stroke=\"#0006\" stroke-width=\".5\"/></svg>"

-- | Estado inicial, só para desenhar a bandeira dos EUA na tela de escolha.
initialStateDaTela :: GameState
initialStateDaTela = iniciarPartida GuerraFria 0 ""

-- ---------------------------------------------------------------------------
-- Jornal do trimestre e crises

jornal :: GameState -> Html ()
jornal st = div_ [class_ "jornal-fundo"] $ div_ [class_ "jornal papel"] $ do
  div_ [class_ "jornal-cabecalho"] $ do
    p_ [class_ "jornal-nome"] (if gsModo st == Resistencia then "Boletim Clandestino" else "Gazeta Internacional")
    p_ [class_ "jornal-data typed"] (toHtml (dataDoTurno (gsTurno st)) <> " · Edição extra")
  case gsCrise st >>= buscarCrise of
    Just crise -> div_ [class_ "jornal-crise"] $ do
      h2_ [class_ "manchete"] (toHtml (criseManchete crise))
      p_ [class_ "jornal-texto"] (toHtml (criseTexto crise))
      p_ [class_ "jornal-pergunta typed"] "O que você faz?"
      div_ [class_ "jornal-opcoes"] $ do
        botaoOpcao st "a" (criseA crise)
        botaoOpcao st "b" (criseB crise)
    Nothing -> h2_ [class_ "manchete"] "Um trimestre sem grandes crises"
  unless (null (gsResumo st)) $ div_ [class_ "jornal-resumo"] $ do
    h3_ "Enquanto isso, no mundo"
    ul_ (mapM_ (li_ . toHtml . semData) (gsResumo st))
  when (isNothing (gsCrise st)) $
    button_ [ class_ "btn btn-turno-jornal"
            , data_ "hx-post" "/jornal/fechar"
            , data_ "hx-target" "#jogo" ] "Continuar ▶"

botaoOpcao :: GameState -> Text -> Opcao -> Html ()
botaoOpcao st letra op =
  button_ ([ class_ (if perigo then "btn-opcao perigo" else "btn-opcao")
           , data_ "hx-post" ("/crise/" <> letra)
           , data_ "hx-target" "#jogo" ] ++ [ data_ "hx-confirm" (avisoFatal st) | perigo ]) $ do
    span_ [class_ "opcao-rotulo"] (toHtml (opcaoRotulo op))
    span_ [class_ "opcao-efeitos typed"] (toHtml (T.intercalate " · " (map (descreverEfeito st) (opcaoEfeitos op))))
    when perigo $ span_ [class_ "opcao-aviso typed"] (toHtml (resumoFatal st))
  where
    perigo :: Bool
    perigo = gsTensao st + tensaoDaOpcao op >= tensaoMaxima

-- | Pergunta feita antes de uma ação que acaba a partida (tensão no máximo).
avisoFatal :: GameState -> Text
avisoFatal st
  | gsModo st == Resistencia =
      "⚠️ Isso vai levar o risco ao máximo: a Gestapo vai descobrir a rede. \
      \Você perderá a partida. Continuar mesmo assim?"
  | otherwise =
      "⚠️ Isso vai levar a tensão ao máximo e provocar uma GUERRA NUCLEAR. \
      \Você perderá a partida. Continuar mesmo assim?"

resumoFatal :: GameState -> Text
resumoFatal st
  | gsModo st == Resistencia = "⚠️ a rede será descoberta"
  | otherwise                = "⚠️ provoca guerra nuclear"

descreverEfeito :: GameState -> Efeito -> Text
descreverEfeito st efeito = case efeito of
  Influencia c v   -> nome c <> " " <> sinal v
  Estabilidade c n -> nome c <> " (estabilidade " <> sinal n <> ")"
  Tensao n         -> (if resistencia then "risco " else "alerta ") <> sinal n
  Orcamento n      -> (if resistencia then "recursos " else "orçamento ") <> sinal n
  Espacial         -> if resistencia then "nova rota de fuga" else "corrida espacial +1"
  Refugiados n     -> tshow n <> " refugiados salvos"
  where
    resistencia :: Bool
    resistencia = gsModo st == Resistencia
    nome :: Char -> Text
    nome c = maybe (T.singleton c) nomeExibido (Map.lookup c (gsPaises st))
    sinal :: Int -> Text
    sinal v = if v >= 0 then "+" <> tshow v else "−" <> tshow (abs v)

-- | Tira a data do começo do relatório ("Abril de 1962: ..." -> "...").
semData :: Text -> Text
semData t = case T.breakOn ": " t of
  (_, resto) | not (T.null resto) -> T.drop 2 resto
  _                               -> t

-- ---------------------------------------------------------------------------
-- Cutscenes (o efeitos.js toca as cenas em sequência)

-- | Com automatico = True, a cutscene começa sozinha assim que aparece.
cutscene :: Text -> Bool -> [Cena] -> Html ()
cutscene nome automatico cenas =
  div_ ([class_ "cutscene", data_ "cutscene" nome] ++ [ data_ "auto" "1" | automatico ]) $ do
  div_ [class_ "faixa faixa-topo"] ""
  div_ [class_ "cena-mapa"] (toHtmlRaw mapaBase)
  forM_ cenas $ \c ->
    div_ ([class_ ("cena " <> cenaClasse c)] ++ [ data_ "mapa" (codificar m) | Just m <- [cenaMapa c] ]) $ do
      unless (T.null (cenaData c)) $ p_ [class_ "cena-data"] (toHtml (cenaData c))
      p_ [class_ "cena-texto"] (toHtml (cenaTexto c))
  div_ [class_ "faixa faixa-baixo"] ""
  button_ [class_ "pular", type_ "button"] "Pular ▶"
  where
    codificar :: [(Char, Lado)] -> Text
    codificar m = T.intercalate "," [ T.singleton c <> ":" <> letraLado l | (c, l) <- m ]
    letraLado :: Lado -> Text
    letraLado EUA   = "E"
    letraLado Reich = "R"

-- | Mapa sem cores e sem cliques: as cenas pintam os países pelo JavaScript.
mapaBase :: Text
mapaBase = T.concat $
  [ "<svg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 "
  , num larguraMapa, " ", num alturaMapa, "\">"
  , "<path class=\"terra-neutra\" d=\"", terrasNeutras, "\"/>" ]
  ++ [ T.concat ["<path class=\"cena-pais\" data-c=\"", T.singleton (formaCodigo f), "\" d=\"", formaD f, "\"/>"]
     | f <- formas ]
  ++ [ "<path class=\"fronteira\" d=\"", fronteirasFortes, "\"/>", "</svg>" ]

-- ---------------------------------------------------------------------------
-- Painel lateral

fimBox :: GameState -> Fim -> Html ()
fimBox st f = div_ [class_ ("papel memo fim " <> classe)] $ do
  h3_ "Fim da campanha"
  hr_ []
  h2_ titulo
  p_ (toHtml (mensagemFim st f))
  when (gsModo st == Resistencia) $
    p_ ("Refugiados salvos: " <> toHtml (tshow (gsRefugiados st)))
  p_ [class_ "nota"] "A partida foi salva no histórico."
  div_ [class_ "fim-botoes"] $ do
    button_ [id_ "rever-final", class_ "btn", type_ "button"] "▶ Rever o final"
    button_ [ class_ "btn"
            , data_ "hx-post" "/reiniciar"
            , data_ "hx-target" "#jogo" ] "Jogar de novo"
  where
    titulo :: Html ()
    classe :: Text
    (titulo, classe) = case f of
      Vitoria lado
        | lado == gsJogador st && resistencia -> ("Europa libertada!", "fim-normal")
        | lado == gsJogador st -> ("Vitória!", "fim-normal")
        | otherwise            -> ("Derrota", "fim-normal")
      Empate                   -> ("Impasse", "fim-normal")
      GuerraNuclear _
        | resistencia          -> ("A rede caiu", "fim-nuclear")
        | otherwise            -> ("Guerra nuclear", "fim-nuclear")
    resistencia :: Bool
    resistencia = gsModo st == Resistencia

-- | Memorando: quem você é, placar, corrida espacial (ou rotas de fuga) e o alerta.
memoBox :: GameState -> Html ()
memoBox st = div_ [class_ "papel memo"] $ do
  div_ [class_ "clipe"] ""
  h3_ "Memorando"
  hr_ []
  div_ [class_ "placar typed"] $ do
    linha "Comando" (toHtml (nomeDoLado st eu))
    if resistencia
      then do
        linha "Países libertados" (toHtml (tshow (libertados st)) <> " de " <> toHtml (tshow metaLibertacao))
        linha "Refugiados salvos" (toHtml (tshow (gsRefugiados st)))
      else do
        linha "Seus pontos" (toHtml (tshow (pontos eu st)))
        linha "Pontos do Reich" (toHtml (tshow (pontos ia st)))
        div_ [class_ "barra-pts"] $ do
          span_ [style_ ("width:" <> pct (pontos eu st) <> ";background:" <> corHex (rgbNucleo EUA))] ""
          span_ [style_ ("width:" <> pct (pontos ia st) <> ";background:" <> corHex (rgbNucleo Reich) <> ";margin-left:auto")] ""
  p_ [class_ "nota"] (toHtml objetivo)
  mapM_ (\e -> button_ [ type_ "button", class_ "btn btn-enigma", data_ "abrir" "enigma" ]
                  ("📻 Mensagem interceptada · decifrar (" <> toHtml (tshow (intTentativas e)) <> " tentativas)"))
        (gsEnigma st)
  div_ [class_ "espacial typed"] $ do
    span_ (if resistencia then "Rotas de fuga" else "Corrida espacial")
    pips eu
    unless resistencia (pips ia)
  when (isNothing (gsFim st) && nivelEspacial eu st < 4) $
    ul_ [class_ "acoes"] $
      if resistencia
        then botaoAcao st CorridaEspacial "Abrir rota de fuga" "salva refugiados e dá +1 recurso por trimestre"
        else botaoAcao st CorridaEspacial "Corrida espacial" "+1 de renda por trimestre"
  p_ [class_ ("nota alerta-texto" <> if gsTensao st >= 4 then " critico" else "")]
    (toHtml (if resistencia then descricaoRisco (gsTensao st) else descricaoTensao (gsTensao st)))
  where
    eu = gsJogador st
    ia = adversario eu
    resistencia :: Bool
    resistencia = gsModo st == Resistencia
    objetivo :: Text
    objetivo
      | resistencia = "Liberte " <> tshow metaLibertacao <> " países europeus até "
                      <> dataDoTurno turnoFinal <> ", sem deixar a Gestapo descobrir a rede."
      | otherwise   = "Vence quem chegar a " <> tshow pontosVitoria <> " pontos, ou quem tiver mais em "
                      <> dataDoTurno turnoFinal <> ". Países estratégicos valem 2."
    pct :: Int -> Text
    pct n = tshow (min 50 (n * 50 `div` max 1 pontosVitoria)) <> "%"
    linha :: Text -> Html () -> Html ()
    linha rotulo valor = div_ [class_ "lin"] $ do
      span_ (toHtml rotulo)
      b_ valor
    pips :: Lado -> Html ()
    pips lado = span_ [class_ "pips", title_ (nomeDoLado st lado)] $
      forM_ [1 .. 4 :: Int] $ \i ->
        span_ [ class_ "pip"
              , style_ (if i <= nivelEspacial lado st then "background:" <> corDoLado lado else "") ] ""
    corDoLado :: Lado -> Text
    corDoLado lado
      | resistencia && lado == EUA = corHex rgbLibertado
      | otherwise                  = corHex (rgbLado lado)

-- | Janela do minijogo da Enigma. O efeitos.js gira os rotores e mostra a
--   prévia; a tentativa vai para o servidor (POST /enigma/KDR), que confere em Haskell.
janelaEnigma :: GameState -> Interceptacao -> Html ()
janelaEnigma st e =
  div_ [class_ ("modal enigma" <> if intTentativas e < 3 then " aberto" else ""), id_ "m-enigma"] $
    div_ [class_ "folha papel"] $ do
      button_ [type_ "button", class_ "fechar-m", title_ "Fechar"] "×"
      p_ [class_ "nome-m"] "Mensagem interceptada"
      p_ [class_ "ed typed"] ("BERLIM → ? · RESTAM " <> toHtml (tshow (intTentativas e)) <> " TENTATIVAS")
      div_ [class_ "enigma-grade"] $ do
        div_ $ do
          p_ [class_ "rot typed"] "TEXTO CIFRADO"
          div_ [class_ "enigma-texto typed", id_ "enigma-cifrado"] (toHtml (intCifrada e))
          p_ [class_ "rot typed"] "COM ESTA POSIÇÃO DOS ROTORES, A MENSAGEM FICA:"
          div_ [class_ "enigma-texto claro typed", id_ "enigma-claro"] (toHtml (intCifrada e))
          p_ [class_ "nota"] "Dica do analista: toda mensagem do Reich termina com a assinatura BERLIM. Gire os rotores até a última palavra ficar certa; o resto se revela sozinho."
        div_ $ do
          p_ [class_ "rot typed"] "ROTORES"
          div_ [class_ "rotores"] $
            forM_ [0 .. 2 :: Int] $ \i -> div_ [class_ "rotor"] $ do
              button_ [type_ "button", class_ "girar", data_ "rotor" (tshow i), data_ "passo" "-1"] "▲"
              span_ [class_ "letra", data_ "rotor" (tshow i)] "A"
              button_ [type_ "button", class_ "girar", data_ "rotor" (tshow i), data_ "passo" "1"] "▼"
          div_ [class_ "enigma-botoes"] $ do
            button_ [type_ "button", class_ "btn-turno", id_ "enigma-enviar"] "Decifrar"
            button_ [ type_ "button", class_ "btn"
                    , data_ "hx-post" "/enigma/desistir", data_ "hx-target" "#jogo"
                    , data_ "hx-confirm" "Jogar a mensagem fora?" ] "Descartar"
          p_ [class_ "nota"] $ toHtml $
            if gsModo st == Resistencia
              then ("Acertando, a Resistência descobre o próximo alvo do Reich e se antecipa. Errando 3 vezes, o risco sobe." :: Text)
              else "Acertando, você descobre o próximo alvo do Reich e se antecipa. Errando 3 vezes, o alerta sobe."

descricaoRisco :: Int -> Text
descricaoRisco n = case n of
  0 -> "Risco: a rede está bem escondida."
  1 -> "Risco baixo."
  2 -> "Risco moderado. Sabotagens e levantes chamam a atenção da Gestapo."
  3 -> "Risco alto. A Gestapo está procurando."
  4 -> "Risco CRÍTICO. Mais uma ação arriscada e a rede cai."
  _ -> "A rede foi descoberta."

descricaoTensao :: Int -> Text
descricaoTensao n = case n of
  0 -> "Alerta: calma. A diplomacia funciona."
  1 -> "Alerta baixo."
  2 -> "Alerta moderado. Golpes e guerras aumentam a tensão."
  3 -> "Alerta alto. Cuidado com o próximo passo."
  4 -> "Alerta CRÍTICO. Mais uma crise e o mundo acaba."
  _ -> "Guerra nuclear."

-- | O dado que o efeitos.js anima no meio da tela antes de mostrar o resultado.
dadoBox :: Int -> Html ()
dadoBox d = div_ [class_ "dado", data_ "final" (tshow d)] (toHtml (tshow d))

-- | Aviso depois de uma ação (aparece como um telegrama no topo e some sozinho).
noticeBox :: Notice -> Html ()
noticeBox (Info t) = div_ [class_ "aviso aviso-ok typed"] (toHtml t)
noticeBox (Erro t) = div_ [class_ "aviso aviso-erro typed"] (toHtml t)

-- | Janela do país, aberta ao lado dele no mapa (como no Hearts of Iron IV).
--   O mapa.js coloca a janela perto do país clicado.
janelaPais :: GameState -> Pais -> Html ()
janelaPais st p = div_ [id_ "janela", class_ "papel aberta", data_ "c" (T.singleton (paisCodigo p))] $ do
  div_ [class_ "faixa", style_ ("background:" <> corDoPais (gsModo st) p)] ""
  div_ [class_ "corpo"] $ do
    div_ [class_ "titulo"] $ do
      h2_ $ do
        toHtmlRaw (bandeiraDoPais 40 p)
        span_ (toHtml (nomeExibido p))
      button_ [type_ "button", class_ "fechar", title_ "Fechar"] "×"
    div_ [class_ "tags typed"] $ do
      if paisEstrategico p
        then span_ [class_ "tag red"] "estratégico · 2 pts"
        else span_ [class_ "tag dim"] "1 ponto"
      span_ [class_ "tag dim"] (toHtml (textoControle st p))
    case paisNucleo p of
      Just lado ->
        p_ [class_ "aviso-acao"] ("Território " <> toHtml (doLado lado) <> ". Não pode ser disputado.")
      Nothing
        | resistencia && paisRegiao p /= Europa ->
            p_ [class_ "aviso-acao"] "Fora da Europa: a Resistência não atua aqui."
      Nothing -> do
        div_ [class_ ("escala" <> if resistencia then " escala-resistencia" else "")] $ do
          span_ [class_ "marca50", style_ "left:25%"] ""
          span_ [class_ "marca50", style_ "left:75%"] ""
          b_ [style_ ("left:" <> tshow pctAlinhamento <> "%")] ""
        div_ [class_ "esc-rot typed"] $ do
          span_ "Reich"
          span_ (toHtml (sinalizado (paisAlinhamento p)))
          span_ (if resistencia then "Resistência" else "EUA")
        div_ [class_ "estab typed"] $ do
          span_ "Estabilidade"
          span_ $ forM_ [1 .. 4 :: Int] $ \i ->
            i_ [class_ (if i <= paisEstabilidade p then "on" else "")] ""
        when (isNothing (gsFim st)) $ ul_ [class_ "acoes"] $ do
          let c = paisCodigo p
          if resistencia
            then do
              botaoAcao st (Influenciar c) "Rádio clandestino" "+15 a favor da Resistência"
              unless (controlador p == Just (gsJogador st)) $
                botaoAcao st (Golpe c) "Sabotagem"
                  ("sucesso +40 e ocupação mais instável · risco +" <> tshow (tensaoDaAcao (Golpe c) st))
              when (isNothing (controlador p)) $
                botaoAcao st (GuerraProcuracao c) "Levante" "vitória liberta o país · risco +2"
            else do
              botaoAcao st (Influenciar c) "Influenciar" "+15 a seu favor"
              unless (controlador p == Just (gsJogador st)) $
                botaoAcao st (Golpe c) "Golpe de Estado"
                  ("sucesso +40 · fracasso −10 · alerta +" <> tshow (tensaoDaAcao (Golpe c) st))
              when (isNothing (controlador p)) $
                botaoAcao st (GuerraProcuracao c) "Guerra por procuração" "vitória dá o controle · alerta +2"
  where
    resistencia :: Bool
    resistencia = gsModo st == Resistencia
    pctAlinhamento :: Int
    pctAlinhamento = (paisAlinhamento p + 100) `div` 2
    sinalizado :: Int -> Text
    sinalizado v = if v > 0 then "+" <> tshow v else tshow v

doLado :: Lado -> Text
doLado EUA   = "dos Estados Unidos"
doLado Reich = "do Reich Alemão"

textoControle :: GameState -> Pais -> Text
textoControle st p = case controlador p of
  Just EUA   | gsModo st == Resistencia -> "libertado"
  Just Reich | gsModo st == Resistencia -> "ocupado pelo Reich"
  Just lado
    | isNothing (paisNucleo p) -> "aliado " <> doLadoCurto lado
    | otherwise                -> "território " <> doLadoCurto lado
  Nothing
    | gsModo st == Resistencia && paisAlinhamento p > 0 -> "em disputa, inclinado à Resistência"
    | gsModo st == Resistencia && paisAlinhamento p < 0 -> "em disputa, inclinado ao Reich"
    | paisAlinhamento p > 0 -> "em disputa, inclinado aos EUA"
    | paisAlinhamento p < 0 -> "em disputa, inclinado ao Reich"
    | otherwise             -> "em disputa, neutro"
  where
    doLadoCurto :: Lado -> Text
    doLadoCurto EUA   = "dos EUA"
    doLadoCurto Reich = "do Reich"

-- | Botão de uma ação, como uma linha de ordem num dossiê ("Influenciar ...... 3").
--   Fica apagado sem orçamento e avisa se for causar guerra nuclear.
botaoAcao :: GameState -> Acao -> Text -> Text -> Html ()
botaoAcao st acao rotulo detalhe = li_ $
  button_ ([ class_ (if perigo then "acao perigo" else "acao")
           , data_ "hx-post" (urlAcao acao)
           , data_ "hx-target" "#jogo" ]
           ++ [ disabled_ "disabled" | not podePagar ]
           ++ [ data_ "hx-confirm" (avisoFatal st) | perigo ]) $ do
    div_ [class_ "l"] $ do
      em_ (toHtml rotulo)
      toHtml (tshow preco)
    div_ [class_ "d"] $ do
      toHtml textoDetalhe
      case chanceSucesso acao st of
        Just pct | podePagar -> do
          toHtml (" · " <> tshow pct <> "%")
          span_ [class_ "chance"] (span_ [style_ ("width:" <> tshow pct <> "%")] "")
        _ -> ""
  where
    preco     = custo acao st
    podePagar = orcamento (gsJogador st) st >= preco
    perigo    = gsTensao st + tensaoDaAcao acao st >= tensaoMaxima
    textoDetalhe :: Text
    textoDetalhe
      | not podePagar = "orçamento insuficiente"
      | perigo        = resumoFatal st <> " · " <> detalhe
      | otherwise     = detalhe

urlAcao :: Acao -> Text
urlAcao (Influenciar c)      = "/influenciar/" <> T.singleton c
urlAcao (Golpe c)            = "/golpe/" <> T.singleton c
urlAcao (GuerraProcuracao c) = "/guerra/" <> T.singleton c
urlAcao CorridaEspacial      = "/espacial"

-- | Formulário que salva a partida no banco (POST /salvar).
saveBox :: GameState -> Html ()
saveBox st = div_ [class_ "papel memo"] $ do
  h3_ "Salvar campanha"
  hr_ []
  form_ [data_ "hx-post" "/salvar", data_ "hx-target" "#jogo", class_ "form-salvar"] $ do
    input_ [ type_ "text", name_ "nomeComandante", class_ "campo typed", maxlength_ "40"
           , placeholder_ "Nome do comandante", value_ (gsComandante st) ]
    button_ [type_ "submit", class_ "btn"] "Salvar"
  case gsPartidaId st of
    Just pid -> p_ [class_ "nota"] ("Salva como partida #" <> toHtml (show pid))
    Nothing  -> p_ [class_ "nota"] "Ainda não salva."
  p_ (a_ [href_ "/historico.html", class_ "link typed"] "Ver histórico de campanhas →")

-- | Telegramas: os últimos acontecimentos da partida.
logBox :: GameState -> Html ()
logBox st = div_ [class_ "papel memo"] $ do
  h3_ "Telegramas"
  hr_ []
  ul_ [class_ "telegramas typed"] $
    forM_ (zip [0 :: Int ..] (take 12 (gsLog st))) $ \(i, linhaLog) ->
      li_ [class_ (if i == 0 then "novo" else "")] $ do
        let (quando, texto) = separar linhaLog
        unless (T.null quando) $ b_ (toHtml quando)
        toHtml texto
  where
    -- "Abril de 1962: texto" -> ("Abr 1962", "texto")
    separar :: Text -> (Text, Text)
    separar t = case T.breakOn ": " t of
      (antes, resto)
        | not (T.null resto) && " de " `T.isInfixOf` antes ->
            (T.take 3 antes <> " " <> T.takeEnd 4 antes, T.drop 2 resto)
      _ -> ("", t)

legenda :: GameState -> Html ()
legenda st = div_ [class_ "leg"] $
  if gsModo st == Resistencia
    then do
      item (corHex (rgbNucleo EUA)) "aliados"
      item (corHex rgbLibertado) "libertado"
      item (corHex rgbNeutro) "em disputa"
      item (corHex (rgbLado Reich)) "ocupado"
      item (corHex (rgbNucleo Reich)) "Reich"
    else do
      item (corHex (rgbNucleo EUA)) "EUA"
      item (corHex (rgbLado EUA)) "aliado"
      item (corHex rgbNeutro) "em disputa"
      item (corHex (rgbLado Reich)) "aliado do Reich"
      item (corHex (rgbNucleo Reich)) "Reich"
  where
    item :: Text -> Html () -> Html ()
    item cor txt = span_ $ do
      i_ [style_ ("background:" <> cor)] ""
      txt

-- | Botões de zoom em cima do mapa (o mapa.js faz o zoom).
controlesZoom :: Html ()
controlesZoom = div_ [class_ "zooms typed"] $ do
  forM_ regioesZoom $ \(chave, rotulo) ->
    button_ [type_ "button", class_ "zoom-btn", data_ "zoom" chave] (toHtml rotulo)
  button_ [type_ "button", class_ "zoom-btn", data_ "zoom" "menos", title_ "Afastar"] "−"
  button_ [type_ "button", class_ "zoom-btn", data_ "zoom" "mais", title_ "Aproximar"] "+"

regioesZoom :: [(Text, Text)]
regioesZoom =
  [ ("mundo", "Mundo"), ("americas", "Américas"), ("europa", "Europa")
  , ("africa", "África"), ("asia", "Ásia") ]

-- ---------------------------------------------------------------------------
-- Cores (paleta do mapa no estilo HOI4)

type RGB = (Int, Int, Int)

rgbLado :: Lado -> RGB
rgbLado EUA   = (63, 127, 216)
rgbLado Reich = (192, 73, 60)

rgbNucleo :: Lado -> RGB
rgbNucleo EUA   = (37, 86, 176)
rgbNucleo Reich = (75, 79, 86)

rgbNeutro :: RGB
rgbNeutro = (165, 154, 114)

-- | Verde dos países libertados pela Resistência.
rgbLibertado :: RGB
rgbLibertado = (79, 154, 94)

misturar :: Double -> RGB -> RGB -> RGB
misturar t (r1, g1, b1) (r2, g2, b2) = (f r1 r2, f g1 g2, f b1 b2)
  where
    f :: Int -> Int -> Int
    f a b = round (fromIntegral a + t * fromIntegral (b - a) :: Double)

corHex :: RGB -> Text
corHex (r, g, b) = T.pack (printf "#%02x%02x%02x" r g b)

-- | Cor do país (em RGB): forte se controlado, tons intermediários se em disputa.
--   No modo Resistência, o lado aliado aparece em verde (libertado).
rgbDoPais :: Modo -> Pais -> RGB
rgbDoPais modo p = case (paisNucleo p, controlador p) of
  (Just lado, _) -> rgbNucleo lado
  (_, Just EUA)  -> aliado
  (_, Just lado) -> rgbLado lado
  _
    | a > 0     -> misturar t rgbNeutro aliado
    | a < 0     -> misturar t rgbNeutro (rgbLado Reich)
    | otherwise -> rgbNeutro
  where
    aliado :: RGB
    aliado = if modo == Resistencia then rgbLibertado else rgbLado EUA
    a = paisAlinhamento p
    t :: Double
    t = 0.65 * fromIntegral (abs a) / fromIntegral limiteControle

corDoPais :: Modo -> Pais -> Text
corDoPais modo p = corHex (rgbDoPais modo p)

-- | Cor mais clara usada no brilho da borda de cada país.
corDoBrilho :: Modo -> Pais -> Text
corDoBrilho modo p = corHex (misturar 0.32 (rgbDoPais modo p) (255, 255, 255))

-- ---------------------------------------------------------------------------
-- Mapa SVG do jogo (gerado como texto: simples e sem dependência extra)
--
-- Camadas, de baixo para cima: mar com textura, brilho da costa, grade,
-- terras neutras, países, relevo, brilho interno de cada país, fronteiras,
-- seleção, sombra nas bordas e nomes. Cada contorno é escrito uma vez só
-- (em <defs>) e reaproveitado com <use>, pra resposta não ficar pesada.

mapSvg :: GameState -> Maybe Char -> Text
mapSvg st sel = T.concat $
  [ "<svg xmlns=\"http://www.w3.org/2000/svg\" class=\"mapa-svg\" id=\"mapa\" viewBox=\"0 0 "
  , num larguraMapa, " ", num alturaMapa, "\">"
  , "<defs>"
  , "<filter id=\"relevo\" x=\"0\" y=\"0\" width=\"100%\" height=\"100%\"><feTurbulence type=\"fractalNoise\" baseFrequency=\".035\" numOctaves=\"5\" seed=\"7\"/><feDiffuseLighting lighting-color=\"#fff\" surfaceScale=\"2.2\"><feDistantLight azimuth=\"235\" elevation=\"38\"/></feDiffuseLighting></filter>"
  , "<filter id=\"mar-relevo\" x=\"0\" y=\"0\" width=\"100%\" height=\"100%\"><feTurbulence type=\"fractalNoise\" baseFrequency=\".02\" numOctaves=\"3\" seed=\"2\"/><feDiffuseLighting lighting-color=\"#fff\" surfaceScale=\"1.2\"><feDistantLight azimuth=\"235\" elevation=\"50\"/></feDiffuseLighting></filter>"
  , "<filter id=\"brilho\"><feGaussianBlur stdDeviation=\"2.2\"/></filter>"
  , "<filter id=\"halo\" x=\"-20%\" y=\"-20%\" width=\"140%\" height=\"140%\"><feGaussianBlur stdDeviation=\"4\"/></filter>"
  , "<radialGradient id=\"vinheta\" cx=\"50%\" cy=\"45%\" r=\"70%\"><stop offset=\"60%\" stop-color=\"#000\" stop-opacity=\"0\"/><stop offset=\"100%\" stop-color=\"#000\" stop-opacity=\".65\"/></radialGradient>"
  , "<path id=\"terras-neutras\" d=\"", terrasNeutras, "\"/>" ]
  ++ [ T.concat ["<path id=\"f-", cod f, "\" d=\"", formaD f, "\"/>"] | f <- formas ]
  ++ [ T.concat ["<clipPath id=\"c-", cod f, "\"><use href=\"#f-", cod f, "\"/></clipPath>"] | f <- formas ]
  ++ [ "</defs>"
     , "<rect x=\"-500\" y=\"-300\" width=\"2000\" height=\"1040\" fill=\"#172434\"/>"
     , "<g class=\"textura\" opacity=\".6\"><rect width=\"1000\" height=\"440\" fill=\"#888\" filter=\"url(#mar-relevo)\"/></g>"
     , "<g fill=\"none\" stroke=\"#7fb2cc\" stroke-width=\"7\" opacity=\".35\" filter=\"url(#halo)\" pointer-events=\"none\">"
     , usarTodos, "</g>"
     , "<path class=\"grade-geo\" d=\"", gradeGeografica, "\"/>"
     , "<use href=\"#terras-neutras\" class=\"terra-neutra\"/>"
     , "<g class=\"paises\">" ]
  ++ map territorio paisesNoMapa
  ++ [ "</g>"
     , "<g class=\"textura\" opacity=\".9\" pointer-events=\"none\"><g fill=\"#888\" filter=\"url(#relevo)\">"
     , usarTodos, "</g></g>"
     , "<g pointer-events=\"none\">" ]
  ++ map brilho paisesNoMapa
  ++ [ "</g>"
     , "<path class=\"fronteira-fraca\" d=\"", fronteirasFracas, "\"/>"
     , "<path class=\"fronteira\" d=\"", fronteirasFortes, "\"/>" ]
  ++ concat [ [ "<path class=\"sel-brilho\" d=\"", formaBorda f, "\"/>"
              , "<path class=\"fronteira-sel\" d=\"", formaBorda f, "\"/>" ]
            | f <- formas, Just (formaCodigo f) == sel ]
  ++ [ "<rect x=\"-500\" y=\"-300\" width=\"2000\" height=\"1040\" fill=\"url(#vinheta)\" pointer-events=\"none\"/>"
     , "<g class=\"nomes\">" ]
  ++ map rotulo paisesNoMapa
  ++ [ "</g></svg>" ]
  where
    paisesNoMapa :: [(Forma, Pais)]
    paisesNoMapa = [ (f, p) | f <- formas, Just p <- [Map.lookup (formaCodigo f) (gsPaises st)] ]

    cod :: Forma -> Text
    cod f = T.singleton (formaCodigo f)

    usarTodos :: Text
    usarTodos = T.concat ("<use href=\"#terras-neutras\"/>" : [ T.concat ["<use href=\"#f-", cod f, "\"/>"] | f <- formas ])

    resistencia :: Bool
    resistencia = gsModo st == Resistencia

    territorio :: (Forma, Pais) -> Text
    territorio (f, p) =
      let classes = T.unwords $ [ "pais" ]
                      ++ [ "mudou" | formaCodigo f `elem` gsMudaram st ]
                      ++ [ "fora" | resistencia, paisRegiao p /= Europa, isNothing (paisNucleo p) ]
          info    = textoControle st p <> maybe (" · " <> sinal (paisAlinhamento p)) (const "") (paisNucleo p)
      in T.concat
           [ "<use href=\"#f-", cod f, "\" class=\"", classes, "\" fill=\"", corDoPais (gsModo st) p
           , "\" data-c=\"", cod f
           , "\" data-nome=\"", atributo (nomeExibido p)
           , "\" data-info=\"", atributo info
           , "\" data-bandeira=\"", atributo (bandeiraDoPais 22 p)
           , "\" data-hx-get=\"/pais/", cod f
           , "\" data-hx-target=\"#jogo\"/>" ]

    brilho :: (Forma, Pais) -> Text
    brilho (f, p) = T.concat
      [ "<g clip-path=\"url(#c-", cod f, ")\"><use href=\"#f-", cod f
      , "\" fill=\"none\" stroke=\"", corDoBrilho (gsModo st) p
      , "\" stroke-width=\"9\" filter=\"url(#brilho)\" opacity=\".85\"/></g>" ]

    rotulo :: (Forma, Pais) -> Text
    rotulo (f, p) = T.concat
      [ "<text class=\"nome-pais\" data-c=\"", cod f
      , "\" data-sig=\"", paisSigla p
      , "\" x=\"", num (formaX f), "\" y=\"", num (formaY f)
      , "\">", atributo (nomeExibido p), "</text>" ]

    sinal :: Int -> Text
    sinal v = if v > 0 then "+" <> tshow v else tshow v

-- | Escapa um texto para ir dentro de um atributo ou texto do SVG.
atributo :: Text -> Text
atributo = T.replace "\"" "&quot;" . T.replace "<" "&lt;" . T.replace ">" "&gt;" . T.replace "&" "&amp;"

-- ---------------------------------------------------------------------------
-- Utilitários

tshow :: Show a => a -> Text
tshow = T.pack . show

-- | Número com uma casa decimal, pra deixar o SVG enxuto.
num :: Double -> Text
num d = tshow (fromIntegral (round (d * 10) :: Int) / 10 :: Double)
