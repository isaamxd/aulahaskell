module Main (main) where

import Data.Either (isLeft, isRight)
import Data.List (nub)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, isNothing)
import Game.Crises
import Game.Geografia
import Game.Map
import Game.Rules
import Game.Types
import Test.Hspec
import Test.QuickCheck

-- | Uma partida jogando com os EUA, sempre com a mesma semente.
estadoTeste :: GameState
estadoTeste = iniciarPartida EUA 42 "Teste"

-- | O mesmo estado, mas com orçamento de sobra.
comDinheiro :: GameState -> GameState
comDinheiro s = s { gsOrcamento = Map.fromList [(EUA, 1000), (Reich, 1000)] }

-- | Gera uma ação qualquer, num país qualquer (inclusive inválidas).
genAcao :: Gen Acao
genAcao = do
  c <- elements (Map.keys (gsPaises estadoTeste))
  elements [Influenciar c, Golpe c, GuerraProcuracao c, CorridaEspacial]

-- | Aplica várias ações do jogador; as que falharem são ignoradas.
aplicarVarias :: [Acao] -> GameState -> GameState
aplicarVarias acoes st0 = foldl passo st0 acoes
  where
    passo s a = either (const s) id (aplicarAcao (gsJogador s) a s)

-- | Passa o trimestre; se houver crise, escolhe a resposta A antes.
passarTurno :: GameState -> GameState
passarTurno s = encerrarTurno (either (const s) id (resolverCrise True s))

alinhamentoDe :: Char -> GameState -> Maybe Int
alinhamentoDe c st = fmap paisAlinhamento (Map.lookup c (gsPaises st))

-- | Todos os países citados nas crises (condições e efeitos).
paisesCitados :: Crise -> [Char]
paisesCitados c =
  [ p | NaoEhDoJogador p <- criseCondicoes c ]
  ++ concat [ [ p | Influencia p _ <- opcaoEfeitos o ] ++ [ p | Estabilidade p _ <- opcaoEfeitos o ]
            | o <- [criseA c, criseB c] ]

main :: IO ()
main = hspec $ do
  describe "Mapa" $ do
    it "todo país cadastrado tem um contorno no mapa" $
      all (\p -> paisCodigo p `elem` map formaCodigo formas) paisesIniciais `shouldBe` True

    it "todo contorno do mapa pertence a um país cadastrado" $
      all (\f -> Map.member (formaCodigo f) (gsPaises initialState)) formas `shouldBe` True

    it "os códigos dos países são únicos" $
      length (nub (map paisCodigo paisesIniciais)) `shouldBe` length paisesIniciais

    it "as siglas ficam dentro da área do mapa" $
      all (\f -> formaX f >= 0 && formaX f <= larguraMapa && formaY f >= 0 && formaY f <= alturaMapa) formas
        `shouldBe` True

  describe "Ações" $ do
    it "influenciar aproxima o país do seu lado (+15)" $
      case aplicarAcao EUA (Influenciar 'B') estadoTeste of
        Left e   -> expectationFailure (show e)
        Right st -> alinhamentoDe 'B' st `shouldBe` fmap (+ 15) (alinhamentoDe 'B' estadoTeste)

    it "a mensagem usa a preposição certa (no Brasil)" $
      case aplicarAcao EUA (Influenciar 'B') estadoTeste of
        Left e   -> expectationFailure (show e)
        Right st -> take 1 (gsLog st) `shouldBe` ["Janeiro de 1962: Washington amplia sua influência no Brasil."]

    it "não deixa agir dentro de uma superpotência" $
      aplicarAcao EUA (Influenciar 'D') (comDinheiro estadoTeste) `shouldSatisfy` isLeft

    it "não deixa gastar mais do que o orçamento" $ do
      let pobre = estadoTeste { gsOrcamento = Map.fromList [(EUA, 0), (Reich, 0)] }
      aplicarAcao EUA (Influenciar 'B') pobre `shouldSatisfy` isLeft

    it "levar a tensão ao máximo causa guerra nuclear (e quem causou perde)" $ do
      let tenso = (comDinheiro estadoTeste) { gsTensao = 4 }
      case aplicarAcao EUA (GuerraProcuracao 'B') tenso of
        Left e   -> expectationFailure (show e)
        Right st -> gsFim st `shouldBe` Just (GuerraNuclear EUA)

    it "alinhamento sempre fica entre -100 e 100" $
      forAll (listOf genAcao) $ \acoes ->
        let st = aplicarVarias acoes (comDinheiro estadoTeste)
        in all (\p -> abs (paisAlinhamento p) <= 100) (Map.elems (gsPaises st))

    it "orçamento nunca fica negativo" $
      forAll (listOf genAcao) $ \acoes ->
        all (>= 0) (Map.elems (gsOrcamento (aplicarVarias acoes estadoTeste)))

    it "tensão fica sempre entre 0 e 5" $
      forAll (listOf genAcao) $ \acoes ->
        let t = gsTensao (aplicarVarias acoes (comDinheiro estadoTeste))
        in t >= 0 && t <= tensaoMaxima

    it "o dado sempre dá um número de 1 a 6" $
      property $ \(NonNegative s) ->
        let (d, _) = rolarDado estadoTeste { gsSemente = s `mod` 2147483648 }
        in d >= 1 && d <= 6

  describe "Crises" $ do
    it "toda crise tem um número único" $
      length (nub (map criseId crises)) `shouldBe` length crises

    it "as crises só citam países que existem" $
      all (`Map.member` gsPaises initialState) (concatMap paisesCitados crises) `shouldBe` True

    it "encerrar o trimestre abre o jornal com uma crise" $ do
      let st = encerrarTurno estadoTeste
      gsJornal st `shouldBe` True
      isJust (gsCrise st) `shouldBe` True

    it "com uma crise pendente, não dá pra agir nem passar o trimestre" $ do
      let st = encerrarTurno (comDinheiro estadoTeste)
      aplicarAcao EUA (Influenciar 'B') st `shouldSatisfy` isLeft
      encerrarTurno st `shouldBe` st

    it "resolver a crise fecha o jornal e ela não se repete" $ do
      let st = encerrarTurno estadoTeste
      case (gsCrise st, resolverCrise False st) of
        (Just cid, Right st') -> do
          isNothing (gsCrise st') `shouldBe` True
          gsJornal st' `shouldBe` False
          cid `elem` gsCrisesVistas st' `shouldBe` True
          cid `elem` map criseId (crisesDisponiveis st') `shouldBe` False
        _ -> expectationFailure "esperava uma crise resolvida"

    it "não dá pra resolver crise quando não há nenhuma" $
      resolverCrise True estadoTeste `shouldSatisfy` isLeft

    it "crises exclusivas de um lado não aparecem pro outro" $ do
      let reich = iniciarPartida Reich 7 ""
      all (\c -> SoPara EUA `notElem` criseCondicoes c) (crisesDisponiveis reich) `shouldBe` True

    it "fechar o jornal funciona quando não há crise" $
      fecharJornal estadoTeste { gsJornal = True } `shouldSatisfy` isRight

  describe "Partida" $ do
    it "o dado é puro: mesma semente, mesmo resultado" $
      encerrarTurno estadoTeste `shouldBe` encerrarTurno estadoTeste

    it "a IA nunca provoca guerra nuclear" $
      forAll (choose (0, 100000)) $ \s ->
        let st = iterate passarTurno (iniciarPartida EUA s "") !! 10
        in gsFim st /= Just (GuerraNuclear Reich)

    it "a partida termina depois do último trimestre" $
      isJust (gsFim (iterate passarTurno estadoTeste !! (turnoFinal + 1))) `shouldBe` True
