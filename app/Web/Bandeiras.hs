-- | Bandeiras dos países (SVG simplificado, 30x20).
--
--   ARQUIVO GERADO AUTOMATICAMENTE a partir de bandeiras.js: dá pra editar,
--   mas cada bandeira é só um texto com o desenho em SVG.
--
--   A bandeira muda conforme quem controla o país: os territórios do Reich
--   ganham a bandeira de libertados quando passam para o lado aliado, e os
--   países comuns dominados pelo Reich ganham um selo de ocupação no canto.
module Web.Bandeiras
  ( bandeiraDoPais
  , nomeExibido
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Game.Rules (controlador)
import Game.Types

-- | Desenho da bandeira de um país, já como um <svg> completo.
bandeiraDoPais :: Int -> Pais -> Text
bandeiraDoPais largura p = T.concat
  [ "<svg class=\"bandeira\" viewBox=\"0 0 30 20\" width=\"", T.pack (show largura)
  , "\" height=\"", T.pack (show (largura * 2 `div` 3)), "\" aria-hidden=\"true\">"
  , corpo
  , "<rect x=\".25\" y=\".25\" width=\"29.5\" height=\"19.5\" fill=\"none\" stroke=\"#0006\" stroke-width=\".5\"/></svg>" ]
  where
    c = paisCodigo p
    corpo = case (controlador p, libertada c) of
      (Just EUA, Just livre)            -> livre
      (_, Just _)                       -> bandeira c
      (Just Reich, Nothing)
        | paisNucleo p == Nothing       -> bandeira c <> seloOcupacao
      _                                 -> bandeira c

-- | Nome mostrado na tela: alguns territórios mudam de nome quando são libertados.
nomeExibido :: Pais -> Text
nomeExibido p = case (controlador p, paisCodigo p) of
  (Just EUA, 'z') -> "África Central"
  (Just EUA, 'w') -> "Rússia Ocidental"
  (Just EUA, 'K') -> "Ucrânia"
  (Just EUA, 'F') -> "França"
  _               -> T.strip (fst (T.breakOn "(" (paisNome p)))

-- | Selo com as cores do Reich, no canto da bandeira de um país ocupado.
seloOcupacao :: Text
seloOcupacao = "<polygon points=\"30,8 30,12 24,12\" fill=\"#000\"/><polygon points=\"24,12 30,12 30,16 18,16\" fill=\"#fff\"/><polygon points=\"18,16 30,16 30,20 12,20\" fill=\"#dd0000\"/><polygon points=\"30,8 30,20 12,20\" fill=\"none\" stroke=\"#000\" stroke-width=\".6\"/><circle cx=\"26.3\" cy=\"15.6\" r=\"2.5\" fill=\"#fff\" stroke=\"#000\" stroke-width=\".35\"/><rect x=\"25.85\" y=\"13.8\" width=\".9\" height=\"3.6\" fill=\"#000\"/><rect x=\"24.5\" y=\"15.15\" width=\"3.6\" height=\".9\" fill=\"#000\"/>"

-- | Bandeira de quando o território é libertado (só para alguns).
libertada :: Char -> Maybe Text
libertada c = case c of
  'z' -> Just "<rect width=\"30\" height=\"20\" fill=\"#009e49\"/><rect y=\"6.67\" width=\"30\" height=\"6.67\" fill=\"#fcd116\"/><rect y=\"13.33\" width=\"30\" height=\"6.67\" fill=\"#ce1126\"/><polygon points=\"15.00,7.40 14.36,9.12 12.53,9.20 13.96,10.34 13.47,12.10 15.00,11.09 16.53,12.10 16.04,10.34 17.47,9.20 15.64,9.12\" fill=\"#000\"/>"
  'w' -> Just "<rect width=\"30\" height=\"20\" fill=\"#fff\"/><rect y=\"6.67\" width=\"30\" height=\"6.67\" fill=\"#0039a6\"/><rect y=\"13.33\" width=\"30\" height=\"6.67\" fill=\"#d52b1e\"/>"
  'K' -> Just "<rect width=\"30\" height=\"20\" fill=\"#0057b7\"/><rect y=\"10\" width=\"30\" height=\"10\" fill=\"#ffd700\"/>"
  'F' -> Just "<rect width=\"30\" height=\"20\" fill=\"#002395\"/><rect x=\"10\" width=\"10\" height=\"20\" fill=\"#fff\"/><rect x=\"20\" width=\"10\" height=\"20\" fill=\"#ed2939\"/><g fill=\"#ed2939\"><rect x=\"14.3\" y=\"4\" width=\"1.4\" height=\"12\"/><rect x=\"12.4\" y=\"6.4\" width=\"5.2\" height=\"1.2\"/><rect x=\"11.6\" y=\"9\" width=\"6.8\" height=\"1.2\"/></g>"
  _   -> Nothing

-- | Bandeira normal de cada país.
bandeira :: Char -> Text
bandeira c = case c of
  '1' -> "<rect width=\"30\" height=\"20\" fill=\"#fff\"/><rect width=\"30\" height=\"10\" fill=\"#ce1126\"/>"
  '2' -> "<rect width=\"30\" height=\"20\" fill=\"#0038a8\"/><rect y=\"10\" width=\"30\" height=\"10\" fill=\"#ce1126\"/><polygon points=\"0,0 17.3,10 0,20\" fill=\"#fff\"/><circle cx=\"6\" cy=\"10\" r=\"2.2\" fill=\"#fcd116\"/>"
  '3' -> "<rect width=\"30\" height=\"20\" fill=\"#fff\"/><circle cx=\"15\" cy=\"10\" r=\"5\" fill=\"#003478\"/><path d=\"M10 10A5 5 0 0 1 20 10A2.5 2.5 0 0 1 15 10A2.5 2.5 0 0 0 10 10Z\" fill=\"#c60c30\"/><g stroke=\"#000\" stroke-width=\".9\"><path d=\"M4 4.5L7 2.3M4.8 5.6L7.8 3.4M5.6 6.7L8.6 4.5M22.4 15.5L25.4 13.3M23.2 16.6L26.2 14.4M24 17.7L27 15.5M22.4 4.5L25.4 6.7M23.2 3.4L26.2 5.6M24 2.3L27 4.5M4 15.5L7 17.7M4.8 14.4L7.8 16.6M5.6 13.3L8.6 15.5\"/></g>"
  'U' -> "<rect width=\"30\" height=\"20\" fill=\"#fff\"/><rect y=\"0\" width=\"30\" height=\"1.5384615384615385\" fill=\"#b22234\"/><rect y=\"3.076923076923077\" width=\"30\" height=\"1.5384615384615385\" fill=\"#b22234\"/><rect y=\"6.153846153846154\" width=\"30\" height=\"1.5384615384615385\" fill=\"#b22234\"/><rect y=\"9.23076923076923\" width=\"30\" height=\"1.5384615384615385\" fill=\"#b22234\"/><rect y=\"12.307692307692308\" width=\"30\" height=\"1.5384615384615385\" fill=\"#b22234\"/><rect y=\"15.384615384615385\" width=\"30\" height=\"1.5384615384615385\" fill=\"#b22234\"/><rect y=\"18.46153846153846\" width=\"30\" height=\"1.5384615384615385\" fill=\"#b22234\"/><rect width=\"13\" height=\"10.77\" fill=\"#3c3b6e\"/>"
  'C' -> "<rect width=\"30\" height=\"20\" fill=\"#c8102e\"/><g transform=\"scale(0.43333333333333335,0.45)\"><rect width=\"30\" height=\"20\" fill=\"#1f3a7a\"/><path d=\"M0 0L30 20M30 0L0 20\" stroke=\"#fff\" stroke-width=\"4\"/><path d=\"M0 0L30 20M30 0L0 20\" stroke=\"#c8102e\" stroke-width=\"1.4\"/><rect x=\"12\" width=\"6\" height=\"20\" fill=\"#fff\"/><rect y=\"7\" width=\"30\" height=\"6\" fill=\"#fff\"/><rect x=\"13.2\" width=\"3.6\" height=\"20\" fill=\"#c8102e\"/><rect y=\"8.2\" width=\"30\" height=\"3.6\" fill=\"#c8102e\"/></g><circle cx=\"22\" cy=\"10\" r=\"2.6\" fill=\"#e9d38a\"/>"
  'M' -> "<rect width=\"30\" height=\"20\" fill=\"#006847\"/><rect x=\"10\" width=\"10\" height=\"20\" fill=\"#fff\"/><rect x=\"20\" width=\"10\" height=\"20\" fill=\"#ce1126\"/><circle cx=\"15\" cy=\"10\" r=\"2\" fill=\"#8a5a2b\"/>"
  'k' -> "<rect width=\"30\" height=\"20\" fill=\"#002a8f\"/><rect y=\"4\" width=\"30\" height=\"4\" fill=\"#fff\"/><rect y=\"12\" width=\"30\" height=\"4\" fill=\"#fff\"/><polygon points=\"0,0 13,10 0,20\" fill=\"#cf142b\"/><polygon points=\"4.50,7.40 3.86,9.12 2.03,9.20 3.46,10.34 2.97,12.10 4.50,11.09 6.03,12.10 5.54,10.34 6.97,9.20 5.14,9.12\" fill=\"#fff\"/>"
  'a' -> "<rect width=\"30\" height=\"20\" fill=\"#0f47af\"/><rect y=\"6.67\" width=\"30\" height=\"6.67\" fill=\"#fff\"/><rect y=\"13.33\" width=\"30\" height=\"6.67\" fill=\"#0f47af\"/><polygon points=\"15.00,7.60 14.41,9.18 12.72,9.26 14.04,10.31 13.59,11.94 15.00,11.01 16.41,11.94 15.96,10.31 17.28,9.26 15.59,9.18\" fill=\"#0f47af\"/>"
  'o' -> "<rect width=\"30\" height=\"20\" fill=\"#fcd116\"/><rect y=\"10\" width=\"30\" height=\"5\" fill=\"#003893\"/><rect y=\"15\" width=\"30\" height=\"5\" fill=\"#ce1126\"/>"
  'v' -> "<rect width=\"30\" height=\"20\" fill=\"#fcd116\"/><rect y=\"6.67\" width=\"30\" height=\"6.67\" fill=\"#00247d\"/><rect y=\"13.33\" width=\"30\" height=\"6.67\" fill=\"#cf142b\"/>"
  'p' -> "<rect width=\"30\" height=\"20\" fill=\"#d91023\"/><rect x=\"10\" width=\"10\" height=\"20\" fill=\"#fff\"/><rect x=\"20\" width=\"10\" height=\"20\" fill=\"#d91023\"/>"
  'B' -> "<rect width=\"30\" height=\"20\" fill=\"#009c3b\"/><polygon points=\"15,2.5 27.5,10 15,17.5 2.5,10\" fill=\"#ffdf00\"/><circle cx=\"15\" cy=\"10\" r=\"4.2\" fill=\"#002776\"/><path d=\"M11 9.3Q15 8.3 19 10.6\" stroke=\"#fff\" stroke-width=\".7\" fill=\"none\"/>"
  'h' -> "<rect width=\"30\" height=\"20\" fill=\"#fff\"/><rect y=\"10\" width=\"30\" height=\"10\" fill=\"#d52b1e\"/><rect width=\"10\" height=\"10\" fill=\"#0039a6\"/><polygon points=\"5.00,2.40 4.36,4.12 2.53,4.20 3.96,5.34 3.47,7.10 5.00,6.09 6.53,7.10 6.04,5.34 7.47,4.20 5.64,4.12\" fill=\"#fff\"/>"
  'A' -> "<rect width=\"30\" height=\"20\" fill=\"#74acdf\"/><rect y=\"6.67\" width=\"30\" height=\"6.67\" fill=\"#fff\"/><rect y=\"13.33\" width=\"30\" height=\"6.67\" fill=\"#74acdf\"/><circle cx=\"15\" cy=\"10\" r=\"1.9\" fill=\"#f6b40e\"/>"
  'R' -> "<g transform=\"scale(1,1)\"><rect width=\"30\" height=\"20\" fill=\"#1f3a7a\"/><path d=\"M0 0L30 20M30 0L0 20\" stroke=\"#fff\" stroke-width=\"4\"/><path d=\"M0 0L30 20M30 0L0 20\" stroke=\"#c8102e\" stroke-width=\"1.4\"/><rect x=\"12\" width=\"6\" height=\"20\" fill=\"#fff\"/><rect y=\"7\" width=\"30\" height=\"6\" fill=\"#fff\"/><rect x=\"13.2\" width=\"3.6\" height=\"20\" fill=\"#c8102e\"/><rect y=\"8.2\" width=\"30\" height=\"3.6\" fill=\"#c8102e\"/></g>"
  'D' -> "<rect width=\"30\" height=\"20\" fill=\"#000\"/><rect y=\"6.67\" width=\"30\" height=\"6.67\" fill=\"#fff\"/><rect y=\"13.33\" width=\"30\" height=\"6.67\" fill=\"#dd0000\"/><circle cx=\"15\" cy=\"10\" r=\"6.3\" fill=\"#fff\"/><rect x=\"13.7\" y=\"5\" width=\"2.6\" height=\"10\" fill=\"#000\"/><rect x=\"10\" y=\"8.7\" width=\"10\" height=\"2.6\" fill=\"#000\"/><path d=\"M10 8.1H13.1V5M16.9 5V8.1H20M10 11.9H13.1V15M16.9 15V11.9H20\" fill=\"none\" stroke=\"#000\" stroke-width=\".35\"/>"
  'F' -> "<rect width=\"30\" height=\"20\" fill=\"#002395\"/><rect x=\"10\" width=\"10\" height=\"20\" fill=\"#fff\"/><rect x=\"20\" width=\"10\" height=\"20\" fill=\"#ed2939\"/>"
  'E' -> "<rect width=\"30\" height=\"20\" fill=\"#aa151b\"/><rect y=\"5\" width=\"30\" height=\"10\" fill=\"#f1bf00\"/>"
  't' -> "<rect width=\"30\" height=\"20\" fill=\"#da291c\"/><rect width=\"12\" height=\"20\" fill=\"#046a38\"/><circle cx=\"12\" cy=\"10\" r=\"3.4\" fill=\"#ffe900\"/><circle cx=\"12\" cy=\"10\" r=\"2\" fill=\"#da291c\"/>"
  'L' -> "<rect width=\"30\" height=\"20\" fill=\"#009246\"/><rect x=\"10\" width=\"10\" height=\"20\" fill=\"#fff\"/><rect x=\"20\" width=\"10\" height=\"20\" fill=\"#ce2b37\"/>"
  'x' -> "<rect width=\"30\" height=\"20\" fill=\"#0c4076\"/><rect y=\"6.67\" width=\"30\" height=\"6.67\" fill=\"#fff\"/><rect y=\"13.33\" width=\"30\" height=\"6.67\" fill=\"#c6363c\"/>"
  'n' -> "<rect width=\"30\" height=\"20\" fill=\"#ba0c2f\"/><rect x=\"8\" width=\"5\" height=\"20\" fill=\"#fff\"/><rect y=\"7.5\" width=\"30\" height=\"5\" fill=\"#fff\"/><rect x=\"9.25\" width=\"2.5\" height=\"20\" fill=\"#00205b\"/><rect y=\"8.75\" width=\"30\" height=\"2.5\" fill=\"#00205b\"/>"
  'S' -> "<rect width=\"30\" height=\"20\" fill=\"#006aa7\"/><rect x=\"8\" width=\"5\" height=\"20\" fill=\"#fecc00\"/><rect y=\"7.5\" width=\"30\" height=\"5\" fill=\"#fecc00\"/>"
  'w' -> "<rect width=\"30\" height=\"20\" fill=\"#000\"/><rect y=\"6.67\" width=\"30\" height=\"6.67\" fill=\"#c00000\"/><circle cx=\"15\" cy=\"10\" r=\"6.3\" fill=\"#c00000\"/><rect x=\"13.7\" y=\"5\" width=\"2.6\" height=\"10\" fill=\"#000\"/><rect x=\"10\" y=\"8.7\" width=\"10\" height=\"2.6\" fill=\"#000\"/><path d=\"M10 8.1H13.1V5M16.9 5V8.1H20M10 11.9H13.1V15M16.9 15V11.9H20\" fill=\"none\" stroke=\"#000\" stroke-width=\".35\"/>"
  'K' -> "<rect width=\"30\" height=\"20\" fill=\"#000\"/><rect y=\"10\" width=\"30\" height=\"10\" fill=\"#e00000\"/>"
  'Z' -> "<rect width=\"30\" height=\"20\" fill=\"#fff\"/><rect y=\"6.67\" width=\"30\" height=\"6.67\" fill=\"#0039a6\"/><rect y=\"13.33\" width=\"30\" height=\"6.67\" fill=\"#d52b1e\"/>"
  'Y' -> "<rect width=\"30\" height=\"20\" fill=\"#e30a17\"/><circle cx=\"11\" cy=\"10\" r=\"5\" fill=\"#fff\"/><circle cx=\"12.3\" cy=\"10\" r=\"4\" fill=\"#e30a17\"/><polygon points=\"18.00,7.60 17.41,9.18 15.72,9.26 17.04,10.31 16.59,11.94 18.00,11.01 19.41,11.94 18.96,10.31 20.28,9.26 18.59,9.18\" fill=\"#fff\"/>"
  'J' -> "<rect width=\"30\" height=\"20\" fill=\"#ce1126\"/><rect y=\"6.67\" width=\"30\" height=\"6.67\" fill=\"#fff\"/><rect y=\"13.33\" width=\"30\" height=\"6.67\" fill=\"#000\"/><polygon points=\"12.00,8.20 11.56,9.39 10.29,9.44 11.28,10.23 10.94,11.46 12.00,10.76 13.06,11.46 12.72,10.23 13.71,9.44 12.44,9.39\" fill=\"#007a3d\"/><polygon points=\"18.00,8.20 17.56,9.39 16.29,9.44 17.28,10.23 16.94,11.46 18.00,10.76 19.06,11.46 18.72,10.23 19.71,9.44 18.44,9.39\" fill=\"#007a3d\"/>"
  's' -> "<rect width=\"30\" height=\"20\" fill=\"#006c35\"/><rect x=\"7\" y=\"12\" width=\"16\" height=\"1.3\" fill=\"#fff\"/><rect x=\"8\" y=\"6.5\" width=\"14\" height=\"2.4\" fill=\"#fff\" opacity=\".85\"/>"
  'i' -> "<rect width=\"30\" height=\"20\" fill=\"#000\"/><rect y=\"6.67\" width=\"30\" height=\"6.67\" fill=\"#fff\"/><rect y=\"13.33\" width=\"30\" height=\"6.67\" fill=\"#007a3d\"/><polygon points=\"0,0 11,10 0,20\" fill=\"#ce1126\"/>"
  'r' -> "<rect width=\"30\" height=\"20\" fill=\"#239f40\"/><rect y=\"6.67\" width=\"30\" height=\"6.67\" fill=\"#fff\"/><rect y=\"13.33\" width=\"30\" height=\"6.67\" fill=\"#da0000\"/><circle cx=\"15\" cy=\"10\" r=\"1.6\" fill=\"#da0000\"/>"
  'g' -> "<rect width=\"30\" height=\"20\" fill=\"#c1272d\"/><polygon points=\"15,5.2 16.3,9.2 20.5,9.2 17.1,11.7 18.4,15.7 15,13.2 11.6,15.7 12.9,11.7 9.5,9.2 13.7,9.2\" fill=\"none\" stroke=\"#006233\" stroke-width=\".9\"/>"
  'b' -> "<rect width=\"30\" height=\"20\" fill=\"#009e49\"/><rect x=\"10\" width=\"10\" height=\"20\" fill=\"#fcd116\"/><rect x=\"20\" width=\"10\" height=\"20\" fill=\"#ce1126\"/>"
  'z' -> "<rect width=\"30\" height=\"20\" fill=\"#000\"/><rect y=\"6.67\" width=\"30\" height=\"6.67\" fill=\"#fff\"/><rect y=\"13.33\" width=\"30\" height=\"6.67\" fill=\"#dd0000\"/><circle cx=\"15\" cy=\"10\" r=\"6.3\" fill=\"#fff\"/><g fill=\"#000\"><path d=\"M15 5.6c.9 0 1.5.6 1.5 1.3 0 .5-.3.8-.6 1l.6 3.4c0 1.2-.7 2-1.5 2.4-.8-.4-1.5-1.2-1.5-2.4l.6-3.4c-.3-.2-.6-.5-.6-1 0-.7.6-1.3 1.5-1.3z\"/><path d=\"M13.6 8.4L9.6 6.6 10.4 8.1 9.3 8 10.4 9.3 9.4 9.5 10.8 10.6 10 11 11.6 11.6 11.1 12.2 13.5 11.6z\"/><path d=\"M16.4 8.4L20.4 6.6 19.6 8.1 20.7 8 19.6 9.3 20.6 9.5 19.2 10.6 20 11 18.4 11.6 18.9 12.2 16.5 11.6z\"/><path d=\"M14.1 12.8L13 15.2 14.4 14.4 15 15.6 15.6 14.4 17 15.2 15.9 12.8z\"/></g><path d=\"M14.3 12.4l-1 .9M15.7 12.4l1 .9\" stroke=\"#dd0000\" stroke-width=\".5\"/><circle cx=\"15\" cy=\"9.6\" r=\"1.05\" fill=\"#fff\" stroke=\"#000\" stroke-width=\".25\"/><path d=\"M14 4.2h2l.2.9h-2.4z\" fill=\"#d4a62a\"/>"
  'e' -> "<rect width=\"30\" height=\"20\" fill=\"#078930\"/><rect y=\"6.67\" width=\"30\" height=\"6.67\" fill=\"#fcdd09\"/><rect y=\"13.33\" width=\"30\" height=\"6.67\" fill=\"#da121a\"/><circle cx=\"15\" cy=\"10\" r=\"3.2\" fill=\"#0f47af\"/><polygon points=\"15.00,7.80 14.46,9.25 12.91,9.32 14.12,10.29 13.71,11.78 15.00,10.92 16.29,11.78 15.88,10.29 17.09,9.32 15.54,9.25\" fill=\"#fcdd09\"/>"
  'u' -> "<rect width=\"30\" height=\"20\" fill=\"#ff7f27\"/><rect y=\"6.67\" width=\"30\" height=\"6.67\" fill=\"#fff\"/><rect y=\"13.33\" width=\"30\" height=\"6.67\" fill=\"#0f3fa6\"/>"
  'I' -> "<rect width=\"30\" height=\"20\" fill=\"#ff9933\"/><rect y=\"6.67\" width=\"30\" height=\"6.67\" fill=\"#fff\"/><rect y=\"13.33\" width=\"30\" height=\"6.67\" fill=\"#138808\"/><circle cx=\"15\" cy=\"10\" r=\"2.6\" fill=\"none\" stroke=\"#000080\" stroke-width=\".7\"/><circle cx=\"15\" cy=\"10\" r=\".6\" fill=\"#000080\"/>"
  'c' -> "<rect width=\"30\" height=\"20\" fill=\"#fe0000\"/><rect width=\"15\" height=\"10\" fill=\"#000095\"/><circle cx=\"7.5\" cy=\"5\" r=\"2.6\" fill=\"#fff\"/><circle cx=\"7.5\" cy=\"5\" r=\"1.8\" fill=\"#000095\"/><circle cx=\"7.5\" cy=\"5\" r=\"1.4\" fill=\"#fff\"/>"
  'm' -> "<rect width=\"30\" height=\"20\" fill=\"#f6c81a\"/><rect width=\"10\" height=\"6.67\" fill=\"#000\"/><rect width=\"10\" height=\"5\" fill=\"#fff\"/><rect width=\"10\" height=\"3.33\" fill=\"#0050a0\"/><rect width=\"10\" height=\"1.67\" fill=\"#d52b1e\"/>"
  'j' -> "<rect width=\"30\" height=\"20\" fill=\"#fff\"/><circle cx=\"15\" cy=\"10\" r=\"6\" fill=\"#bc002d\"/>"
  'd' -> "<rect width=\"30\" height=\"20\" fill=\"#a51931\"/><rect y=\"3.33\" width=\"30\" height=\"13.33\" fill=\"#f4f5f8\"/><rect y=\"6.67\" width=\"30\" height=\"6.67\" fill=\"#2d2a4a\"/>"
  'O' -> "<rect width=\"30\" height=\"20\" fill=\"#012169\"/><g transform=\"scale(0.5,0.5)\"><rect width=\"30\" height=\"20\" fill=\"#1f3a7a\"/><path d=\"M0 0L30 20M30 0L0 20\" stroke=\"#fff\" stroke-width=\"4\"/><path d=\"M0 0L30 20M30 0L0 20\" stroke=\"#c8102e\" stroke-width=\"1.4\"/><rect x=\"12\" width=\"6\" height=\"20\" fill=\"#fff\"/><rect y=\"7\" width=\"30\" height=\"6\" fill=\"#fff\"/><rect x=\"13.2\" width=\"3.6\" height=\"20\" fill=\"#c8102e\"/><rect y=\"8.2\" width=\"30\" height=\"3.6\" fill=\"#c8102e\"/></g><polygon points=\"7.50,13.00 6.96,14.45 5.41,14.52 6.62,15.49 6.21,16.98 7.50,16.12 8.79,16.98 8.38,15.49 9.59,14.52 8.04,14.45\" fill=\"#fff\"/><polygon points=\"23.00,2.90 22.73,3.63 21.95,3.66 22.56,4.14 22.35,4.89 23.00,4.46 23.65,4.89 23.44,4.14 24.05,3.66 23.27,3.63\" fill=\"#fff\"/><polygon points=\"20.00,7.90 19.73,8.63 18.95,8.66 19.56,9.14 19.35,9.89 20.00,9.46 20.65,9.89 20.44,9.14 21.05,8.66 20.27,8.63\" fill=\"#fff\"/><polygon points=\"26.00,7.30 25.73,8.03 24.95,8.06 25.56,8.54 25.35,9.29 26.00,8.86 26.65,9.29 26.44,8.54 27.05,8.06 26.27,8.03\" fill=\"#fff\"/><polygon points=\"23.00,14.90 22.73,15.63 21.95,15.66 22.56,16.14 22.35,16.89 23.00,16.46 23.65,16.89 23.44,16.14 24.05,15.66 23.27,15.63\" fill=\"#fff\"/>"
  _   -> "<rect width=\"30\" height=\"20\" fill=\"#777\"/>"
