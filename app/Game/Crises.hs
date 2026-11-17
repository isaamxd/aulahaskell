-- | As crises do jornal: todo trimestre acontece uma, e o jogador escolhe
--   entre duas respostas. Tudo aqui é só DADO (nenhuma função): as regras
--   que aplicam as condições e os efeitos ficam em Game.Rules.
--
--   Os efeitos são do ponto de vista do JOGADOR:
--   Influencia 'B' 20 empurra o Brasil 20 pontos pro lado do jogador;
--   Influencia 'B' (-20) empurra pro lado do inimigo.
module Game.Crises
  ( Condicao (..)
  , Efeito (..)
  , Opcao (..)
  , Crise (..)
  , crises
  , buscarCrise
  ) where

import Data.Text (Text)
import Game.Types

-- | Quando a crise pode aparecer.
data Condicao
  = SoPara Lado          -- ^ só pra quem joga com este lado
  | SoNoModo Modo        -- ^ crises com SoNoModo Resistencia só aparecem nesse modo
  | APartirDoTurno Int
  | AteOTurno Int
  | NaoEhDoJogador Char  -- ^ o país ainda não está sob controle do jogador
  | TensaoMinima Int
  deriving (Eq, Show)

-- | O que uma escolha provoca.
data Efeito
  = Influencia Char Int    -- ^ positivo = a favor do jogador
  | Tensao Int
  | Orcamento Int
  | Estabilidade Char Int
  | Espacial               -- ^ avança um nível na corrida espacial (ou rota de fuga)
  | Refugiados Int         -- ^ pessoas salvas (modo Resistência)
  deriving (Eq, Show)

data Opcao = Opcao
  { opcaoRotulo   :: Text
  , opcaoEfeitos  :: [Efeito]
  , opcaoDesfecho :: Text   -- ^ a frase que vai pros relatórios
  } deriving (Eq, Show)

data Crise = Crise
  { criseId        :: Int
  , criseManchete  :: Text
  , criseTexto     :: Text
  , criseCondicoes :: [Condicao]
  , criseA         :: Opcao
  , criseB         :: Opcao
  } deriving (Eq, Show)

buscarCrise :: Int -> Maybe Crise
buscarCrise i = lookup i [ (criseId c, c) | c <- crises ]

crises :: [Crise]
crises =
  -- ------------------------------------------------------------------------
  -- Para os dois lados
  [ Crise 1 "Copa do Mundo no Chile"
      "O Chile sedia a Copa do Mundo. Patrocinar a transmissão de rádio pode render prestígio em toda a América do Sul."
      [AteOTurno 3]
      (Opcao "Patrocinar a transmissão" [Orcamento (-3), Influencia 'h' 10, Influencia 'B' 10, Influencia 'A' 5]
         "Milhões ouvem a Copa com o seu nome na abertura.")
      (Opcao "Ignorar o futebol" [Orcamento 2, Influencia 'h' (-5)]
         "Você economiza, mas o continente nota a sua ausência.")

  , Crise 2 "O xá ameaça nacionalizar o petróleo"
      "O governo do Irã quer o controle total do seu petróleo e procura um parceiro estrangeiro."
      [NaoEhDoJogador 'r']
      (Opcao "Oferecer um acordo generoso" [Orcamento (-4), Influencia 'r' 25]
         "O xá aceita o acordo e se aproxima de você.")
      (Opcao "Apoiar a oposição nas ruas" [Influencia 'r' 15, Estabilidade 'r' (-1), Tensao 1]
         "Protestos tomam Teerã; o país fica mais instável.")

  , Crise 3 "A monção falha: fome na Índia"
      "As chuvas não vieram e milhões passam fome. Nova Délhi pede ajuda às duas potências."
      []
      (Opcao "Enviar navios de trigo" [Orcamento (-5), Influencia 'I' 25]
         "Seus navios chegam a Bombaim sob aplausos.")
      (Opcao "Vender o trigo a preço de mercado" [Orcamento 3, Influencia 'I' (-10)]
         "Você lucra, mas a Índia não esquece.")

  , Crise 4 "Militares conspiram em Buenos Aires"
      "Oficiais argentinos planejam derrubar o governo e pedem discretamente a sua bênção."
      [NaoEhDoJogador 'A']
      (Opcao "Apoiar os militares" [Influencia 'A' 30, Estabilidade 'A' (-1), Tensao 1]
         "Tanques cercam a Casa Rosada; o novo governo é seu aliado.")
      (Opcao "Defender a constituição" [Influencia 'A' 10, Estabilidade 'A' 1]
         "O golpe é abortado e a democracia argentina sai fortalecida.")

  , Crise 5 "Guerra de independência na Argélia"
      "Rebeldes argelinos lutam contra o Estado Francês. Os dois lados do conflito pedem armas."
      [NaoEhDoJogador 'g']
      (Opcao "Armar os rebeldes" [Influencia 'g' 25, Influencia 'F' (-10), Tensao 1]
         "Com armas novas, os rebeldes tomam o interior do país.")
      (Opcao "Ficar de fora" [Tensao (-1)]
         "Você mantém distância; a guerra continua sem você.")

  , Crise 6 "Cientista inimigo pede asilo"
      "Um engenheiro do programa de foguetes inimigo aparece na sua embaixada em Estocolmo."
      []
      (Opcao "Conceder asilo" [Espacial, Tensao 1]
         "Os segredos do engenheiro aceleram o seu programa espacial.")
      (Opcao "Devolvê-lo discretamente" [Tensao (-1), Orcamento 2]
         "O inimigo agradece em silêncio. A tensão diminui.")

  , Crise 7 "Eleições no Brasil"
      "O Brasil vai às urnas. Os dois principais candidatos procuram financiamento estrangeiro."
      [NaoEhDoJogador 'B']
      (Opcao "Financiar o favorito" [Orcamento (-4), Influencia 'B' 25]
         "O seu candidato vence e promete uma aliança.")
      (Opcao "Respeitar a neutralidade brasileira" [Influencia 'B' 5, Estabilidade 'B' 1]
         "Brasília elogia o seu respeito à soberania do país.")

  , Crise 8 "Canal de Suez fechado"
      "O Cairo fecha o Canal de Suez aos navios do seu bloco. O comércio com a Ásia para."
      [NaoEhDoJogador 'J']
      (Opcao "Negociar a reabertura" [Orcamento (-4), Influencia 'J' 15]
         "Depois de semanas de conversa, o canal reabre.")
      (Opcao "Mandar a frota" [Influencia 'J' 10, Tensao 2]
         "O canal reabre sob a mira dos seus canhões; o mundo prende a respiração.")

  , Crise 9 "Milícias tomam as minas do Congo"
      "A Mittelafrika se fragmenta e milícias ocupam as minas de urânio de Katanga."
      [NaoEhDoJogador 'z']
      (Opcao "Mandar mercenários" [Influencia 'z' 25, Tensao 1]
         "Os mercenários retomam as minas, e o urânio agora é seu.")
      (Opcao "Mandar médicos e engenheiros" [Orcamento (-3), Influencia 'z' 15, Estabilidade 'z' 1]
         "Hospitais e estradas conquistam a população local.")

  , Crise 10 "Ofensiva da corrida espacial"
      "Seus cientistas garantem que conseguem chegar à frente do inimigo, desde que recebam um orçamento extra."
      [APartirDoTurno 6]
      (Opcao "Aprovar o orçamento" [Orcamento (-6), Espacial]
         "O novo foguete decola, e o mundo inteiro assiste.")
      (Opcao "Priorizar a economia" [Orcamento 4]
         "O dinheiro fica em casa; o programa espacial espera.")

  , Crise 11 "Revolta na Indonésia"
      "Um general rebelde marcha sobre Jacarta e promete se aliar a quem o apoiar."
      [NaoEhDoJogador '1']
      (Opcao "Apoiar o general" [Influencia '1' 25, Tensao 1]
         "O general toma o palácio presidencial.")
      (Opcao "Mediar a paz" [Influencia '1' 10, Estabilidade '1' 1]
         "Um acordo de paz é assinado sob a sua mediação.")

  , Crise 12 "Cúpula de paz em Genebra"
      "A Suíça convida as duas potências para negociar a redução das armas nucleares."
      [TensaoMinima 2]
      (Opcao "Comparecer e fazer concessões" [Tensao (-2), Orcamento (-2)]
         "O aperto de mãos em Genebra alivia o mundo inteiro.")
      (Opcao "Boicotar a cúpula" [Tensao 1, Orcamento 2]
         "Sua cadeira vazia em Genebra é notícia no mundo todo.")

  , Crise 13 "Avião espião cai na Turquia"
      "Um avião de reconhecimento seu cai em território turco. Ancara exige explicações."
      [NaoEhDoJogador 'Y']
      (Opcao "Admitir e pedir desculpas" [Tensao (-1), Influencia 'Y' (-10)]
         "A Turquia aceita as desculpas, mas fica desconfiada.")
      (Opcao "Negar tudo" [Tensao 1, Influencia 'Y' 5]
         "Ancara finge acreditar na história do balão meteorológico.")

  , Crise 14 "Crise do petróleo"
      "O preço do petróleo dispara. A Arábia e a Venezuela disputam quem vai abastecer o seu bloco."
      []
      (Opcao "Fechar contrato com a Arábia" [Orcamento (-3), Influencia 's' 20]
         "Os petroleiros árabes passam a navegar com a sua bandeira.")
      (Opcao "Fechar contrato com a Venezuela" [Orcamento (-3), Influencia 'v' 20]
         "Caracas comemora o maior contrato da sua história.")

  , Crise 15 "Revolução na Indochina"
      "Guerrilheiros avançam sobre Saigon e Bangcoc. A região pode mudar de lado."
      [NaoEhDoJogador 'd']
      (Opcao "Enviar conselheiros militares" [Influencia 'd' 20, Tensao 1]
         "Seus conselheiros reorganizam o exército local.")
      (Opcao "Enviar ajuda humanitária" [Orcamento (-2), Influencia 'd' 10]
         "Arroz e remédios chegam às vilas devastadas pela guerra.")

  -- ------------------------------------------------------------------------
  -- Só para os Estados Unidos
  , Crise 101 "MÍSSEIS DO REICH EM CUBA!"
      "Aviões espiões fotografam bases de mísseis do Reich em Cuba, a 150 km da Flórida."
      [SoPara EUA, NaoEhDoJogador 'k']
      (Opcao "Bloqueio naval da ilha" [Influencia 'k' 25, Tensao 2]
         "Os navios do Reich dão meia-volta no último minuto.")
      (Opcao "Negociar em segredo com Berlim" [Tensao (-1), Influencia 'k' (-20)]
         "Os mísseis saem de Cuba, mas a ilha fica do lado de Berlim.")

  , Crise 102 "Aliança para o Progresso"
      "Seus economistas propõem um grande plano de investimentos na América Latina."
      [SoPara EUA]
      (Opcao "Aprovar o plano" [Orcamento (-6), Influencia 'o' 10, Influencia 'p' 10, Influencia 'v' 10, Influencia 'B' 5]
         "Estradas, escolas e fábricas se espalham pelo continente.")
      (Opcao "Arquivar o plano" [Orcamento 2]
         "O plano vai pra gaveta do Congresso.")

  , Crise 103 "Marchas pelos direitos civis"
      "Milhares marcham em Washington por direitos iguais. O mundo inteiro está assistindo."
      [SoPara EUA, APartirDoTurno 3]
      (Opcao "Aprovar a Lei dos Direitos Civis" [Orcamento (-2), Influencia 'I' 10, Influencia 'b' 10, Influencia 'J' 5]
         "A lei é aprovada; países recém-independentes elogiam Washington.")
      (Opcao "Reprimir as marchas" [Influencia 'b' (-15), Influencia 'I' (-10)]
         "As imagens da repressão correm o mundo e mancham a sua imagem.")

  , Crise 104 "A resistência francesa pede armas"
      "A resistência contra o Estado Francês pede armas e rádios para um levante em Paris."
      [SoPara EUA, NaoEhDoJogador 'F']
      (Opcao "Enviar armas" [Influencia 'F' 20, Tensao 2]
         "Paris amanhece com barricadas nas ruas.")
      (Opcao "Apenas transmitir rádio" [Influencia 'F' 8]
         "A Voz da América chega a milhões de lares franceses.")

  , Crise 105 "Tóquio sonda um acordo"
      "Cansado da tutela de Berlim, o Japão propõe em segredo um acordo comercial."
      [SoPara EUA, APartirDoTurno 6, NaoEhDoJogador 'j']
      (Opcao "Aceitar o acordo" [Orcamento (-3), Influencia 'j' 20, Tensao 1]
         "Navios japoneses começam a chegar à Califórnia.")
      (Opcao "Recusar" [Orcamento 1]
         "Tóquio continua alinhado a Berlim.")

  -- ------------------------------------------------------------------------
  -- Só para o Reich
  , Crise 201 "Levante no Ostland"
      "Partisans russos explodem ferrovias e atacam guarnições no Ostland."
      [SoPara Reich]
      (Opcao "Mandar mais tropas" [Orcamento (-4), Influencia 'w' 15, Estabilidade 'w' 1]
         "As ferrovias voltam a funcionar, a um custo alto.")
      (Opcao "Prometer autonomia local" [Influencia 'w' (-15), Estabilidade 'w' 2, Tensao (-1)]
         "Os combates diminuem, mas o controle de Berlim enfraquece.")

  , Crise 202 "Greve geral na Itália"
      "Operários paralisam as fábricas de Milão e Turim. Roma pede ajuda a Berlim."
      [SoPara Reich]
      (Opcao "Enviar ajuda econômica" [Orcamento (-4), Influencia 'L' 10]
         "Os salários sobem e as fábricas voltam a funcionar.")
      (Opcao "Exigir que Roma reprima a greve" [Influencia 'L' (-15), Estabilidade 'L' (-1)]
         "A greve termina, mas os italianos culpam Berlim.")

  , Crise 203 "Uma base no Atlântico Sul"
      "A Argentina aceita discutir uma base naval do Reich na Patagônia."
      [SoPara Reich, NaoEhDoJogador 'A']
      (Opcao "Construir a base" [Orcamento (-5), Influencia 'A' 25, Tensao 2]
         "A base fica pronta, e Washington coloca a frota em alerta.")
      (Opcao "Recuar" [Tensao (-1)]
         "O projeto é engavetado sem alarde.")

  , Crise 204 "O programa espacial de Peenemünde"
      "Os engenheiros de Peenemünde prometem um voo tripulado, se receberem verba dobrada."
      [SoPara Reich]
      (Opcao "Dobrar o orçamento" [Orcamento (-6), Espacial]
         "Um novo foguete decola de Peenemünde.")
      (Opcao "Desviar a verba para o exército" [Orcamento 4]
         "O dinheiro vai para os quartéis.")

  , Crise 205 "Fome na Ucrânia"
      "As colheitas falham e a Ucrânia ocupada passa fome."
      [SoPara Reich]
      (Opcao "Enviar grãos" [Orcamento (-4), Influencia 'K' 15, Estabilidade 'K' 1]
         "Os grãos chegam e a revolta perde força.")
      (Opcao "Manter as requisições" [Orcamento 3, Influencia 'K' (-20), Estabilidade 'K' (-1)]
         "Os celeiros do Reich ficam cheios; a Ucrânia, revoltada.")

  -- ------------------------------------------------------------------------
  -- Modo Resistência (os efeitos são a favor da Resistência)
  , Crise 301 "Londres pede informações"
      "Os aliados querem saber onde ficam as fábricas de armas do Reich na França."
      [SoNoModo Resistencia, NaoEhDoJogador 'F']
      (Opcao "Mandar agentes fotografar as fábricas" [Influencia 'F' 15, Tensao 1]
         "As fotos chegam a Londres, e os bombardeiros acertam o alvo.")
      (Opcao "Recusar: é arriscado demais" [Tensao (-1)]
         "A rede fica em silêncio por um tempo.")

  , Crise 302 "Um mensageiro foi capturado"
      "A Gestapo prendeu um mensageiro em Lyon. Ele conhece nomes e endereços da rede."
      [SoNoModo Resistencia]
      (Opcao "Mudar todos os esconderijos" [Orcamento (-4), Tensao (-1)]
         "A rede se muda a tempo; a Gestapo encontra casas vazias.")
      (Opcao "Tentar resgatá-lo da prisão" [Tensao 2, Influencia 'F' 10]
         "O resgate dá certo, e Paris inteira comenta a fuga.")

  , Crise 303 "Paraquedas sobre a Noruega"
      "Aviões aliados podem lançar armas para a resistência norueguesa, se alguém marcar a zona de pouso."
      [SoNoModo Resistencia, NaoEhDoJogador 'n']
      (Opcao "Marcar a zona com fogueiras" [Influencia 'n' 20, Tensao 1]
         "Os contêineres caem no lugar certo, cheios de armas e rádios.")
      (Opcao "Adiar o lançamento" [Orcamento 2]
         "As armas ficam guardadas em Londres por enquanto.")

  , Crise 304 "Greve em Turim"
      "Operários de Turim cruzam os braços contra a ocupação e pedem apoio."
      [SoNoModo Resistencia, NaoEhDoJogador 'L']
      (Opcao "Sustentar a greve com dinheiro" [Orcamento (-3), Influencia 'L' 20]
         "A greve se espalha por toda a Itália do norte.")
      (Opcao "Esperar um momento melhor" [Estabilidade 'L' 1]
         "A greve perde força, e as fábricas voltam a produzir para o Reich.")

  , Crise 305 "Partisans nas montanhas dos Bálcãs"
      "Grupos de partisans controlam vales inteiros e pedem armas."
      [SoNoModo Resistencia, NaoEhDoJogador 'x']
      (Opcao "Enviar armas" [Influencia 'x' 25, Tensao 1]
         "Com armas novas, os partisans libertam várias cidades.")
      (Opcao "Enviar só mantimentos" [Orcamento (-2), Influencia 'x' 10]
         "Os mantimentos ajudam os vilarejos a resistir ao inverno.")

  , Crise 306 "Famílias na fronteira suíça"
      "Centenas de famílias perseguidas tentam cruzar a fronteira com a Suíça e precisam de documentos falsos."
      [SoNoModo Resistencia]
      (Opcao "Falsificar documentos para todos" [Orcamento (-3), Tensao 1, Refugiados 800]
         "Todas as famílias cruzam a fronteira em segurança.")
      (Opcao "Levar primeiro as crianças" [Orcamento (-1), Refugiados 300]
         "As crianças chegam à Suíça; os adultos esperam a próxima chance.")

  , Crise 307 "Madri hesita"
      "O governo espanhol pensa em se afastar de Berlim, se receber garantias dos aliados."
      [SoNoModo Resistencia, NaoEhDoJogador 'E']
      (Opcao "Prometer apoio aliado" [Orcamento (-4), Influencia 'E' 20]
         "Madri esfria as relações com Berlim.")
      (Opcao "Não confiar em Madri" [Influencia 'E' (-5)]
         "A Espanha continua alinhada ao Reich.")

  , Crise 308 "Partisans na Ucrânia"
      "Partisans ucranianos querem coordenar com vocês um grande ataque às linhas de trem."
      [SoNoModo Resistencia, NaoEhDoJogador 'K']
      (Opcao "Coordenar o ataque" [Influencia 'K' 25, Tensao 2]
         "Dezenas de trens param ao mesmo tempo em toda a Ucrânia.")
      (Opcao "Pedir que esperem" [Influencia 'K' 5]
         "Os partisans aguardam, mas continuam em contato.")

  , Crise 309 "Uma máquina Enigma capturada!"
      "Marinheiros aliados capturaram uma máquina de cifrar alemã num submarino afundado."
      [SoNoModo Resistencia]
      (Opcao "Mandar a máquina para Londres" [Orcamento 3, Tensao (-1)]
         "Os matemáticos de Londres começam a ler as mensagens do Reich.")
      (Opcao "Usar a máquina aqui mesmo" [Influencia 'F' 10, Tensao 1]
         "A rede intercepta ordens da Gestapo e salva vários agentes.")

  , Crise 310 "A Suécia neutra"
      "Estocolmo pode abrigar agentes da Resistência, mas teme a reação de Berlim."
      [SoNoModo Resistencia, NaoEhDoJogador 'S']
      (Opcao "Pedir abrigo para a rede" [Influencia 'S' 20, Tensao 1]
         "A Suécia vira o refúgio secreto da Resistência no norte.")
      (Opcao "Respeitar a neutralidade sueca" [Tensao (-1), Refugiados 400]
         "A Suécia continua neutra, mas recebe refugiados em silêncio.")
  ]
