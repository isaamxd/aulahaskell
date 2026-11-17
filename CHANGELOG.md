# Revision history for haskellaula

## 0.3.0.0 -- 2026-11-17

* Novo modo **Resistência Europeia**: rádio clandestino, sabotagem, levantes e rotas de fuga,
  risco de descoberta, meta de libertar 6 países europeus, 10 crises e finais próprios.
* Tela inicial com dois modos (EUA ou Resistência); o Reich deixa de ser jogável.
* Mapa abre com zoom na Europa no modo Resistência; países libertados em verde.
* Testes novos (Hspec e curl) para o modo Resistência.
* Visual novo de "dossiê escuro": barra de recursos, memorandos, telegramas e janela do país
  que abre ao lado dele no mapa, como no Hearts of Iron IV.
* Mapa no estilo HOI4: relevo, brilho nas bordas, nomes grandes que se ajustam ao zoom e
  dica com bandeira ao passar o mouse.
* Minijogo da Enigma: a cada 3 trimestres chega uma mensagem cifrada do Reich para decifrar
  girando 3 rotores (lógica pura em `Game.Enigma`, com teste QuickCheck).
* Trilha sonora "Legionnaire" (Scott Buckley, CC BY 4.0) e som de digitação sincronizado nas cutscenes.
* Bandeiras dos 42 países (módulo `Web.Bandeiras`), que mudam quando um território é
  libertado ou ocupado pelo Reich.

## 0.2.0.0 -- 2026-11-16

* Jornal do trimestre com 25 crises de escolha (algumas exclusivas de cada lado).
* Cutscene de abertura (1939–1962) e cutscenes de fim de jogo.
* Sons (Web Audio), dado animado, países piscando, guerra nuclear com tela tremendo.
* Zoom e arrastar no mapa; "Como jogar"; aviso antes de uma ação causar guerra nuclear.
* Mensagens com preposições corretas ("no Brasil", "em Cuba").
* Correção: renomear uma partida no histórico não é mais desfeito pelo salvamento automático.

## 0.1.0.0 -- 2026-11-16

* Exemplo da aula: rota `GET /hello` com Servant.
* Jogo Guerra Fria 1962: mapa-múndi real (Natural Earth), 42 países e regiões, influência, golpes,
  guerras por procuração, corrida espacial, tensão nuclear e IA.
* API REST de partidas com PostgreSQL (GET, GET por ID, POST, PATCH, PUT, DELETE).
* Página de histórico em JavaScript (fetch) consumindo a API.
* Migração automática ao ligar o servidor e script `scripts/iniciar.sh`.
* Testes das regras com Hspec e QuickCheck.
