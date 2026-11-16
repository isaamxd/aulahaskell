# Guerra Fria 1962

Jogo de estratégia por turnos para navegador, escrito em **Haskell** com
**Servant** e **PostgreSQL**.

**Cenário (história alternativa):** em 1962, numa linha do tempo em que o Eixo
venceu a Segunda Guerra na Europa, os **Estados Unidos** e o **Reich Alemão** são
as duas superpotências nucleares. Um confronto direto destruiria o planeta,
então a disputa é pelo resto do mundo: 42 países e regiões reais num mapa-múndi
com os contornos verdadeiros dos países.

## Como se joga

- Escolha um lado; o computador joga com o outro.
- Cada turno é um **trimestre**, de janeiro de 1962 a outubro de 1966.
- Gaste o orçamento em ações sobre países:

| Ação | Custo | Efeito | Tensão |
|---|---|---|---|
| Influenciar | 1 + estabilidade | +15 de alinhamento a seu favor | 0 |
| Golpe de Estado | 4 | dado > estabilidade: +40; se falhar, −10 | +1 (+2 em país estratégico) |
| Guerra por procuração | 6 | só em países em disputa; 66% de chance de tomar o controle | +2 |
| Corrida espacial | 8 | +1 de renda por trimestre (4 níveis) | 0 |

- Alinhamento vai de −100 (Reich) a +100 (EUA); a partir de ±50 o país é controlado.
- Países controlados dão pontos (estratégicos valem 2) e aumentam a renda.
- **Tensão nuclear:** golpes e guerras aumentam; ela cai 1 por trimestre. Se chegar
  a 5, é guerra nuclear e **quem causou perde**.
- **Jornal do trimestre:** ao fim de cada trimestre o inimigo joga e chega um
  jornal com o que aconteceu no mundo e uma **crise** com duas respostas
  (25 crises, algumas exclusivas de cada lado, como os mísseis em Cuba).
- Vence quem chegar a 36 pontos, ou quem tiver mais pontos no fim de 1966.
- O resultado vai automaticamente para o **histórico** (banco de dados).

### Apresentação

- **Cutscene de abertura** (1939–1962) contando a história alternativa, com o
  mapa se pintando a cada cena; cutscenes de vitória, derrota e guerra nuclear.
- Sons gerados pelo navegador (Web Audio, sem arquivos de áudio), dado
  animado nos golpes, países piscando quando mudam de lado, tela tremendo na
  guerra nuclear.
- Zoom no mapa (roda do mouse, arrastar, atalhos por região).

## Como rodar (GitHub Codespaces)

Dentro da pasta `haskellaula`, rode sempre:

```bash
bash scripts/iniciar.sh
```

Esse comando liga o banco de dados e roda o jogo. Na primeira vez ele também
instala o PostgreSQL. A tabela é criada sozinha quando o servidor liga
(migração automática). Depois abra a porta **8080** (aba *Portas* do VS Code).

Testes automáticos:

```bash
cabal test
```

## Arquitetura

```
app/Main.hs           conecta no banco, roda a migração e liga o servidor (porta 8080)
app/Model.hs          estruturas da API (ToJSON / FromJSON)
app/Db.hs             conexão com o PostgreSQL e migração automática
app/Api/Partidas.hs   API REST de partidas (CRUD completo)
app/Game/Types.hs     tipos do jogo (lados, países, ações, estado)
app/Game/Map.hs       os 42 países e regiões de 1962 e o estado inicial
app/Game/Geografia.hs contornos do mapa (gerado a partir do Natural Earth)
app/Game/Rules.hs     regras, dado pseudoaleatório puro, crises e IA (funções puras)
app/Game/Crises.hs    as 25 crises do jornal (só dados: condições e efeitos)
app/Web/Cenas.hs      roteiro das cutscenes
app/Web/Views.hs      HTML do jogo (Lucid) e mapa SVG
app/Web/Server.hs     junta tudo num servidor Servant (inclui o "hello" da aula)
static/historico.html front-end em JavaScript puro (fetch) consumindo a API
static/efeitos.js     sons, dado e cutscenes (lê os sinais que o Haskell coloca no HTML)
static/mapa.js        zoom e arrastar no mapa
migrations/           SQL de criação da tabela
scripts/iniciar.sh    liga o banco e roda o jogo
test/Spec.hs          testes (Hspec + QuickCheck)
```

- A **lógica do jogo é pura**: cada ação é uma função `GameState -> GameState`
  (ou `Either Text GameState` quando pode falhar), testada com Hspec e QuickCheck.
- Até o **dado é puro**: a sorte vem de uma semente guardada no estado
  (gerador congruencial linear), então a mesma semente sempre dá o mesmo jogo.
- O equilíbrio entre os lados foi ajustado simulando 600 partidas IA contra IA.
- A **tela do jogo** é gerada em Haskell (Lucid) e atualizada com HTMX.
- O **histórico** usa a API JSON a partir de JavaScript com `fetch`.

## Mapa

Os contornos vêm do [Natural Earth](https://www.naturalearthdata.com/) (1:110m,
domínio público). Os países reais foram agrupados nas regiões do jogo e as
fronteiras ajustadas ao cenário (por exemplo, a Rússia europeia virou o Ostland
e a Manchúria virou Manchukuo). O resultado foi projetado (projeção Natural
Earth I) e salvo em `app/Game/Geografia.hs`.

## API de partidas

| Método | Rota | O que faz | Respostas |
|---|---|---|---|
| GET | `/hello` | exemplo da aula | 200 |
| GET | `/api/partidas` | lista todas (ranking) | 200 |
| GET | `/api/partidas/:id` | busca uma partida | 200, 404 |
| POST | `/api/partidas` | cadastra, devolve o ID | 200, 500 |
| PATCH | `/api/partidas/:id/nome` | muda só o comandante | 200, 404 |
| PUT | `/api/partidas/:id` | atualiza a partida inteira | 200, 404 |
| DELETE | `/api/partidas/:id` | apaga | 200, 404 |

Exemplos com `curl` (com o servidor rodando):

```bash
# listar
curl -v localhost:8080/api/partidas

# buscar por ID
curl -v localhost:8080/api/partidas/1

# cadastrar
curl -v -X POST localhost:8080/api/partidas \
  -H "Content-Type: application/json" \
  -d '{"idPartida":0,"comandante":"Teste","nacao":"Estados Unidos","turnos":12,"paises":20,"situacao":"Vitória"}'

# mudar o nome
curl -v -X PATCH localhost:8080/api/partidas/1/nome \
  -H "Content-Type: application/json" \
  -d '{"novoNome":"General Silva"}'

# atualizar tudo
curl -v -X PUT localhost:8080/api/partidas/1 \
  -H "Content-Type: application/json" \
  -d '{"idPartida":1,"comandante":"General Silva","nacao":"Estados Unidos","turnos":20,"paises":25,"situacao":"Vitória"}'

# apagar
curl -v -X DELETE localhost:8080/api/partidas/1
```

## Roadmap

- [x] Mapa-múndi real (Natural Earth) com 42 países e regiões
- [x] Influência, golpes, guerras por procuração e corrida espacial
- [x] Tensão nuclear e condições de vitória
- [x] IA do lado oposto
- [x] API REST com PostgreSQL e histórico de campanhas
- [x] Crises com escolhas (jornal do trimestre)
- [x] Cutscenes, sons e animações
