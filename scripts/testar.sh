#!/usr/bin/env bash
# Testa todas as rotas do servidor, como um "robô" usando o jogo.
# O servidor precisa estar rodando (cabal run haskellaula) em outro terminal.
#   bash scripts/testar.sh
# Atenção: o teste começa uma partida nova (a partida aberta é descartada).

URL="${1:-http://localhost:8080}"
ok=0
falhou=0
CORPO=/tmp/resposta-teste.txt

# Faz uma requisição. Uso: req METODO CAMINHO [JSON]
req() {
  if [ -n "$3" ]; then
    STATUS=$(curl -s -o "$CORPO" -w '%{http_code}' -X "$1" -H 'Content-Type: application/json' -d "$3" "$URL$2")
  else
    STATUS=$(curl -s -o "$CORPO" -w '%{http_code}' -X "$1" "$URL$2")
  fi
}

# Confere a última resposta. Uso: teste "descrição" STATUS_ESPERADO ["texto que deve aparecer"]
teste() {
  if [ "$STATUS" = "$2" ] && { [ -z "$3" ] || grep -qF -- "$3" "$CORPO"; }; then
    printf '\033[32m  ✔\033[0m %s\n' "$1"
    ok=$((ok + 1))
  else
    printf '\033[31m  ✘ %s\033[0m  (esperado %s' "$1" "$2"
    [ -n "$3" ] && printf ' com "%s"' "$3"
    printf ', veio %s)\n' "$STATUS"
    falhou=$((falhou + 1))
  fi
}

if ! curl -s -o /dev/null "$URL/hello"; then
  echo "O servidor não está respondendo em $URL."
  echo "Rode 'cabal run haskellaula' em outro terminal e tente de novo."
  exit 1
fi

echo "Testando $URL"
echo
echo "Exemplo da aula e arquivos"
req GET /hello;                teste "GET /hello responde Hello, world!" 200 "Hello, world!"
req GET /;                     teste "GET / abre a página do jogo" 200 "Guerra Fria 1962"
req GET /style.css;            teste "GET /style.css (visual)" 200
req GET /historico.html;       teste "GET /historico.html (página com JavaScript)" 200 "listarPartidas"

echo
echo "API de partidas (CRUD)"
JSON='{"idPartida":0,"comandante":"Teste automatico","nacao":"Estados Unidos","turnos":12,"paises":15,"situacao":"Vitória"}'
req POST /api/partidas "$JSON"; teste "POST cadastra e devolve o ID" 200 '"resultado"'
ID=$(grep -o '"resultado":[0-9]*' "$CORPO" | grep -o '[0-9]*$')
req GET /api/partidas;          teste "GET lista todas as partidas" 200 '"partidas"'
req GET "/api/partidas/$ID";    teste "GET por ID encontra a partida #$ID" 200 "Teste automatico"
req PATCH "/api/partidas/$ID/nome" '{"novoNome":"Renomeado"}'
                                teste "PATCH muda só o nome" 200
req GET "/api/partidas/$ID";    teste "  ...e o nome mudou mesmo" 200 "Renomeado"
JSON2='{"idPartida":0,"comandante":"Atualizado","nacao":"Reich Alemão","turnos":20,"paises":30,"situacao":"Derrota"}'
req PUT "/api/partidas/$ID" "$JSON2"; teste "PUT atualiza a partida inteira" 200
req GET "/api/partidas/$ID";    teste "  ...e todos os campos mudaram" 200 '"turnos":20'
req DELETE "/api/partidas/$ID"; teste "DELETE apaga a partida" 200
req GET "/api/partidas/$ID";    teste "  ...e depois dela não existe mais (404)" 404
req GET /api/partidas/999999;   teste "GET de ID inexistente dá 404" 404
req PATCH /api/partidas/999999/nome '{"novoNome":"x"}'; teste "PATCH de ID inexistente dá 404" 404
req PUT /api/partidas/999999 "$JSON"; teste "PUT de ID inexistente dá 404" 404
req DELETE /api/partidas/999999; teste "DELETE de ID inexistente dá 404" 404
# o Servant não acha rota que aceite "abc" como Int, então responde 404
req GET /api/partidas/abc;      teste "ID que não é número é recusado (404)" 404

echo
echo "Jogo"
req POST /novo/eua;             teste "escolher os EUA começa a partida" 200 "Estados Unidos"
req POST /novo/marte;           teste "lado inválido dá 404" 404
req GET /pais/B;                teste "clicar no Brasil mostra os detalhes" 200 "Brasil"
req GET /pais/xyz;              teste "país inválido dá 404" 404
req POST /influenciar/B;        teste "influenciar o Brasil" 200 "amplia sua influência no Brasil"
req POST /influenciar/D;        teste "não deixa agir dentro do Reich" 200 "Não é possível agir dentro de uma superpotência"
req POST /golpe/p;              teste "golpe de Estado no Peru (com dado)" 200 "Golpe apoiado por Washington"
req POST /espacial;             teste "não deixa gastar sem orçamento" 200 "Orçamento insuficiente"
req POST /turno;                teste "encerrar o trimestre avança pra Abril de 1962" 200 "Abril de 1962"
                                teste "  ...e chega o jornal do trimestre" 200 "Gazeta Internacional"
if grep -qF 'data-hx-post="/crise/a"' "$CORPO"; then
  req POST /jornal/fechar;      teste "  ...não dá pra fechar o jornal sem decidir a crise" 200 "Decida a crise"
  req POST /crise/x;            teste "  ...opção de crise inválida dá 404" 404
  req POST /crise/a;            teste "  ...decidir a crise fecha o jornal" 200 "Abril de 1962"
  if grep -qF 'jornal-fundo' "$CORPO"; then teste "  ...(o jornal continuou aberto)" 999; fi
else
  req POST /jornal/fechar;      teste "  ...fechar o jornal" 200 "Abril de 1962"
fi
req POST /turno;                teste "segundo trimestre (Julho de 1962)" 200 "Julho de 1962"
req POST /novo/eua
req POST /espacial;             teste "corrida espacial: primeiro satélite" 200 "primeiro satélite"
STATUS=$(curl -s -o "$CORPO" -w '%{http_code}' -X POST -d 'nomeComandante=Robo de Teste' "$URL/salvar")
                                teste "salvar a campanha no banco" 200 "Partida salva no histórico"
SALVA=$(grep -o 'partida #[0-9]*' "$CORPO" | head -1 | grep -o '[0-9]*$')
req GET "/api/partidas/$SALVA"; teste "  ...e ela aparece na API" 200 "Robo de Teste"
req DELETE "/api/partidas/$SALVA"; teste "  ...e é apagada no fim do teste" 200
echo
echo "Modo Resistência"
req POST /novo/resistencia;     teste "escolher a Resistência começa a partida" 200 "Resistência Europeia"
                                teste "  ...com o placar de países libertados" 200 "Países libertados"
req POST /influenciar/B;        teste "não deixa agir fora da Europa" 200 "só atua na Europa"
req POST /influenciar/F;        teste "rádio clandestino na França" 200 "rádio clandestino"
req POST /espacial;             teste "abrir uma rota de fuga salva refugiados" 200 "refugiados chegam em segurança"
req POST /novo/reich;           teste "o Reich não é mais jogável (404)" 404
req POST /reiniciar;            teste "nova partida volta pra escolha de lado" 200 "Assumir o comando"
echo
total=$((ok + falhou))
if [ "$falhou" -eq 0 ]; then
  printf '\033[32m%d de %d testes passaram.\033[0m\n' "$ok" "$total"
else
  printf '\033[31m%d de %d testes falharam.\033[0m\n' "$falhou" "$total"
  exit 1
fi
