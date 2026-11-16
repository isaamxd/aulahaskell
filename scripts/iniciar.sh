#!/usr/bin/env bash
# Liga o banco e roda o jogo. Use SEMPRE este comando:
#   bash scripts/iniciar.sh
# Na primeira vez ele também instala o PostgreSQL.
set -e
cd "$(dirname "$0")/.."

if ! command -v psql >/dev/null 2>&1 || ! dpkg -s libpq-dev >/dev/null 2>&1; then
  echo ">> Primeira vez: instalando o PostgreSQL (uns 2 minutos)..."
  sudo apt-get update -y || true
  sudo apt-get install -y postgresql libpq-dev pkg-config
fi

echo ">> Ligando o banco de dados..."
sudo service postgresql start > /dev/null

# Senha e banco "fronteiras" (só cria na primeira vez)
(cd /tmp && sudo -u postgres psql -qc "ALTER USER postgres PASSWORD 'postgres';" > /dev/null)
if ! (cd /tmp && sudo -u postgres psql -lqt) | cut -d '|' -f 1 | grep -qw fronteiras; then
  (cd /tmp && sudo -u postgres createdb fronteiras)
fi

echo ">> Compilando e iniciando o jogo (porta 8080)..."
cabal run haskellaula
