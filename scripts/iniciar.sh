#!/usr/bin/env bash
# Liga o banco e roda o jogo. Use SEMPRE este comando:
#   bash scripts/iniciar.sh
# Na primeira vez ele também instala o PostgreSQL.
set -e
cd "$(dirname "$0")/.."

# Se já somos root (administrador), não precisamos de sudo.
if [ "$(id -u)" -eq 0 ]; then
  SUDO=""
else
  SUDO="sudo"
fi

# Roda um comando como o usuário "postgres" do banco.
como_postgres() {
  if [ "$(id -u)" -eq 0 ]; then
    (cd /tmp && runuser -u postgres -- "$@")
  else
    (cd /tmp && sudo -u postgres "$@")
  fi
}

if ! command -v psql > /dev/null 2>&1 || ! dpkg -s libpq-dev > /dev/null 2>&1; then
  echo ">> Primeira vez: instalando o PostgreSQL (uns 2 minutos)..."
  $SUDO apt-get update -y || true
  DEBIAN_FRONTEND=noninteractive $SUDO apt-get install -y postgresql libpq-dev pkg-config
fi

echo ">> Ligando o banco de dados..."
if command -v service > /dev/null 2>&1; then
  $SUDO service postgresql start > /dev/null
else
  versao="$(ls /etc/postgresql | sort -n | tail -1)"
  $SUDO pg_ctlcluster "$versao" main start || true
fi
sleep 2

# Senha e banco "fronteiras" (só cria na primeira vez)
como_postgres psql -qc "ALTER USER postgres PASSWORD 'postgres';" > /dev/null
if ! como_postgres psql -lqt | cut -d '|' -f 1 | grep -qw fronteiras; then
  como_postgres createdb fronteiras
fi

echo ">> Compilando e iniciando o jogo (porta 8080)..."
cabal run haskellaula
