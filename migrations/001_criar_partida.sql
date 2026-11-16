-- Migracao: tabela de partidas salvas (historico / ranking)
CREATE TABLE IF NOT EXISTS partida (
    id          SERIAL PRIMARY KEY,
    comandante  TEXT    NOT NULL,
    nacao       TEXT    NOT NULL,
    turnos      INTEGER NOT NULL,
    paises      INTEGER NOT NULL,
    situacao    TEXT    NOT NULL
);

-- Ajuste para quem criou a tabela na versao anterior (coluna "regioes")
ALTER TABLE partida ADD COLUMN IF NOT EXISTS paises INTEGER NOT NULL DEFAULT 0;
ALTER TABLE partida DROP COLUMN IF EXISTS regioes;
