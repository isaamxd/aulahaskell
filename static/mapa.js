// Zoom e arrastar no mapa. Funciona mudando o "viewBox" do SVG
// (a parte do mapa que aparece na tela). A posição é lembrada entre os
// cliques, mesmo quando o HTMX troca o mapa por um novo.

const MAPA_LARGURA = 1000;
const MAPA_ALTURA  = 440;
const PROPORCAO    = MAPA_ALTURA / MAPA_LARGURA;

// Regiões prontas: [x, y, largura]
const REGIOES = {
  mundo:    [0, 0, 1000],
  americas: [40, 0, 420],
  europa:   [415, 25, 290],
  africa:   [400, 120, 300],
  asia:     [560, 0, 440],
};

let vista = null;          // [x, y, largura] atual, ou null = mundo inteiro
let arrasto = null;        // dados do arrasto em andamento
let ignorarClique = false; // depois de arrastar, o clique não seleciona país

function svgDoMapa() { return document.querySelector('#jogo .mapa-svg'); }

function limitar(v) {
  let [x, y, w] = v;
  w = Math.max(120, Math.min(MAPA_LARGURA, w));
  const h = w * PROPORCAO;
  x = Math.max(0, Math.min(MAPA_LARGURA - w, x));
  y = Math.max(0, Math.min(MAPA_ALTURA - h, y));
  return [x, y, w];
}

function aplicarVista() {
  const svg = svgDoMapa();
  if (!svg) return;
  const [x, y, w] = vista || REGIOES.mundo;
  svg.setAttribute('viewBox', `${x} ${y} ${w} ${w * PROPORCAO}`);
  svg.style.setProperty('--z', String(MAPA_LARGURA / w));
  document.querySelectorAll('#jogo .zoom-btn[data-zoom]').forEach(b => {
    const r = REGIOES[b.dataset.zoom];
    b.classList.toggle('ativo', !!r && vista && r[0] === vista[0] && r[1] === vista[1] && r[2] === vista[2]);
  });
}

// Converte a posição do mouse na tela pra coordenadas do mapa
function pontoNoMapa(svg, clienteX, clienteY) {
  const p = svg.createSVGPoint();
  p.x = clienteX; p.y = clienteY;
  return p.matrixTransform(svg.getScreenCTM().inverse());
}

function zoom(fator, centro) {
  const atual = vista || REGIOES.mundo;
  const [x, y, w] = atual;
  const h = w * PROPORCAO;
  const c = centro || { x: x + w / 2, y: y + h / 2 };
  const novoW = w * fator;
  const novoH = novoW * PROPORCAO;
  vista = limitar([c.x - (c.x - x) * (novoW / w), c.y - (c.y - y) * (novoH / h), novoW]);
  aplicarVista();
}

// Botões de região e de +/−
document.addEventListener('click', (e) => {
  const botao = e.target.closest('#jogo .zoom-btn');
  if (!botao) return;
  const chave = botao.dataset.zoom;
  if (chave === 'mais') zoom(0.7);
  else if (chave === 'menos') zoom(1 / 0.7);
  else if (REGIOES[chave]) { vista = limitar(REGIOES[chave].slice()); aplicarVista(); }
});

// Roda do mouse: zoom no ponto onde está o cursor
document.addEventListener('wheel', (e) => {
  const svg = e.target.closest && e.target.closest('#jogo .mapa-svg');
  if (!svg) return;
  e.preventDefault();
  zoom(e.deltaY > 0 ? 1.15 : 1 / 1.15, pontoNoMapa(svg, e.clientX, e.clientY));
}, { passive: false });

// Arrastar pra mover
document.addEventListener('pointerdown', (e) => {
  const svg = e.target.closest && e.target.closest('#jogo .mapa-svg');
  if (!svg || e.button !== 0) return;
  arrasto = { svg, x0: e.clientX, y0: e.clientY, vista0: (vista || REGIOES.mundo).slice(), moveu: false };
});

document.addEventListener('pointermove', (e) => {
  if (!arrasto) return;
  const dx = e.clientX - arrasto.x0;
  const dy = e.clientY - arrasto.y0;
  if (!arrasto.moveu && Math.hypot(dx, dy) < 5) return;
  arrasto.moveu = true;
  arrasto.svg.classList.add('arrastando');
  const escala = arrasto.vista0[2] / arrasto.svg.clientWidth;   // unidades do mapa por pixel
  vista = limitar([arrasto.vista0[0] - dx * escala, arrasto.vista0[1] - dy * escala, arrasto.vista0[2]]);
  aplicarVista();
});

document.addEventListener('pointerup', () => {
  if (!arrasto) return;
  if (arrasto.moveu) ignorarClique = true;
  arrasto.svg.classList.remove('arrastando');
  arrasto = null;
});

// Depois de arrastar, cancela o clique (senão o país embaixo seria selecionado).
// Roda na fase de "captura", antes do HTMX ver o clique.
document.addEventListener('click', (e) => {
  if (ignorarClique) {
    ignorarClique = false;
    if (e.target.closest && e.target.closest('#jogo .mapa-svg')) {
      e.stopPropagation();
      e.preventDefault();
    }
  }
}, true);

document.addEventListener('DOMContentLoaded', aplicarVista);
document.addEventListener('htmx:afterSwap', aplicarVista);
