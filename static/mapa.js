// Mapa interativo: zoom e arrastar, nomes dos países que mudam de tamanho
// com o zoom, a dica ao passar o mouse e a janela do país (estilo HOI4).
//
// O zoom muda o "viewBox" do SVG (a parte do mapa que aparece). A posição é
// lembrada entre os cliques, mesmo quando o HTMX troca o mapa por um novo.

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
let animacao = null;

function svgDoMapa() { return document.querySelector('#jogo .mapa-svg'); }
function vistaAtual() { return vista || REGIOES.mundo; }

function limitar(v) {
  let [x, y, w] = v;
  w = Math.max(110, Math.min(MAPA_LARGURA, w));
  const h = w * PROPORCAO;
  x = Math.max(0, Math.min(MAPA_LARGURA - w, x));
  y = Math.max(0, Math.min(MAPA_ALTURA - h, y));
  return [x, y, w];
}

// ---------------------------------------------------------------------------
// Tamanho dos nomes: o maior pedaço de cada país (sem as ilhas) define o
// espaço disponível. Calculado uma vez só, a partir do desenho do país.
const caixas = {};
function caixaPrincipal(codigo) {
  if (caixas[codigo]) return caixas[codigo];
  const forma = document.getElementById('f-' + codigo);
  if (!forma) return null;
  let melhor = null;
  (forma.getAttribute('d').match(/M[^M]*/g) || []).forEach(pedaco => {
    const n = pedaco.match(/-?\d+\.?\d*/g).map(Number);
    let x0 = Infinity, y0 = Infinity, x1 = -Infinity, y1 = -Infinity;
    for (let i = 0; i + 1 < n.length; i += 2) {
      x0 = Math.min(x0, n[i]); x1 = Math.max(x1, n[i]);
      y0 = Math.min(y0, n[i + 1]); y1 = Math.max(y1, n[i + 1]);
    }
    const area = (x1 - x0) * (y1 - y0);
    if (!melhor || area > melhor.area) melhor = { area, w: x1 - x0, h: y1 - y0 };
  });
  caixas[codigo] = melhor;
  return melhor;
}

function ajustarNomes(svg) {
  const z = MAPA_LARGURA / vistaAtual()[2];
  svg.querySelectorAll('.nome-pais').forEach(t => {
    if (!t.dataset.nome) t.dataset.nome = t.textContent;
    const caixa = caixaPrincipal(t.dataset.c);
    if (!caixa) return;
    const nome = t.dataset.nome.toUpperCase();
    let tam = Math.min(26, caixa.w * 1.25 / Math.max(nome.length, 3), caixa.h * 0.5);
    let texto = nome;
    if (tam * z < 5.2) { texto = t.dataset.sig; tam = 5.5 / z; }
    tam = Math.min(tam, 31 / z);
    t.textContent = texto;
    t.setAttribute('font-size', tam.toFixed(2));
    t.setAttribute('letter-spacing', (tam * 0.2).toFixed(2));
    t.setAttribute('stroke-width', (tam * 0.16).toFixed(2));
  });
}

function aplicarVista() {
  const svg = svgDoMapa();
  if (!svg) return;
  const [x, y, w] = vistaAtual();
  svg.setAttribute('viewBox', `${x} ${y} ${w} ${w * PROPORCAO}`);
  svg.style.setProperty('--z', String(MAPA_LARGURA / w));
  document.querySelectorAll('#jogo .zoom-btn[data-zoom]').forEach(b => {
    const r = REGIOES[b.dataset.zoom];
    b.classList.toggle('ativo', !!r && vista && r[0] === vista[0] && r[1] === vista[1] && r[2] === vista[2]);
  });
  ajustarNomes(svg);
  posicionarJanela();
}

// Desliza suavemente até a nova vista
function irPara(destino) {
  destino = limitar(destino);
  cancelAnimationFrame(animacao);
  const passo = () => {
    const atual = vistaAtual();
    let chegou = true;
    vista = atual.map((a, i) => {
      const d = destino[i] - a;
      if (Math.abs(d) > 0.3) chegou = false;
      return a + d * 0.2;
    });
    if (chegou) vista = destino;
    aplicarVista();
    if (!chegou) animacao = requestAnimationFrame(passo);
  };
  animacao = requestAnimationFrame(passo);
}

// Converte a posição do mouse na tela pra coordenadas do mapa
function pontoNoMapa(svg, clienteX, clienteY) {
  const p = svg.createSVGPoint();
  p.x = clienteX; p.y = clienteY;
  return p.matrixTransform(svg.getScreenCTM().inverse());
}

function zoom(fator, centro) {
  const [x, y, w] = vistaAtual();
  const h = w * PROPORCAO;
  const c = centro || { x: x + w / 2, y: y + h / 2 };
  const novoW = w * fator;
  const novoH = novoW * PROPORCAO;
  return limitar([c.x - (c.x - x) * (novoW / w), c.y - (c.y - y) * (novoH / h), novoW]);
}

// Botões de região e de +/−
document.addEventListener('click', (e) => {
  const botao = e.target.closest('#jogo .zoom-btn');
  if (!botao) return;
  const chave = botao.dataset.zoom;
  if (chave === 'mais') irPara(zoom(0.7));
  else if (chave === 'menos') irPara(zoom(1 / 0.7));
  else if (REGIOES[chave]) irPara(REGIOES[chave].slice());
});

// Roda do mouse: zoom no ponto onde está o cursor
document.addEventListener('wheel', (e) => {
  const svg = e.target.closest && e.target.closest('#jogo .mapa-svg');
  if (!svg) return;
  e.preventDefault();
  cancelAnimationFrame(animacao);
  vista = zoom(e.deltaY > 0 ? 1.15 : 1 / 1.15, pontoNoMapa(svg, e.clientX, e.clientY));
  aplicarVista();
}, { passive: false });

// Arrastar pra mover
document.addEventListener('pointerdown', (e) => {
  const svg = e.target.closest && e.target.closest('#jogo .mapa-svg');
  if (!svg || e.button !== 0) return;
  cancelAnimationFrame(animacao);
  arrasto = { svg, x0: e.clientX, y0: e.clientY, vista0: vistaAtual().slice(), moveu: false };
});

document.addEventListener('pointermove', (e) => {
  if (arrasto) {
    const dx = e.clientX - arrasto.x0;
    const dy = e.clientY - arrasto.y0;
    if (!arrasto.moveu && Math.hypot(dx, dy) < 5) return;
    arrasto.moveu = true;
    esconderDica();
    arrasto.svg.classList.add('arrastando');
    const escala = arrasto.vista0[2] / arrasto.svg.clientWidth;   // unidades do mapa por pixel
    vista = limitar([arrasto.vista0[0] - dx * escala, arrasto.vista0[1] - dy * escala, arrasto.vista0[2]]);
    aplicarVista();
    return;
  }
  mostrarDica(e);
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

// ---------------------------------------------------------------------------
// Dica ao passar o mouse: bandeira, nome e situação do país

function esconderDica() {
  const dica = document.getElementById('dica');
  if (dica) dica.classList.remove('visivel');
}

function mostrarDica(e) {
  const dica = document.getElementById('dica');
  if (!dica) return;
  const pais = e.target.closest && e.target.closest('#jogo .pais');
  if (!pais || e.target.closest('#janela')) { esconderDica(); return; }
  if (dica.dataset.c !== pais.dataset.c) {
    dica.dataset.c = pais.dataset.c;
    dica.innerHTML = '';
    const nome = document.createElement('b');
    nome.innerHTML = pais.dataset.bandeira || '';
    nome.append(' ' + pais.dataset.nome);
    const info = document.createElement('span');
    info.textContent = pais.dataset.info;
    dica.append(nome, info);
  }
  dica.style.left = (e.clientX + 16) + 'px';
  dica.style.top = (e.clientY + 14) + 'px';
  dica.classList.add('visivel');
}

// ---------------------------------------------------------------------------
// Janela do país: aparece ao lado do país clicado

function posicionarJanela() {
  const janela = document.getElementById('janela');
  const svg = svgDoMapa();
  const moldura = document.getElementById('moldura');
  if (!janela || !svg || !moldura) return;
  const rotulo = svg.querySelector(`.nome-pais[data-c="${janela.dataset.c}"]`);
  if (!rotulo) return;
  const [vx, vy, vw] = vistaAtual();
  const r = svg.getBoundingClientRect();
  const m = moldura.getBoundingClientRect();
  const px = (Number(rotulo.getAttribute('x')) - vx) / vw * r.width + (r.left - m.left);
  const py = (Number(rotulo.getAttribute('y')) - vy) / (vw * PROPORCAO) * r.height + (r.top - m.top);
  let esquerda = px + 30;
  if (esquerda + janela.offsetWidth + 10 > m.width) esquerda = px - janela.offsetWidth - 30;
  esquerda = Math.max(8, esquerda);
  const topo = Math.max(8, Math.min(m.height - janela.offsetHeight - 8, py - 60));
  janela.style.left = esquerda + 'px';
  janela.style.top = topo + 'px';
}

function fecharJanela() {
  const janela = document.getElementById('janela');
  if (janela) janela.remove();
  document.querySelectorAll('#jogo .fronteira-sel, #jogo .sel-brilho').forEach(p => p.remove());
}

document.addEventListener('click', (e) => {
  if (e.target.closest('#janela .fechar')) { fecharJanela(); return; }
  // clicar no mar (fora de um país) fecha a janela
  if (e.target.closest('#jogo .mapa-svg') && !e.target.closest('.pais')) fecharJanela();
});
document.addEventListener('keydown', (e) => { if (e.key === 'Escape') fecharJanela(); });

// ---------------------------------------------------------------------------
// Ao começar no modo Resistência, o mapa abre com zoom na Europa (uma vez só;
// depois o jogador pode mover livremente). Volta ao mundo ao trocar de modo.
let modoAnterior = null;
function ajustarModo() {
  const sinais = document.getElementById('sinais');
  const modo = sinais ? sinais.dataset.modo : null;
  if (modo && modo !== modoAnterior) {
    if (modoAnterior !== null || modo === 'resistencia') {
      vista = modo === 'resistencia' ? REGIOES.europa.slice() : null;
    }
    modoAnterior = modo;
  }
  esconderDica();
  aplicarVista();
}

document.addEventListener('DOMContentLoaded', ajustarModo);
document.addEventListener('htmx:afterSwap', ajustarModo);
window.addEventListener('resize', posicionarJanela);
