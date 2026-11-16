// Efeitos do jogo: sons, cutscenes e animações.
// O Haskell decide o que acontece; este arquivo só toca o que o HTML pede
// (o <div id="sinais"> diz quais sons tocar, .dado mostra o dado rolando,
// .cutscene contém as cenas dos "filmes").

// ---------------------------------------------------------------------------
// Guardar preferências no navegador (com proteção caso não seja permitido)

function lerPreferencia(chave) {
  try { return localStorage.getItem(chave); } catch (e) { return null; }
}
function gravarPreferencia(chave, valor) {
  try { localStorage.setItem(chave, valor); } catch (e) { /* sem armazenamento */ }
}

// ---------------------------------------------------------------------------
// Sons gerados pelo próprio navegador (Web Audio): nenhum arquivo de áudio

const Sons = (() => {
  let ctx = null;
  let ligado = lerPreferencia('som') !== 'off';

  function contexto() {
    if (!ctx) {
      try { ctx = new (window.AudioContext || window.webkitAudioContext)(); } catch (e) { ctx = null; }
    }
    return ctx;
  }

  // Um tom simples: frequência, duração, tipo de onda, volume, atraso e (opcional) frequência final
  function tom(freq, dur, tipo, vol, atraso, freqFinal) {
    const c = contexto(); if (!c) return;
    const t0 = c.currentTime + (atraso || 0);
    const osc = c.createOscillator();
    const ganho = c.createGain();
    osc.type = tipo || 'square';
    osc.frequency.setValueAtTime(freq, t0);
    if (freqFinal) osc.frequency.exponentialRampToValueAtTime(freqFinal, t0 + dur);
    ganho.gain.setValueAtTime(0.0001, t0);
    ganho.gain.exponentialRampToValueAtTime(vol || 0.05, t0 + 0.01);
    ganho.gain.exponentialRampToValueAtTime(0.0001, t0 + dur);
    osc.connect(ganho).connect(c.destination);
    osc.start(t0);
    osc.stop(t0 + dur + 0.05);
  }

  // Ruído (chiado), filtrado: serve pra máquina de escrever e explosões
  function ruido(dur, vol, freqFiltro, atraso) {
    const c = contexto(); if (!c) return;
    const t0 = c.currentTime + (atraso || 0);
    const buffer = c.createBuffer(1, Math.max(1, Math.floor(c.sampleRate * dur)), c.sampleRate);
    const dados = buffer.getChannelData(0);
    for (let i = 0; i < dados.length; i++) dados[i] = (Math.random() * 2 - 1) * (1 - i / dados.length);
    const fonte = c.createBufferSource();
    fonte.buffer = buffer;
    const filtro = c.createBiquadFilter();
    filtro.type = 'lowpass';
    filtro.frequency.value = freqFiltro || 1000;
    const ganho = c.createGain();
    ganho.gain.value = vol || 0.1;
    fonte.connect(filtro).connect(ganho).connect(c.destination);
    fonte.start(t0);
  }

  const efeitos = {
    clique:     () => tom(900, 0.04, 'square', 0.02),
    tecla:      () => ruido(0.03, 0.05, 3500),
    influencia: () => { tom(520, 0.08, 'triangle', 0.05); tom(780, 0.12, 'triangle', 0.05, 0.08); },
    sucesso:    () => [523, 659, 784].forEach((f, i) => tom(f, 0.18, 'triangle', 0.06, 0.65 + i * 0.1)),
    fracasso:   () => { tom(300, 0.25, 'sawtooth', 0.04, 0.65); tom(200, 0.4, 'sawtooth', 0.04, 0.85); },
    foguete:    () => { ruido(1.2, 0.12, 800); tom(150, 1.2, 'sawtooth', 0.03, 0, 1400); },
    virada:     () => { tom(660, 0.1, 'square', 0.03, 0.9); tom(990, 0.18, 'square', 0.03, 1.0); },
    telegrafo:  () => { [0, 0.1, 0.16, 0.3, 0.36, 0.42, 0.6].forEach(t => tom(820, 0.05, 'square', 0.03, t)); },
    manchete:   () => tom(440, 0.3, 'triangle', 0.05),
    dado:       () => { for (let i = 0; i < 7; i++) ruido(0.03, 0.12, 2500, i * 0.08); },
    alarme:     () => { for (let i = 0; i < 4; i++) tom(880, 0.25, 'sawtooth', 0.04, i * 0.45, 440); },
    explosao:   () => { ruido(2.5, 0.5, 350); tom(70, 2, 'sine', 0.35, 0, 25); },
    nuclear:    () => { efeitos.alarme(); setTimeout(efeitos.explosao, 1800); },
    fim:        () => [392, 523, 659, 784].forEach((f, i) => tom(f, 0.35, 'triangle', 0.06, i * 0.2)),
  };

  return {
    tocar(nome) {
      if (!ligado || !efeitos[nome]) return;
      const c = contexto(); if (!c) return;
      if (c.state === 'suspended') c.resume();
      try { efeitos[nome](); } catch (e) { /* som não é essencial */ }
    },
    alternar() {
      ligado = !ligado;
      gravarPreferencia('som', ligado ? 'on' : 'off');
      return ligado;
    },
    get ligado() { return ligado; },
  };
})();

// ---------------------------------------------------------------------------
// Cutscenes: cenas em sequência, com texto datilografado

const CORES = { E: '#3d6a9e', R: '#8c2f2f' };
let cutsceneAtual = null;

function pintarMapaDaCena(cutscene, cena) {
  const mapa = cutscene.querySelector('.cena-mapa');
  if (!mapa) return;
  const codigo = cena.dataset.mapa;
  if (codigo === undefined) { mapa.classList.remove('visivel'); return; }
  const donos = {};
  codigo.split(',').filter(Boolean).forEach(par => { const [c, l] = par.split(':'); donos[c] = l; });
  mapa.querySelectorAll('.cena-pais').forEach(p => {
    const dono = donos[p.dataset.c];
    p.style.fill = dono ? CORES[dono] : '';
  });
  mapa.classList.add('visivel');
}

function tocarCutscene(el) {
  if (!el || cutsceneAtual) return;
  const cenas = [...el.querySelectorAll('.cena')];
  if (!cenas.length) return;
  let i = 0;
  let timer = null;
  let ativa = true;
  cutsceneAtual = el;
  el.classList.add('ativa');
  document.body.classList.add('em-cutscene');

  function terminar() {
    ativa = false;
    clearTimeout(timer);
    el.classList.remove('ativa');
    document.body.classList.remove('em-cutscene', 'tremendo');
    cenas.forEach(c => c.classList.remove('visivel'));
    cutsceneAtual = null;
    if (el.dataset.cutscene === 'intro') gravarPreferencia('introVista', '1');
    document.removeEventListener('keydown', teclado);
  }

  function proxima() { clearTimeout(timer); i++; mostrar(); }

  function mostrar() {
    if (!ativa) return;
    cenas.forEach(c => c.classList.remove('visivel'));
    if (i >= cenas.length) { terminar(); return; }
    const cena = cenas[i];
    pintarMapaDaCena(el, cena);
    cena.classList.add('visivel');
    if (cena.classList.contains('clarao')) {
      Sons.tocar('explosao');
      document.body.classList.add('tremendo');
      setTimeout(() => document.body.classList.remove('tremendo'), 1200);
    }
    if (cena.classList.contains('alerta')) Sons.tocar('alarme');

    const texto = cena.querySelector('.cena-texto');
    const completo = texto.dataset.completo || texto.textContent;
    texto.dataset.completo = completo;
    texto.textContent = '';
    let k = 0;
    const digitar = () => {
      if (!ativa || cenas[i] !== cena) return;
      if (k < completo.length) {
        texto.textContent += completo[k++];
        if (k % 3 === 0 && completo[k - 1] !== ' ') Sons.tocar('tecla');
        timer = setTimeout(digitar, 40);
      } else {
        timer = setTimeout(proxima, cena.classList.contains('titulo') ? 2200 : 2800);
      }
    };
    timer = setTimeout(digitar, 600);
  }

  function teclado(e) {
    if (e.key === 'Escape') terminar();
    else if (e.key === ' ' || e.key === 'Enter' || e.key === 'ArrowRight') { e.preventDefault(); proxima(); }
  }

  el.querySelector('.pular').onclick = (e) => { e.stopPropagation(); terminar(); };
  el.onclick = () => proxima();  // clicar na tela pula pra próxima cena
  document.addEventListener('keydown', teclado);
  mostrar();
}

// ---------------------------------------------------------------------------
// O que fazer quando a tela muda (primeiro carregamento e cada resposta do HTMX)

function animarDado() {
  const dado = document.querySelector('#jogo .dado[data-final]');
  if (!dado || dado.dataset.animado) return;
  dado.dataset.animado = '1';
  const final = dado.dataset.final;
  const aviso = document.querySelector('#jogo .aviso');
  if (aviso) aviso.classList.add('esperando');
  dado.classList.add('rolando');
  Sons.tocar('dado');
  let voltas = 0;
  const intervalo = setInterval(() => {
    dado.textContent = String(1 + Math.floor(Math.random() * 6));
    if (++voltas >= 8) {
      clearInterval(intervalo);
      dado.textContent = final;
      dado.classList.remove('rolando');
      if (aviso) aviso.classList.remove('esperando');
    }
  }, 75);
}

function processarSinais() {
  const sinais = document.getElementById('sinais');
  if (sinais && sinais.dataset.sons && !sinais.dataset.tocado) {
    sinais.dataset.tocado = '1';
    sinais.dataset.sons.split(' ').filter(Boolean).forEach(s => Sons.tocar(s));
  }
  animarDado();

  // cutscene de fim de jogo
  const final = document.querySelector('#jogo .cutscene[data-cutscene="final"][data-auto="1"]');
  if (final && !final.dataset.tocada) { final.dataset.tocada = '1'; setTimeout(() => tocarCutscene(final), 900); }

  // guerra nuclear: a tela treme uma vez
  if (document.querySelector('#jogo .layout.nuclear') && sinais && sinais.dataset.sons.includes('nuclear')) {
    setTimeout(() => { document.body.classList.add('tremendo'); setTimeout(() => document.body.classList.remove('tremendo'), 1500); }, 1800);
  }

  // abertura: toca sozinha só na primeira visita
  const intro = document.querySelector('#jogo .cutscene[data-cutscene="intro"]');
  if (intro && !intro.dataset.verificada) {
    intro.dataset.verificada = '1';
    if (!lerPreferencia('introVista')) setTimeout(() => tocarCutscene(intro), 400);
  }
}

document.addEventListener('click', (e) => {
  if (e.target.closest('#ver-intro')) {
    tocarCutscene(document.querySelector('#jogo .cutscene[data-cutscene="intro"]'));
  } else if (e.target.closest('#rever-final')) {
    const final = document.querySelector('#jogo .cutscene[data-cutscene="final"]');
    if (final) tocarCutscene(final);
  } else if (e.target.closest('#botao-som')) {
    const ligado = Sons.alternar();
    e.target.closest('#botao-som').textContent = ligado ? '🔊' : '🔇';
    if (ligado) Sons.tocar('clique');
  } else if (e.target.closest('button, .pais')) {
    Sons.tocar('clique');
  }
});

document.addEventListener('DOMContentLoaded', () => {
  const botao = document.getElementById('botao-som');
  if (botao) botao.textContent = Sons.ligado ? '🔊' : '🔇';
  processarSinais();
});
document.addEventListener('htmx:afterSwap', processarSinais);
