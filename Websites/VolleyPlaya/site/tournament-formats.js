const tournamentFormats = [
  ['single', 'Eliminación directa (Single Elimination)', 2, 'Rapidez', 'Recreativo y finales rápidas', 'Cada derrota elimina al equipo. El ganador de la final es campeón.', 'Sortea o siembra el cuadro; si faltan equipos para una potencia de dos, asigna pases libres antes de empezar. Juega octavos, cuartos, semifinales y final según el tamaño del cuadro.', 'Con 8 equipos se disputan 8 partidos en total al incluir el partido por el bronce. Los perdedores de semifinales juegan por el tercer puesto.'],
  ['double', 'Doble eliminación (Double Elimination)', 4, 'Segunda oportunidad', 'Competitivo', 'La primera derrota envía al cuadro de perdedores; la segunda elimina.', 'Conecta cada derrota del cuadro principal con su cruce en el cuadro de perdedores. Los ganadores de ambos cuadros se enfrentan en la gran final.', 'En la variante estricta, si gana el finalista del cuadro de perdedores se repite la final: ambos tienen entonces una derrota. Publica previamente si usas otra regla.'],
  ['pools', 'Fase de grupos (Pool Play)', 3, 'Más partidos', 'Eventos grandes', 'Los equipos juegan todos contra todos dentro de su grupo y se ordenan por resultados.', 'Distribuye los equipos en grupos equilibrados, programa cada pareja una vez y publica los criterios de desempate. Decide antes del inicio si termina con clasificación o continúa con playoffs.', 'Un grupo de 3 disputa 3 partidos y cada equipo juega 2. A diferencia de una liga completa, no todos se enfrentan a todos los inscritos.'],
  ['mpp', 'Modified Pool Play (MPP)', 4, 'Grupos adaptados', 'Eventos con siembra previa', 'Adapta los cruces de grupo para obtener una clasificación con menos encuentros que un todos contra todos.', 'Publica la variante y coloca los cabezas de serie por ranking. Una variante de 4 equipos empieza con 1º–4º y 2º–3º; después se enfrentan ganadores entre sí y perdedores entre sí, con un cruce decisivo si hace falta.', 'No existe un único calendario MPP universal. Define los cruces y las plazas de clasificación; repartir equipos por ranking, por sí solo, no describe un MPP.'],
  ['round-robin', 'Round Robin completo', 3, 'Todos contra todos', 'Torneos pequeños y ligas cortas', 'Todos los equipos inscritos se enfrentan entre sí. La clasificación refleja toda la liga.', 'Programa una vuelta completa y ordena por victorias y los desempates publicados. Decide si el primero es campeón o habrá una fase final adicional.', 'Con n equipos hay n × (n − 1) / 2 partidos por vuelta. Con 6 equipos son 15 partidos y 5 por equipo.'],
  ['final-four', 'Round Robin parcial + Final Four', 4, 'Clasificación y semifinales', 'Universitario y eventos cortos', 'Una fase parcial o de grupos produce un ranking general; los cuatro mejores juegan el título.', 'Publica rivales y número de partidos por equipo. Tras la fase inicial, cruza 1º–4º y 2º–3º; los ganadores disputan la final.', 'Si los grupos tienen distinto tamaño, compara mediante un criterio normalizado publicado antes del torneo. Rivales distintos pueden afectar la comparación.'],
  ['swiss', 'Formato suizo (Swiss System)', 6, 'Rivales de rendimiento similar', 'Eventos grandes y clasificatorios', 'Se juegan varias rondas sin eliminación temprana; los cruces dependen de las victorias acumuladas.', 'Fija el número de rondas. Tras cada ronda, empareja equipos con resultados similares evitando repetir rival. Con número impar, reparte los descansos sin repetirlos cuando sea posible.', 'Publica desempates, tratamiento del descanso y si habrá playoffs. Los cruces de la siguiente ronda se calculan al cerrar los resultados de la anterior.'],
  ['league', 'Liga con playoffs', 4, 'Temporada y fase final', 'Clubes y ligas', 'La liga regular determina la clasificación y las plazas para una fase final eliminatoria.', 'Organiza una o más vueltas, fija cuántos equipos clasifican y establece las fechas de playoffs. Para cuatro clasificados: 1º–4º, 2º–3º y final.', 'A diferencia de una fase parcial, la temporada tiene un calendario regular. Define si los playoffs son a partido único o por series.'],
  ['plus', 'Doble eliminación PLUS (DE+)', 6, 'Clasificación completa', 'Competitivo', 'Añade partidos de posición a una doble eliminación para ordenar todos los puestos.', 'Prepara el cuadro principal y el de perdedores; añade cruces entre equipos eliminados en la misma fase para resolver sus posiciones.', 'DE+ es una denominación de organización con variantes. Publica cada cruce de posición y reserva tiempo y pistas para esos partidos adicionales.'],
  ['consolation', 'Consolación paralela (Overflow / Side Bracket)', 2, 'Partidos adicionales', 'Recreativo', 'Un cuadro secundario ofrece partidos sin dar acceso al título principal.', 'Define quién entra, cuándo se incorpora y si la consolación será una liga o una eliminatoria. Mantén su clasificación separada de la principal.', 'El mínimo se refiere a 2 equipos en la consolación, además de los participantes necesarios para el cuadro principal. Aceptar inscripciones tardías depende de las bases.'],
  ['kotc', 'King of the Court (KOTC)', 5, 'Rotación y exhibición', 'Eventos especiales', 'Las parejas rotan entre el lado retador, el lado del rey y la cola. Solo el lado del rey suma puntos.', 'Fija duración, orden de entrada y eliminaciones. Si el rey gana el rally, suma y permanece; si pierde, el retador pasa al lado del rey y entra la siguiente pareja de la cola.', 'Ganar como retador permite ocupar el lado del rey, sin sumar ese rally. Define quién saca, cómo se resuelven empates y qué ocurre al terminar el tiempo.'],
  ['groups-knockout', 'Grupos → Eliminación directa', 4, 'Partidos garantizados y final rápida', 'Torneos medianos', 'La fase de grupos asegura encuentros iniciales; después, una derrota elimina del cuadro final.', 'Publica cuántos clasifican por grupo y los cruces entre grupos. Completa la clasificación y juega cuartos, semifinales y final según las plazas disponibles.', 'A diferencia del Final Four, puede clasificar un número distinto de cuatro equipos y usar cruces por puesto de grupo en vez de un ranking general.']
];


const tournamentPodiumRules = {
  single: 'Oro y plata: ganador y perdedor de la final. Bronce: ganador del partido entre los perdedores de semifinales. Con 3 equipos, el equipo eliminado antes de la final es tercero.',
  double: 'Oro y plata: ganador y perdedor de la final definitiva. Bronce: perdedor de la última ronda del cuadro de perdedores. Si hay repetición de la gran final, espera a su resultado para anunciar oro y plata.',
  pools: 'Sin playoffs, los tres primeros de la clasificación final reciben oro, plata y bronce. Con varios grupos, programa una fase final común para decidir el podio general; la posición dentro de un grupo no basta.',
  mpp: 'Define en las bases una fase final que resuelva los tres puestos. Oro y plata salen de la final; el bronce se decide entre los perdedores de semifinales. Si la variante termina en una clasificación única, usa sus tres primeros tras resolver todos los empates.',
  'round-robin': 'Sin playoffs, oro, plata y bronce corresponden a los tres primeros de la liga tras aplicar los desempates. Si añades semifinales y final, incluye un partido por el bronce entre los perdedores de semifinales.',
  'final-four': 'Oro: ganador de la final. Plata: perdedor de la final. Bronce: ganador del partido entre los perdedores de las dos semifinales. Reserva ese encuentro en el calendario.',
  swiss: 'Sin playoffs, anuncia los tres primeros de la clasificación tras la última ronda y todos los desempates. Con semifinales y final, oro y plata salen de la final y el bronce de un partido entre los perdedores de semifinales.',
  league: 'Oro y plata se deciden en la final de playoffs. Los perdedores de semifinales disputan un partido o serie por el bronce, según las bases; no asignes el tercero solo por su posición en la liga regular.',
  plus: 'Oro y plata: ganador y perdedor de la final definitiva. Bronce: perdedor de la última ronda del cuadro de perdedores. Los cruces adicionales resuelven los puestos restantes sin cambiar ese podio.',
  consolation: 'Anuncia un podio propio de consolación, separado del principal. En eliminatoria, final para oro y plata y partido entre los perdedores de semifinales para el bronce; en liga, los tres primeros de la tabla. Con solo 2 equipos no hay tercer puesto.',
  kotc: 'Ordena a los equipos de la ronda final por puntos obtenidos en el lado del rey: primero oro, segundo plata y tercero bronce. Publica el desempate y resuélvelo antes de anunciar el podio; no mezcles puntos de rondas con distinta duración.',
  'groups-knockout': 'Oro y plata: ganador y perdedor de la final. Bronce: ganador del partido entre los perdedores de semifinales. El tercer puesto se juega después de las semifinales y antes o junto a la final.'
};


function setupTournamentFormatGuide() {
  const guide = document.querySelector('#tournamentFormatGuide');
  if (!guide) return;
  const automatic = new Set(['final-four', 'double']);
  guide.innerHTML = `
    <div class="section-heading">
      <p class="eyebrow">Elige con criterio</p>
      <h2 id="format-guide-title">Cada torneo tiene su forma de jugar.</h2>
      <p>Compara 12 formatos. Abre uno para ver cómo organizarlo y decidir su podio.</p>
    </div>
    <p class="setup-hint">Para empezar, usa <strong>Final Four</strong>: grupos, dos semifinales, final y partido por el bronce. El generador también ofrece doble eliminación; el resto se organiza con esta guía.</p>
    <div class="format-guide-tools">
      <label>Buscar un formato<input id="formatSearch" type="search" placeholder="Ej.: grupos, liga, suizo…" /></label>
      <label>Mostrar<select id="formatFilter"><option value="all">Los 12 formatos</option><option value="automatic">Disponibles en el generador</option><option value="manual">Guías de organización manual</option></select></label>
    </div>
    <p id="formatResultCount" class="results-count" role="status">12 formatos</p>
    <div class="format-guide-grid">${tournamentFormats.map(([id, name, min, purpose, use, explanation, steps, detail]) => `
      <details class="format-guide-card" id="format-${id}" data-format="${id}">
        <summary><span>${name}</span><small>Desde ${min} equipos · ${purpose}</small></summary>
        <div class="format-guide-body"><p>${explanation}</p><h4>Cómo organizarlo</h4><p>${steps}</p><h4>Podio · 1.º, 2.º y 3.º</h4><p>${tournamentPodiumRules[id]}</p><h4>Ten en cuenta</h4><p>${detail}</p><p><strong>Ideal para:</strong> ${use.toLowerCase()}.</p>${automatic.has(id) ? `<button type="button" class="button secondary-dark" data-use-format="${id}">Usar este formato</button>` : ''}</div>
      </details>`).join('')}</div>
    <p id="formatNoResults" class="empty-state" hidden>No hay coincidencias. Prueba otro nombre o selecciona «Los 12 formatos».</p>
    <details class="content-disclosure format-comparison"><summary>Comparar los 12 formatos en una tabla</summary>
      <div class="format-table-scroll" role="region" aria-label="Comparativa de formatos" tabindex="0">
        <table class="format-table"><caption>Objetivo, equipos y uso habitual</caption>
          <thead><tr><th scope="col">Formato</th><th scope="col">Objetivo</th><th scope="col">Mínimo orientativo</th><th scope="col">Uso habitual</th></tr></thead>
          <tbody>${tournamentFormats.map(([id, name, min, purpose, use]) => `<tr><th scope="row"><a href="#format-${id}">${name}</a></th><td>${purpose}</td><td>${min} equipos</td><td>${use}</td></tr>`).join('')}</tbody>
        </table>
      </div>
    </details>
    <details class="content-disclosure"><summary>Antes de empezar: reglas y podio</summary><div class="disclosure-body"><p>Publica puntuación, calendario, descansos y desempates antes del primer partido. Reserva tiempo para la final y el bronce; en KOTC fija también la duración de cada ronda.</p><p>Los mínimos son referencias prácticas de organización. Para un podio de tres necesitas al menos 3 equipos; con solo 2, anuncia oro y plata y marca el bronce como «No se disputa».</p></div></details>`;
  const search = guide.querySelector('#formatSearch');
  const filter = guide.querySelector('#formatFilter');
  const normalize = text => text.normalize('NFD').replace(/[\u0300-\u036f]/g, '').toLowerCase();
  const applyFilters = () => {
    const query = normalize(search.value.trim());
    let visible = 0;
    guide.querySelectorAll('[data-format]').forEach(card => {
      const supported = automatic.has(card.dataset.format);
      card.hidden = !(normalize(card.textContent).includes(query) && (filter.value === 'all' || (filter.value === 'automatic' ? supported : !supported)));
      if (!card.hidden) visible += 1;
    });
    guide.querySelector('#formatResultCount').textContent = visible + (visible === 1 ? ' formato' : ' formatos');
    guide.querySelector('#formatNoResults').hidden = visible > 0;
  };
  search.addEventListener('input', applyFilters);
  filter.addEventListener('change', applyFilters);
  guide.querySelectorAll('.format-table a').forEach(link => link.addEventListener('click', () => {
    search.value = ''; filter.value = 'all'; applyFilters();
    const card = document.getElementById(link.hash.slice(1));
    card.open = true;
  }));
  guide.querySelectorAll('[data-use-format]').forEach(button => button.addEventListener('click', () => {
    const select = document.querySelector('#tournamentFormat');
    if (select.value !== button.dataset.useFormat) {
      select.value = button.dataset.useFormat;
      select.dispatchEvent(new Event('change', {bubbles: true}));
    }
    document.querySelector('#torneo').scrollIntoView({behavior: window.matchMedia('(prefers-reduced-motion: reduce)').matches ? 'auto' : 'smooth'});
    select.focus({preventScroll: true});
  }));
}

setupTournamentFormatGuide();
