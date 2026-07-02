let scenarios = [
  {
    id: "recepcion-22",
    label: "Recepción 2-2",
    meta: ["Side-out", "Recepción", "Base"],
    title: "Recepción 2-2: cerrar el pasillo central",
    summary:
      "Antes del saque la pareja define quién manda en el centro. El receptor que entra de frente tiene prioridad y el compañero se convierte rápido en colocador.",
    objective:
      "Recibir alto hacia la ventana central, evitar dudas en el balón entre dos y llegar al segundo toque con hombros orientados al atacante.",
    calls: [
      "Mía: el receptor entra de frente al pasillo central.",
      "Tuya: el compañero llega con mejor ángulo.",
      "Alta: la recepción sale separada de la red y necesita tiempo.",
      "Centro: el segundo toque debe quedar dentro de la ventana de colocación."
    ],
    movements: [
      "A inicia en fondo-línea, a 2 metros de la línea lateral. Si el saque viene al pasillo central, primer paso corto hacia dentro y plataforma estable.",
      "B inicia en diagonal media. Si A recibe, B entra por delante del balón hacia zona de colocación, sin cruzarse por delante del receptor.",
      "Después del primer toque, el receptor recupera dos pasos hacia cobertura del atacante."
    ],
    roles: [
      "Receptor de frente manda sobre el balón del centro.",
      "No receptor entra a colocar con carrera curva y frena antes del contacto.",
      "Atacante espera la altura del pase y mira bloqueo/defensa rival antes de decidir."
    ],
    corrections: [
      "Si ambos dudan, asignar centro fijo al receptor que está más adelantado.",
      "Si la recepción cae pegada a la red, cantar 'libre' o 'alta' antes de correr.",
      "Si el colocador llega tarde, priorizar pase alto y no bola rápida."
    ],
    drill:
      "Saque dirigido a línea, pasillo central y diagonal. La pareja solo suma si canta prioridad antes del contacto y termina con ataque dirigido.",
    court: {
      players: [
        { type: "home", label: "A", x: 23, y: 68 },
        { type: "partner", label: "B", x: 36, y: 34 },
        { type: "rival", label: "S", x: 78, y: 50 }
      ],
      ball: { x: 72, y: 50 },
      zones: [
        { label: "Pasillo", x: 28, y: 42, w: 16, h: 24 },
        { label: "Colocar", x: 45, y: 40, w: 10, h: 22 }
      ],
      routes: [
        { kind: "ball", x1: 72, y1: 50, x2: 30, y2: 54 },
        { kind: "move", x1: 36, y1: 34, x2: 47, y2: 47 },
        { kind: "move", x1: 23, y1: 68, x2: 30, y2: 54 }
      ]
    }
  },
  {
    id: "saque-seam",
    label: "Saque al centro",
    meta: ["Saque", "Estrategia", "Presión"],
    title: "Saque al pasillo central: provocar decisión tardía",
    summary:
      "El saque al espacio entre receptores no busca solo punto directo: fuerza una llamada, mueve al colocador y reduce la calidad del ataque rival.",
    objective:
      "Hacer que el rival reciba en movimiento y obligarle a jugar una colocación alta, previsible y separada de la red.",
    calls: [
      "Centro: objetivo entre hombros de los receptores.",
      "Corto: si el bloqueador rival se queda pesado atrás.",
      "Largo: si el receptor adelanta demasiado el peso.",
      "Entra: aviso del sacador para ocupar base defensiva tras sacar."
    ],
    movements: [
      "Sacador inicia detrás de fondo, sirve al pasillo central y entra hacia diagonal media del lado acordado.",
      "Bloqueador inicia a 1 metro de la red, observa la recepción y decide si bloquea línea, diagonal o se retira.",
      "Si la recepción rival sale alta y separada, el defensor se planta profundo; si sale corta, avanza a zona de toque."
    ],
    roles: [
      "Sacador comunica objetivo antes de sacar y entra sin quedarse mirando.",
      "Compañero en red confirma señal de bloqueo y lee primer toque rival.",
      "Defensor ajusta su base según la calidad de recepción rival."
    ],
    corrections: [
      "No sacar al centro sin plan de bloqueo posterior.",
      "Si el sacador tarda en entrar, el rival ataca la diagonal libre.",
      "Con viento frontal, reducir riesgo y buscar saque flotado profundo al cuerpo."
    ],
    drill:
      "Serie de 12 saques: 4 al pasillo, 4 al cuerpo del receptor débil y 4 cortos. Puntúa si el equipo entra a su base defensiva antes del segundo toque rival.",
    court: {
      players: [
        { type: "home", label: "S", x: 88, y: 78 },
        { type: "partner", label: "B", x: 52, y: 38 },
        { type: "rival", label: "R1", x: 25, y: 33 },
        { type: "rival", label: "R2", x: 25, y: 67 }
      ],
      ball: { x: 88, y: 78 },
      zones: [
        { label: "Objetivo", x: 18, y: 43, w: 16, h: 20 },
        { label: "Base S", x: 62, y: 62, w: 14, h: 20 }
      ],
      routes: [
        { kind: "ball", x1: 88, y1: 78, x2: 26, y2: 52 },
        { kind: "move", x1: 88, y1: 78, x2: 68, y2: 68 },
        { kind: "move", x1: 52, y1: 38, x2: 50, y2: 31 }
      ]
    }
  },
  {
    id: "sideout-linea",
    label: "Side-out línea",
    meta: ["Ataque", "Recepción", "Salida"],
    title: "Side-out por línea: abrir el campo después de recibir",
    summary:
      "Cuando la recepción obliga al colocador a entrar desde el centro, el atacante puede cargar línea si el bloqueo llega tarde o cierra diagonal.",
    objective:
      "Crear una carrera de ataque corta pero equilibrada, con llamada temprana del colocador y cobertura inmediata del compañero.",
    calls: [
      "Línea: ataque directo al lateral libre.",
      "Manos: usar bloqueo si está formado.",
      "Corta: si el defensor se hunde demasiado.",
      "Cubro: el colocador se queda debajo del atacante tras soltar el pase."
    ],
    movements: [
      "Receptor inicia en fondo, recibe y sale por fuera de la línea del balón para preparar carrera de dos pasos.",
      "Colocador entra desde centro, coloca alto a hombro externo y cae a cobertura corta.",
      "Atacante aterriza y recupera al centro por si el rival defiende."
    ],
    roles: [
      "Atacante no invade debajo del balón: espera con hombros abiertos.",
      "Colocador comunica dirección probable si ve al defensor.",
      "La cobertura se coloca delante del atacante, no detrás."
    ],
    corrections: [
      "Si la colocación queda dentro, no forzar línea: jugar manos o diagonal corta.",
      "Si el bloqueo está cerrado, usar golpe alto a fondo contrario.",
      "Si no hay llamada, atacar zona grande y recuperar rápido."
    ],
    drill:
      "Recepción + ataque condicionado: el colocador debe cantar línea, corta o manos antes del salto del atacante.",
    court: {
      players: [
        { type: "home", label: "A", x: 28, y: 66 },
        { type: "partner", label: "C", x: 43, y: 49 },
        { type: "rival", label: "B", x: 52, y: 34 },
        { type: "rival", label: "D", x: 70, y: 64 }
      ],
      ball: { x: 43, y: 49 },
      zones: [
        { label: "Carrera", x: 18, y: 55, w: 18, h: 28 },
        { label: "Línea", x: 57, y: 18, w: 12, h: 34 }
      ],
      routes: [
        { kind: "ball", x1: 43, y1: 49, x2: 31, y2: 39 },
        { kind: "move", x1: 28, y1: 66, x2: 31, y2: 39 },
        { kind: "move", x1: 43, y1: 49, x2: 36, y2: 45 }
      ]
    }
  },
  {
    id: "defensa-linea",
    label: "Bloqueo línea",
    meta: ["Bloqueo", "Defensa", "2-2"],
    title: "Bloqueo línea, defensa diagonal",
    summary:
      "El bloqueador quita línea y el defensor se coloca con hombros hacia diagonal larga, preparado para defender golpe fuerte o correr la corta.",
    objective:
      "Reducir opciones del atacante, obligar a diagonal y transformar defensa positiva en contraataque.",
    calls: [
      "Línea: el bloqueador cierra lateral.",
      "Diagonal: el defensor se responsabiliza de la diagonal larga.",
      "Corta: aviso si el brazo atacante baja.",
      "Fuera: si el atacante llega tarde y golpea por detrás del cuerpo."
    ],
    movements: [
      "Bloqueador inicia frente al atacante, salta vertical cerrando línea con mano exterior fuerte.",
      "Defensor inicia en diagonal media, baja centro de gravedad y ajusta un paso según carrera del atacante.",
      "Si hay toque de bloqueo, defensor avanza dos pasos a zona corta; si no hay toque, mantiene diagonal larga."
    ],
    roles: [
      "Bloqueador no persigue balón: cierra espacio elegido.",
      "Defensor lee hombro y muñeca, no la pelota solamente.",
      "Tras defensa, bloqueador se abre para ser atacante de transición."
    ],
    corrections: [
      "Si el bloqueo salta hacia dentro, la línea queda abierta.",
      "Si el defensor se hunde demasiado, regala corta.",
      "Si nadie canta toque, se pierde el primer paso de transición."
    ],
    drill:
      "Ataque rival desde zona 4: tres bolas a línea, diagonal y corta. La defensa suma doble si la llamada llega antes del golpeo.",
    court: {
      players: [
        { type: "home", label: "D", x: 27, y: 64 },
        { type: "partner", label: "B", x: 48, y: 35 },
        { type: "rival", label: "A", x: 58, y: 34 }
      ],
      ball: { x: 58, y: 34 },
      zones: [
        { label: "Diagonal", x: 20, y: 49, w: 25, h: 34 },
        { label: "Corta", x: 40, y: 37, w: 10, h: 20 }
      ],
      routes: [
        { kind: "ball", x1: 58, y1: 34, x2: 27, y2: 64 },
        { kind: "move", x1: 48, y1: 35, x2: 49, y2: 28 },
        { kind: "move", x1: 27, y1: 64, x2: 34, y2: 58 }
      ]
    }
  },
  {
    id: "defensa-diagonal",
    label: "Bloqueo diagonal",
    meta: ["Bloqueo", "Defensa", "Variación"],
    title: "Bloqueo diagonal, defensa línea",
    summary:
      "Variación para castigar atacantes que viven de la diagonal fuerte. El bloqueador se mete en diagonal y el defensor protege línea y toque alto.",
    objective:
      "Quitar el golpe natural del atacante y forzar una línea técnica bajo presión.",
    calls: [
      "Ángulo: el bloqueador cierra diagonal.",
      "Línea: el defensor se abre a la línea larga.",
      "Alta: defensa de shot profundo.",
      "Cubro: cobertura corta si el atacante cambia a dejada."
    ],
    movements: [
      "Bloqueador inicia centrado, ajusta medio paso hacia dentro y presenta manos a diagonal.",
      "Defensor inicia más abierto hacia línea, con pie exterior listo para correr fondo.",
      "Si el atacante no tiene carrera, defensor avanza porque aumenta probabilidad de corta."
    ],
    roles: [
      "Bloqueador muestra tarde la intención para no regalar lectura.",
      "Defensor protege línea sin perder visión del atacante.",
      "La pareja cambia esta estrategia solo con señal clara antes del saque."
    ],
    corrections: [
      "No usar bloqueo diagonal si el defensor llega tarde a línea.",
      "Si el rival juega shot por encima, el defensor debe empezar medio paso más profundo.",
      "Si el atacante pega alto a manos, bloquear con hombros firmes y no invadir hacia fuera."
    ],
    drill:
      "Bloqueo diagonal durante 8 ataques seguidos. El defensor solo puntúa si arranca desde línea media y nombra el golpe antes del contacto.",
    court: {
      players: [
        { type: "home", label: "D", x: 33, y: 31 },
        { type: "partner", label: "B", x: 49, y: 63 },
        { type: "rival", label: "A", x: 58, y: 64 }
      ],
      ball: { x: 58, y: 64 },
      zones: [
        { label: "Línea", x: 19, y: 18, w: 18, h: 30 },
        { label: "Ángulo", x: 43, y: 54, w: 12, h: 26 }
      ],
      routes: [
        { kind: "ball", x1: 58, y1: 64, x2: 31, y2: 31 },
        { kind: "move", x1: 49, y1: 63, x2: 48, y2: 57 },
        { kind: "move", x1: 33, y1: 31, x2: 27, y2: 28 }
      ]
    }
  },
  {
    id: "pull-defense",
    label: "Falso bloqueo",
    meta: ["Pull", "Lectura", "Defensa"],
    title: "Falso bloqueo: retirarse de la red para defender dos zonas",
    summary:
      "Cuando el rival recibe mal o ataca sin carrera, el bloqueador puede retirarse de la red para sumar un segundo defensor.",
    objective:
      "Convertir una situación de ataque débil rival en defensa organizada con dos jugadores leyendo el golpe.",
    calls: [
      "Puño: el bloqueador se retira de la red.",
      "Corta: quien sale del bloqueo toma la zona corta.",
      "Fondo: el defensor original se queda profundo.",
      "Yo: confirmación de balón defendible."
    ],
    movements: [
      "Bloqueador inicia en red, lee recepción mala y retrocede con pasos cruzados hacia zona corta-media.",
      "Defensor inicia profundo, no avanza hasta confirmar que el bloqueador se retira.",
      "Tras defensa positiva, el jugador que no toca el balón entra a colocar."
    ],
    roles: [
      "El pull se decide antes del salto rival, no durante.",
      "Jugador que se retira protege corta y línea media.",
      "Defensor profundo protege golpe alto y diagonal larga."
    ],
    corrections: [
      "Si el bloqueador sale tarde, queda a medio camino.",
      "Si ambos avanzan, el fondo queda libre.",
      "Si nadie confirma la señal de puño, el defensor no sabe que cambia su zona."
    ],
    drill:
      "Entrenador lanza recepciones malas al rival. La pareja decide bloquear o retirarse y debe nombrar corta/fondo antes del golpeo.",
    court: {
      players: [
        { type: "home", label: "F", x: 42, y: 42 },
        { type: "partner", label: "P", x: 29, y: 65 },
        { type: "rival", label: "A", x: 58, y: 38 }
      ],
      ball: { x: 58, y: 38 },
      zones: [
        { label: "Corta", x: 35, y: 35, w: 16, h: 22 },
        { label: "Fondo", x: 18, y: 58, w: 23, h: 25 }
      ],
      routes: [
        { kind: "move", x1: 49, y1: 38, x2: 42, y2: 42 },
        { kind: "ball", x1: 58, y1: 38, x2: 39, y2: 43 },
        { kind: "move", x1: 29, y1: 65, x2: 27, y2: 69 }
      ]
    }
  },
  {
    id: "free-ball",
    label: "Free ball",
    meta: ["Transición", "Ataque", "2-2"],
    title: "Free ball: pasar de defensa a ataque",
    summary:
      "Cuando el rival regala una bola alta, la pareja cambia el ritmo: primer toque limpio, colocación estable y ataque al hueco.",
    objective:
      "Convertir una bola fácil en una acción ofensiva organizada, sin precipitar el segundo toque.",
    calls: [
      "Tiempo: frenar y construir.",
      "Alta: colocar con margen.",
      "Hueco: atacar zona libre.",
      "Cubro: el colocador protege toque de bloqueo."
    ],
    movements: [
      "Quien recibe free ball inicia debajo de la trayectoria y juega primer toque alto al centro.",
      "Compañero se perfila como colocador a 2 metros de la red, hombros al atacante.",
      "Atacante espera fuera de la línea del balón y entra con carrera corta."
    ],
    roles: [
      "Primer contacto prioriza altura y centro, no velocidad.",
      "Colocador canta tiempo si ve al rival desordenado.",
      "Atacante mira defensa rival antes del último paso."
    ],
    corrections: [
      "No atacar directo una bola que permite construir.",
      "No colocar desde debajo del balón sin hombros al objetivo.",
      "No terminar el punto sin cobertura: el rival también puede defender."
    ],
    drill:
      "El entrenador lanza free balls alternas. La pareja debe nombrar el hueco antes del tercer toque.",
    court: {
      players: [
        { type: "home", label: "R", x: 30, y: 56 },
        { type: "partner", label: "C", x: 43, y: 44 },
        { type: "rival", label: "D", x: 69, y: 62 },
        { type: "rival", label: "B", x: 53, y: 32 }
      ],
      ball: { x: 64, y: 46 },
      zones: [
        { label: "Primer toque", x: 25, y: 45, w: 15, h: 22 },
        { label: "Hueco", x: 64, y: 20, w: 18, h: 24 }
      ],
      routes: [
        { kind: "ball", x1: 64, y1: 46, x2: 30, y2: 56 },
        { kind: "move", x1: 43, y1: 44, x2: 49, y2: 41 },
        { kind: "move", x1: 30, y1: 56, x2: 35, y2: 43 }
      ]
    }
  },
  {
    id: "uno-uno",
    label: "1-1 iniciación",
    meta: ["Individual", "Lectura", "Base"],
    title: "1-1: ocupar centro útil y recuperar equilibrio",
    summary:
      "El jugador aprende a volver al centro útil después de cada toque y a orientar el cuerpo antes de decidir dirección.",
    objective:
      "Mantener continuidad con desplazamientos cortos y recuperación rápida tras el golpeo.",
    calls: [
      "Centro: volver a base útil.",
      "Alta: ganar tiempo.",
      "Dentro: priorizar seguridad.",
      "Hueco: atacar donde el rival no puede llegar equilibrado."
    ],
    movements: [
      "Jugador inicia en centro útil, no pegado a línea.",
      "Tras defender, da un paso de salida hacia fuera para atacar con espacio.",
      "Después del golpeo, recupera con pasos cruzados hacia el centro útil."
    ],
    roles: [
      "Defender desde el centro útil, no desde el centro geométrico.",
      "Recuperar después del golpeo antes de mirar el resultado.",
      "Atacar a espacio libre cuando el rival queda fuera de base."
    ],
    corrections: [
      "Evitar quedarse mirando el golpeo propio.",
      "No defender demasiado pegado a línea.",
      "No atacar fuerte si el cuerpo está desequilibrado."
    ],
    drill:
      "Campo reducido 4 x 8. Punto solo válido si el jugador recupera base antes del siguiente contacto rival.",
    court: {
      players: [
        { type: "home", label: "J", x: 28, y: 50 },
        { type: "rival", label: "R", x: 72, y: 42 }
      ],
      ball: { x: 72, y: 42 },
      zones: [
        { label: "Centro útil", x: 20, y: 36, w: 20, h: 28 },
        { label: "Espacio", x: 67, y: 58, w: 16, h: 22 }
      ],
      routes: [
        { kind: "ball", x1: 72, y1: 42, x2: 28, y2: 50 },
        { kind: "move", x1: 28, y1: 50, x2: 30, y2: 42 },
        { kind: "move", x1: 30, y1: 42, x2: 28, y2: 50 }
      ]
    }
  }
];

scenarios = scenarios.filter((scenario) => scenario.id !== "uno-uno");

const signalModes = [
  {
    id: "linea",
    label: "1-1 · Línea",
    fingers: 1,
    handLabel: "Dos índices extendidos",
    badge: "1-1: bloqueo línea a ambos lados",
    title: "Bloqueador cierra línea; defensor toma diagonal",
    meaning:
      "Los dos dedos índice indican que, ataque por izquierda o por derecha, el bloqueador quitará la línea.",
    blockZone: "B: línea",
    defenseZone: "D: diagonal",
    supportZone: "Corta",
    finalCoverage: "linea",
    attack: "línea",
    correction:
      "El bloqueador debe llegar frente al hombro atacante y saltar vertical. Si deriva hacia el centro, vuelve a abrir la línea."
  },
  {
    id: "diagonal",
    label: "2-2 · Diagonal",
    fingers: 2,
    handLabel: "Dos dedos en V en cada mano",
    badge: "2-2: bloqueo diagonal a ambos lados",
    title: "Bloqueador cierra diagonal; defensor toma línea",
    meaning:
      "Las dos V indican que el bloqueo entra en el ángulo de ataque y el defensor sale a proteger la línea.",
    blockZone: "B: diagonal",
    defenseZone: "D: línea",
    supportZone: "Corta",
    finalCoverage: "diagonal",
    attack: "diagonal",
    correction:
      "El defensor no espera debajo del bloqueo: abre su base hacia la línea antes del golpeo y conserva visión del atacante."
  },
  {
    id: "pull",
    label: "Puño · Sin bloqueo",
    fingers: 0,
    handLabel: "Dos puños cerrados",
    badge: "Puño: el bloqueador se retira",
    title: "Sin bloqueo: dos jugadores pasan a defender",
    meaning:
      "El puño cerrado indica que no habrá bloqueo al atacante de ese lado. El bloqueador baja a defender la zona corta.",
    blockZone: "B: corta",
    defenseZone: "D: fondo",
    supportZone: "Centro",
    finalCoverage: "pull",
    attack: "controlado",
    correction:
      "La retirada empieza al leer una recepción o colocación mala. Si se inicia cuando el atacante ya salta, el bloqueador queda a medio camino."
  },
  {
    id: "tres",
    label: "3 · Finta diagonal",
    fingers: 3,
    handLabel: "Tres dedos extendidos",
    badge: "3: muestra diagonal y cambia a línea",
    title: "Engaño 3: amaga diagonal y bloquea línea",
    meaning:
      "El bloqueador se presenta en diagonal y, justo antes del ataque, rectifica para cerrar línea. El defensor hace el cambio contrario.",
    blockZone: "B: cambia a línea",
    defenseZone: "D: cambia a diagonal",
    supportZone: "Corta",
    fakeCoverage: "diagonal",
    finalCoverage: "linea",
    attack: "línea",
    correction:
      "La rectificación es corta y tardía, pero debe terminar antes del salto. Un cambio grande o demasiado temprano revela el engaño."
  },
  {
    id: "cuatro",
    label: "4 · Finta línea",
    fingers: 4,
    handLabel: "Cuatro dedos extendidos",
    badge: "4: muestra línea y cambia a diagonal",
    title: "Engaño 4: amaga línea y bloquea diagonal",
    meaning:
      "El bloqueador se presenta en línea y, en el último instante, entra a diagonal. El defensor abandona diagonal y sale a línea.",
    blockZone: "B: cambia a diagonal",
    defenseZone: "D: cambia a línea",
    supportZone: "Corta",
    fakeCoverage: "linea",
    finalCoverage: "diagonal",
    attack: "diagonal",
    correction:
      "Bloqueador y defensor deben cambiar a la vez. Si uno rectifica y el otro no, ambos terminan protegiendo la misma zona."
  }
];

const signalCoverage = {
  linea: {
    block: { x: 39, y: 8, w: 10, h: 37, player: [47, 25] },
    defense: { x: 7, y: 46, w: 37, h: 45, player: [25, 69] },
    support: { x: 34, y: 36, w: 14, h: 24 },
    ball: [43, 25]
  },
  diagonal: {
    block: { x: 35, y: 25, w: 14, h: 35, player: [47, 36] },
    defense: { x: 7, y: 8, w: 34, h: 36, player: [24, 25] },
    support: { x: 34, y: 40, w: 14, h: 23 },
    ball: [43, 41]
  },
  pull: {
    block: { x: 31, y: 31, w: 18, h: 31, player: [39, 45] },
    defense: { x: 7, y: 52, w: 36, h: 39, player: [24, 70] },
    support: { x: 8, y: 10, w: 31, h: 38 },
    ball: [31, 38]
  }
};

function buildSignalSteps(mode) {
  const final = signalCoverage[mode.finalCoverage];
  const shown = signalCoverage[mode.fakeCoverage || mode.finalCoverage];
  const isAdvanced = Boolean(mode.fakeCoverage);
  const receiveSide = 27;
  const attackTarget =
    mode.attack === "diagonal" ? "la diagonal" : mode.attack === "línea" ? "la línea" : "una bola controlada";

  return [
    {
      name: "Pre-saque",
      caption: `B enseña ${mode.handLabel.toLowerCase()}. D confirma la responsabilidad y prepara el saque.`,
      players: { blocker: [46, 50], defender: [10, 73], attacker: [78, receiveSide], setter: [80, 70] },
      ball: [10, 73],
      coverage: null
    },
    {
      name: "Saque y recepción",
      caption: "D saca y entra sin quedarse mirando. El atacante rival recibe; B identifica quién atacará.",
      players: { blocker: [47, 39], defender: [24, 61], attacker: [76, receiveSide], setter: [79, 70] },
      ball: [76, receiveSide],
      coverage: null
    },
    {
      name: "Colocación",
      caption: "La recepción va al colocador rival. B se desplaza por la red y D recupera el centro útil de la cancha.",
      players: { blocker: [47, 33], defender: [29, 52], attacker: [65, receiveSide], setter: [71, 55] },
      ball: [71, 55],
      coverage: null
    },
    {
      name: isAdvanced ? "Mostrar engaño" : "Leer ataque",
      caption: isAdvanced
        ? `B muestra bloqueo ${mode.fakeCoverage}; D ocupa la zona complementaria sin adelantarse al cambio.`
        : `B se alinea con el atacante. D parte desde el centro y empieza a salir hacia ${mode.defenseZone.replace("D: ", "").toLowerCase()}.`,
      players: {
        blocker: shown.block.player,
        defender: shown.defense.player,
        attacker: [58, receiveSide],
        setter: [69, 55]
      },
      ball: [60, receiveSide],
      coverage: shown,
      coverageTone: isAdvanced ? "fake" : "active"
    },
    {
      name: isAdvanced ? "Rectificación" : "Posición final",
      caption: isAdvanced
        ? `En el último instante B cambia a ${mode.blockZone.replace("B: cambia a ", "")} y D cambia a ${mode.defenseZone.replace("D: cambia a ", "")}.`
        : `Antes del golpeo, B ya tapa ${mode.blockZone.replace("B: ", "")} y D está equilibrado en ${mode.defenseZone.replace("D: ", "")}.`,
      players: {
        blocker: final.block.player,
        defender: final.defense.player,
        attacker: [58, receiveSide],
        setter: [69, 55]
      },
      ball: [57, receiveSide],
      coverage: final,
      coverageTone: "active"
    },
    {
      name: "Ataque rival",
      caption: `El rival ataca ${attackTarget}. La pareja mantiene el reparto: una zona la cierra B y la complementaria la defiende D.`,
      players: {
        blocker: final.block.player,
        defender: final.defense.player,
        attacker: [58, receiveSide],
        setter: [69, 55]
      },
      ball: final.ball,
      coverage: final,
      coverageTone: "active"
    }
  ];
}

const settingModes = [
  {
    id: "contigo",
    label: "Contigo",
    badge: "CONTIGO",
    title: "Pase cerca del colocador y salida inmediata",
    meaning:
      "La pelota queda cerca de la ventana del colocador. Después de soltarla, el colocador abandona el carril para que el receptor pueda entrar a atacar.",
    receiverStart: [20, 68],
    setterStart: [25, 28],
    target: [47, 50],
    targetZone: { x: 41, y: 39, w: 8, h: 22 },
    attackerPrep: [26, 66],
    attackerApproach: [39, 56],
    setterContact: [40, 50],
    setterExit: [35, 32],
    setterCoverage: [38, 37],
    facing: 0,
    look: "Mira al punto de ataque y mantiene pecho y hombros orientados hacia la red.",
    set:
      "Contacta por encima de la frente y deja una bola alta junto a su posición, sin pegarla a la red.",
    attacker:
      "Después de recibir, se separa por detrás del balón, espera que el colocador salga y entra con una carrera corta sin cruzarlo.",
    correction:
      "El colocador no debe quedarse debajo del atacante. Suelta, sale hacia el lado contrario de la carrera y se coloca en cobertura."
  },
  {
    id: "abierta",
    label: "Abierta",
    badge: "ABIERTA",
    title: "Pase hacia la línea para crear espacio",
    meaning:
      "La colocación viaja hacia la línea del mismo lado del receptor. El atacante se abre antes de entrar para ver el bloqueo y disponer de una carrera más larga.",
    receiverStart: [20, 66],
    setterStart: [25, 27],
    target: [47, 72],
    targetZone: { x: 41, y: 61, w: 8, h: 23 },
    attackerPrep: [25, 74],
    attackerApproach: [39, 71],
    setterContact: [40, 49],
    setterExit: [35, 34],
    setterCoverage: [38, 45],
    facing: 36,
    look: "Gira pies, pecho y hombros hacia la línea del receptor antes del contacto.",
    set:
      "Empuja alto hacia fuera, al mismo lado del receptor, con margen dentro de la antena imaginaria.",
    attacker:
      "Tras recibir, sale hacia su línea, frena fuera de la trayectoria y después entra hacia la red con dos pasos acelerados.",
    correction:
      "Si el atacante corre directo desde la recepción, llega debajo del balón. Primero debe abrirse y luego atacar hacia delante."
  },
  {
    id: "atras",
    label: "Atrás",
    badge: "ATRÁS",
    title: "Pase detrás del colocador sin revelar pronto",
    meaning:
      "El balón sale por detrás de la posición del colocador. El atacante ocupa el lado opuesto y evita cruzarse por delante de su compañero.",
    receiverStart: [20, 30],
    setterStart: [25, 72],
    target: [47, 79],
    targetZone: { x: 41, y: 68, w: 8, h: 23 },
    attackerPrep: [25, 81],
    attackerApproach: [39, 78],
    setterContact: [40, 50],
    setterExit: [34, 36],
    setterCoverage: [38, 62],
    facing: 0,
    look: "Mantiene pecho hacia la red y ojos en el balón; no gira la cabeza para buscar al atacante.",
    set:
      "Recibe el balón sobre la frente y termina la extensión de manos hacia atrás, con altura suficiente para que el atacante vea el bloqueo.",
    attacker:
      "Después de recibir, rodea por detrás del colocador, espera en el lado opuesto y entra sin atravesar su zona de apoyo.",
    correction:
      "La llamada debe llegar antes del contacto. Si el atacante cruza por delante o el colocador gira el cuerpo, la jugada pierde espacio y sorpresa."
  }
];

function buildSettingSteps(mode) {
  const server = [88, 50];
  const rivalBlockStart = [54, 50];
  const receptionBall = mode.receiverStart;
  const transitionBall = [34, 50];
  const contactBall = [41, 50];

  return [
    {
      name: "Base de recepción",
      caption: "A y C reparten el campo. A sabe qué pasillo recibe; C espera sin adelantarse hasta escuchar la llamada.",
      players: {
        attacker: mode.receiverStart,
        setter: mode.setterStart,
        server,
        blocker: rivalBlockStart
      },
      ball: server,
      facing: mode.facing,
      zones: { reception: true, window: false, target: false }
    },
    {
      name: "Recepción",
      caption: "A canta “Mía”, orienta la plataforma hacia el centro útil y deja el primer toque separado de la red.",
      players: {
        attacker: mode.receiverStart,
        setter: [30, 42],
        server,
        blocker: rivalBlockStart
      },
      ball: receptionBall,
      facing: mode.facing,
      zones: { reception: true, window: true, target: false }
    },
    {
      name: "Entrada del colocador",
      caption: "C entra con una ruta curva, se coloca debajo y ligeramente detrás del balón y frena antes de tocar.",
      players: {
        attacker: mode.attackerPrep,
        setter: mode.setterContact,
        server,
        blocker: rivalBlockStart
      },
      ball: transitionBall,
      facing: mode.facing,
      zones: { reception: false, window: true, target: false }
    },
    {
      name: `Llamada “${mode.label}”`,
      caption: `${mode.look} A termina de abrirse y deja libre la trayectoria del segundo toque.`,
      players: {
        attacker: mode.attackerPrep,
        setter: mode.setterContact,
        server,
        blocker: [54, mode.target[1]]
      },
      ball: contactBall,
      facing: mode.facing,
      zones: { reception: false, window: true, target: true }
    },
    {
      name: "Colocación y salida",
      caption: `${mode.set} C sale inmediatamente del carril de ataque.`,
      players: {
        attacker: mode.attackerApproach,
        setter: mode.setterExit,
        server,
        blocker: [54, mode.target[1]]
      },
      ball: mode.target,
      facing: mode.facing,
      zones: { reception: false, window: false, target: true }
    },
    {
      name: "Carrera y ataque",
      caption: `${mode.attacker} C queda preparado para cubrir un toque de bloqueo.`,
      players: {
        attacker: mode.target,
        setter: mode.setterCoverage,
        server,
        blocker: [54, mode.target[1]]
      },
      ball: [52, mode.target[1]],
      facing: mode.facing,
      zones: { reception: false, window: false, target: true }
    }
  ];
}

let movementPhases = [
  {
    id: "saque",
    label: "Saque",
    title: "Fase de saque: servir, entrar y defender",
    goal:
      "El saque termina cuando el equipo ya está organizado para bloquear y defender, no cuando el balón cruza la red.",
    principles: [
      "El sacador debe saber su destino defensivo antes de sacar.",
      "El compañero en red no mira solo la pelota: lee calidad de recepción rival.",
      "Si el saque desordena al rival, la pareja avanza; si el rival recibe perfecto, se respeta el sistema de bloqueo-defensa."
    ],
    steps: [
      {
        role: "Sacador",
        start: "Fondo, detrás de la línea, alineado con el objetivo de saque.",
        trigger: "Contacto de saque.",
        move: "Entra con tres o cuatro pasos controlados sin invadir el centro del compañero.",
        destination: "Base defensiva: diagonal media o fondo según señal de bloqueo.",
        cue: "Entra / diagonal / línea",
        correction: "No quedarse mirando el saque. Entrar antes del segundo toque rival."
      },
      {
        role: "Bloqueador",
        start: "Cerca de red, frente al atacante probable.",
        trigger: "Calidad de recepción rival.",
        move: "Ajusta lateralmente, decide bloqueo o retirada y comunica.",
        destination: "Red para bloquear o zona corta-media si hace pull.",
        cue: "Línea / diagonal / puño",
        correction: "La decisión debe aparecer antes de que el atacante salte."
      },
      {
        role: "Defensor",
        start: "Zona media-profunda del lado asignado.",
        trigger: "Señal del bloqueador y carrera del atacante.",
        move: "Se coloca donde el bloqueo no cubre.",
        destination: "Línea, diagonal o corta según sistema.",
        cue: "Tengo fondo / tengo corta",
        correction: "No cambiar de zona tarde por intuición; confiar en la señal."
      }
    ]
  },
  {
    id: "recepcion",
    label: "Recepción",
    title: "Fase de recepción: prioridad, primer toque y entrada del colocador",
    goal:
      "Convertir el saque rival en una colocación jugable con tres decisiones claras: quién recibe, quién coloca y quién cubre.",
    principles: [
      "El balón entre dos se decide por ángulo de entrada, no por orgullo.",
      "El no receptor se mueve después de confirmar que no va a recibir.",
      "La recepción perfecta no es pegada a la red; es alta, estable y colocable."
    ],
    steps: [
      {
        role: "Receptor A",
        start: "Fondo-línea o centro-línea según plan.",
        trigger: "Saque a su zona o al pasillo central con mejor ángulo.",
        move: "Primer paso hacia el balón, plataforma temprano, salida fuera de la línea del pase.",
        destination: "Carrera de ataque con dos pasos y hombros abiertos.",
        cue: "Mía / alta",
        correction: "Recibir y quedarse parado elimina la carrera de ataque."
      },
      {
        role: "Receptor B",
        start: "Diagonal media, responsable de balones profundos y pasillo.",
        trigger: "Compañero confirma recepción.",
        move: "Entra por curva hacia zona de colocación, frenando antes de tocar.",
        destination: "Ventana de colocación a 2-3 metros de red.",
        cue: "Voy / centro",
        correction: "No correr por debajo del balón ni colocar de espaldas al atacante."
      },
      {
        role: "Atacante",
        start: "Lugar donde terminó el primer toque.",
        trigger: "Colocador toma el segundo toque.",
        move: "Se separa medio paso, mira bloqueo/defensa y entra a golpear.",
        destination: "Punto de ataque equilibrado, con cobertura cerca.",
        cue: "Línea / diagonal / corta",
        correction: "Si la colocación es mala, atacar zona grande y mantener continuidad."
      }
    ]
  },
  {
    id: "ataque",
    label: "Ataque",
    title: "Fase de ataque: carrera, golpeo y cobertura",
    goal:
      "Atacar con intención sin perder organización si el rival defiende o toca el bloqueo.",
    principles: [
      "La carrera empieza fuera de la línea del balón para ver campo.",
      "La llamada del compañero informa, no obliga: el atacante decide con la última imagen.",
      "Después del golpeo, el equipo vuelve a defender; el punto no termina en el salto."
    ],
    steps: [
      {
        role: "Atacante",
        start: "Separado del balón, con hombros hacia el campo rival.",
        trigger: "Colocación sale de manos del compañero.",
        move: "Últimos dos pasos aceleran hacia el punto de golpeo.",
        destination: "Contacto alto y aterrizaje listo para transición.",
        cue: "Veo / línea / corta / manos",
        correction: "No entrar debajo del balón; reduce visión y opciones."
      },
      {
        role: "Colocador",
        start: "Debajo de la ventana de colocación.",
        trigger: "Suelta el segundo toque.",
        move: "Cae hacia el lado del atacante y baja centro de gravedad.",
        destination: "Cobertura corta frente al bloqueo.",
        cue: "Cubro",
        correction: "Cobertura detrás del atacante llega tarde al balón tocado."
      },
      {
        role: "Pareja",
        start: "Sistema ofensivo formado.",
        trigger: "Bloqueo toca o rival defiende.",
        move: "Uno toma primer balón, el otro prepara segundo toque.",
        destination: "Nueva transición de ataque.",
        cue: "Toque / otra / alta",
        correction: "Tras defensa rival, no discutir la elección del ataque: reorganizar."
      }
    ]
  },
  {
    id: "bloqueo",
    label: "Bloqueo-defensa",
    title: "Fase bloqueo-defensa: repartir la pista",
    goal:
      "El bloqueo quita una opción y la defensa protege la opción complementaria. La pareja debe parecer un solo jugador grande.",
    principles: [
      "La señal define responsabilidad, no adivina el futuro.",
      "El defensor empieza donde puede ver atacante y bloqueo a la vez.",
      "El bloqueador salta vertical y estable; si persigue, abre huecos."
    ],
    steps: [
      {
        role: "Bloqueador",
        start: "Frente al hombro atacante.",
        trigger: "Colocación rival y carrera del atacante.",
        move: "Ajusta medio paso y salta cerrando línea o diagonal.",
        destination: "Manos sobre zona elegida, caída lista para abrirse.",
        cue: "Línea / ángulo",
        correction: "Mostrar señal y luego hacer otra cosa rompe la defensa."
      },
      {
        role: "Defensor",
        start: "Zona media-profunda opuesta al bloqueo.",
        trigger: "Señal y brazo atacante.",
        move: "Ajusta un paso antes del golpeo; después reacciona.",
        destination: "Zona libre principal o corta si el brazo baja.",
        cue: "Tengo diagonal / tengo línea",
        correction: "Si corre antes del golpeo, el atacante ve el hueco."
      },
      {
        role: "Equipo",
        start: "Bloqueo-defensa formado.",
        trigger: "Defensa positiva.",
        move: "El no defensor entra a colocar; el defensor se abre para atacar.",
        destination: "Contraataque con carrera corta.",
        cue: "Alta / tiempo",
        correction: "La defensa perfecta sin transición se convierte en balón regalado."
      }
    ]
  },
  {
    id: "transicion",
    label: "Transición",
    title: "Fase de transición: defender y volver a atacar",
    goal:
      "Convertir un balón defendido en una acción ofensiva con orden, incluso cuando la defensa sale baja o separada.",
    principles: [
      "El segundo toque debe comprar tiempo si el primer toque es difícil.",
      "Quien no toca el balón se convierte automáticamente en siguiente solución.",
      "La comunicación cambia de táctica a supervivencia: claro, corto y temprano."
    ],
    steps: [
      {
        role: "Defensor",
        start: "Base defensiva asignada.",
        trigger: "Contacto defensivo propio.",
        move: "Después de defender, se abre para opción de ataque o apoyo.",
        destination: "Carrera corta si la defensa fue positiva; apoyo si fue negativa.",
        cue: "Alta / fuera / puedo",
        correction: "Defender y quedarse en el suelo cancela la transición."
      },
      {
        role: "Compañero",
        start: "Red o zona de bloqueo.",
        trigger: "Defensa del compañero.",
        move: "Sale de la red hacia el balón con carrera curva.",
        destination: "Segundo toque alto al lado del atacante.",
        cue: "Voy / tiempo",
        correction: "Entrar recto debajo del balón limita la dirección del pase."
      },
      {
        role: "Atacante de transición",
        start: "Lugar posterior a la defensa.",
        trigger: "Colocación alta o balón salvado.",
        move: "Prioriza equilibrio sobre potencia.",
        destination: "Ataque a zona grande o free ball profunda.",
        cue: "Dentro / hueco",
        correction: "No regalar corto sin mirar: si el rival está cerca, es balón fácil."
      }
    ]
  },
  {
    id: "uno",
    label: "1-1",
    title: "Fase 1-1: aprender espacio y recuperación",
    goal:
      "Construir hábitos individuales que después hacen posible el 2-2: base, lectura, orientación y recuperación.",
    principles: [
      "La referencia no es el centro matemático, es el centro útil.",
      "Cada golpeo tiene salida y regreso.",
      "La prioridad es tocar equilibrado antes que ganar fuerte."
    ],
    steps: [
      {
        role: "Jugador",
        start: "Centro útil del campo reducido.",
        trigger: "Golpeo rival.",
        move: "Primer paso hacia trayectoria, plataforma o toque alto.",
        destination: "Zona de contacto equilibrada.",
        cue: "Centro / alta",
        correction: "No dar el primer paso hacia atrás salvo balón claramente profundo."
      },
      {
        role: "Jugador",
        start: "Después de defender.",
        trigger: "Balón sube jugable.",
        move: "Se separa para tener ángulo de ataque.",
        destination: "Punto de golpeo con visión del rival.",
        cue: "Hueco / dentro",
        correction: "Atacar desde debajo del balón quita control."
      },
      {
        role: "Jugador",
        start: "Después del golpeo.",
        trigger: "Balón cruza la red.",
        move: "Recupera con pasos cruzados cortos.",
        destination: "Centro útil listo para defender.",
        cue: "Vuelvo",
        correction: "Celebrar o lamentar antes de recuperar concede puntos gratis."
      }
    ]
  }
];

movementPhases = movementPhases.filter((phase) => phase.id !== "uno");

const communicationProtocol = [
  {
    phase: "Antes del saque propio",
    aim: "Crear plan común antes de poner el balón en juego.",
    script: "Objetivo de saque + señal de bloqueo + destino del sacador.",
    example: "Centro, bloqueo línea, entro diagonal.",
    must: ["Mirada breve", "Señal clara", "Confirmación verbal"],
    avoid: "Sacar sin que el compañero sepa quién defiende qué zona."
  },
  {
    phase: "Antes de recibir",
    aim: "Evitar dudas en el primer toque.",
    script: "Responsable del pasillo central + zona corta + plan si la recepción sale mala.",
    example: "Yo centro, tú corta; si va mal, alta.",
    must: ["Prioridad del centro", "Volumen de voz", "Plan B"],
    avoid: "Dos jugadores entrando al mismo balón sin llamada."
  },
  {
    phase: "Durante la recepción",
    aim: "Nombrar propietario del balón y calidad del primer toque.",
    script: "Mía/Tuya + alta/pegada/fuera.",
    example: "Mía, alta. Voy.",
    must: ["Una palabra primero", "Información útil después", "No gritar frases largas"],
    avoid: "Hablar cuando el compañero ya está golpeando."
  },
  {
    phase: "Durante la colocación",
    aim: "Dar al atacante una imagen antes del salto.",
    script: "Dirección disponible + cobertura.",
    example: "Línea, cubro.",
    must: ["Llamada temprana", "Cobertura real", "Aceptar decisión del atacante"],
    avoid: "Cantar una zona tarde, cuando el atacante ya no puede ajustar."
  },
  {
    phase: "Bloqueo-defensa",
    aim: "Repartir pista sin contradicciones.",
    script: "Señal antes del saque + confirmación si cambia.",
    example: "Línea. Tengo diagonal.",
    must: ["Señal visible", "Responsabilidad opuesta", "Aviso si hay pull"],
    avoid: "Bloquear línea mientras el defensor también protege línea."
  },
  {
    phase: "Transición",
    aim: "Pasar de sobrevivir a organizar ataque.",
    script: "Voy/tuya + tiempo/alta + dirección simple.",
    example: "Voy, tiempo, hueco.",
    must: ["Solución rápida", "Segundo toque alto", "Nuevo reparto de roles"],
    avoid: "Intentar una bola perfecta desde defensa negativa."
  },
  {
    phase: "Entre puntos",
    aim: "Corregir sin cortar ritmo emocional.",
    script: "Una información + una decisión siguiente.",
    example: "Nos están sacando corto; próxima, tú avisas antes.",
    must: ["Mensaje corto", "Responsabilidad compartida", "Cerrar con mirada"],
    avoid: "Explicar tres errores antes del siguiente saque."
  }
];

const callbook = [
  {
    call: "Mía",
    moment: "Antes del primer toque",
    meaning: "Yo voy a tocar primero.",
    action: "El compañero sale de la trayectoria y se prepara para colocar."
  },
  {
    call: "Tuya",
    moment: "Antes del primer toque",
    meaning: "El compañero va a tocar primero.",
    action: "Quien llama libera espacio y se prepara para el siguiente rol."
  },
  {
    call: "Centro mía",
    moment: "Antes del saque rival",
    meaning: "Además de mi zona, yo asumo el balón que cae en el centro.",
    action: "El compañero respeta esa prioridad y protege su lateral y la corta."
  },
  {
    call: "Centro tuya",
    moment: "Antes del saque rival",
    meaning: "El compañero asume el balón que cae en el centro.",
    action: "Quien llama no invade el pasillo central salvo corrección tardía."
  },
  {
    call: "Corta",
    moment: "Mientras viaja el saque o el ataque",
    meaning: "El balón va cerca de la red.",
    action: "Primer paso hacia delante y carrera decidida hasta la zona corta."
  },
  {
    call: "Fuera",
    moment: "Cuando se lee la trayectoria",
    meaning: "El balón va a salir del campo.",
    action: "Nadie toca el balón; se frena la carrera y se confirma visualmente."
  },
  {
    call: "Contigo",
    moment: "Durante la colocación",
    meaning: "La colocación cae cerca de la posición del colocador.",
    action: "El colocador se quita de la carrera para dejar entrar al atacante."
  },
  {
    call: "Abierta",
    moment: "Antes del segundo toque",
    meaning: "El colocador enviará el balón hacia la línea.",
    action: "El atacante se abre fuera de la trayectoria y entra con espacio."
  },
  {
    call: "Atrás",
    moment: "Antes del segundo toque",
    meaning: "El colocador enviará el balón hacia atrás.",
    action: "El atacante espera detrás del colocador y ajusta la carrera sin cruzarse."
  }
];

const strategies = [
  {
    title: "Saque al pasillo central",
    phase: "Saque",
    when: "Contra parejas que dudan en balones entre dos o cuando el colocador no receptor entra tarde.",
    execution: [
      "Servir al hombro interno del receptor más débil.",
      "Sacador entra a base defensiva asignada.",
      "Bloqueador lee si la recepción sale pegada, separada o fuera de sistema."
    ],
    movement: "Inicio: sacador en fondo. Destino: diagonal media o fondo. Compañero: red, preparado para bloquear o retirarse.",
    risk: "Si el saque queda cómodo al centro sin presión, el rival tiene todos los ángulos.",
    calls: ["Centro", "Entra", "Línea/Ángulo"]
  },
  {
    title: "Saque corto al bloqueador",
    phase: "Saque",
    when: "Si el bloqueador rival es alto, pesado de piernas o prefiere atacar desde carrera larga.",
    execution: [
      "Saque flotado corto al espacio delante del bloqueador.",
      "Defensa avanza medio paso porque aumenta opción de ataque sin potencia.",
      "Bloqueador propio puede hacer pull si el rival ataca sin carrera."
    ],
    movement: "Inicio: receptor rival cerca de red. Destino esperado: obligado a recibir y atacar sin separación.",
    risk: "Si queda demasiado alto, permite recepción perfecta y ataque rápido.",
    calls: ["Corta", "Puño"]
  },
  {
    title: "Bloqueo línea + defensa diagonal",
    phase: "Defensa",
    when: "Contra atacantes que puntúan fuerte por línea o cuando la línea es corta por viento/sol.",
    execution: [
      "Bloqueador cierra mano exterior sobre línea.",
      "Defensor toma diagonal larga y lee muñeca.",
      "Si el brazo baja, defensor cambia a corta."
    ],
    movement: "Inicio: bloqueador frente al atacante; defensor diagonal media. Destino: línea cerrada y diagonal protegida.",
    risk: "Regala línea si el bloqueador salta hacia dentro.",
    calls: ["Línea", "Tengo diagonal", "Corta"]
  },
  {
    title: "Bloqueo diagonal + defensa línea",
    phase: "Defensa",
    when: "Contra atacantes de diagonal dominante o cuando el viento empuja el ataque cruzado.",
    execution: [
      "Bloqueador ocupa ángulo natural.",
      "Defensor se abre a línea y protege shot profundo.",
      "La señal debe esconderse hasta el último momento posible."
    ],
    movement: "Inicio: bloqueador centrado; defensor línea media. Destino: diagonal cerrada y línea defendida.",
    risk: "Si el defensor sale tarde a línea, el atacante tiene golpe fácil.",
    calls: ["Ángulo", "Línea", "Alta"]
  },
  {
    title: "Pull o falso bloqueo",
    phase: "Defensa",
    when: "Recepción rival mala, colocación separada o atacante sin carrera real.",
    execution: [
      "Bloqueador confirma el puño y sale antes del salto rival.",
      "Sale con pasos cruzados a zona corta-media.",
      "Defensor profundo no invade corta hasta confirmar el pull."
    ],
    movement: "Inicio: bloqueador en red. Destino: zona corta-media. Defensor mantiene fondo.",
    risk: "Llegar a medio camino y no cubrir ni red ni defensa.",
    calls: ["Puño", "Corta"]
  },
  {
    title: "Side-out seguro con ataque a zona grande",
    phase: "Recepción",
    when: "Tras recepción negativa, viento difícil o marcador de presión.",
    execution: [
      "Primer toque alto y separado.",
      "Segundo toque con margen, no pegado a red.",
      "Ataque profundo al centro-fondo para recuperar organización."
    ],
    movement: "Inicio: receptor en desequilibrio. Destino: ataque controlado y recuperación inmediata al centro.",
    risk: "Intentar punto perfecto con pelota imperfecta.",
    calls: ["Alta", "Dentro", "Cubro"]
  },
  {
    title: "Free ball con ataque al hueco",
    phase: "Transición",
    when: "El rival regala balón alto o defiende sin posibilidad de atacar.",
    execution: [
      "Primer toque al centro con altura.",
      "Colocador mira defensa rival antes de colocar.",
      "Atacante decide entre línea, diagonal corta o fondo libre."
    ],
    movement: "Inicio: defensa organizada. Destino: atacante separado, colocador en cobertura y hueco identificado.",
    risk: "Acelerar una bola que pedía construir.",
    calls: ["Tiempo", "Hueco", "Cubro"]
  },
  {
    title: "Ataque contra viento",
    phase: "Ataque",
    when: "El viento frena el balón o cambia trayectorias de colocación.",
    execution: [
      "Pedir colocación más alta y centrada.",
      "Atacar con mano firme a zona profunda segura.",
      "Evitar shots flotados si el viento los devuelve."
    ],
    movement: "Inicio: atacante más paciente. Destino: golpeo equilibrado, caída lista para defender.",
    risk: "Usar golpe técnico suave sin control del viento.",
    calls: ["Alta", "Fondo", "Dentro"]
  }
];

let modules = [
  {
    category: "Comunicación",
    title: "Sistema de llamadas por fase",
    level: "Alto rendimiento",
    description:
      "Define qué se dice antes del saque, durante recepción, en colocación, ataque, bloqueo-defensa y transición.",
    points: ["Prioridad antes del punto", "Una palabra en vuelo", "Corrección breve entre puntos", "Confirmación con mirada"]
  },
  {
    category: "Movimiento",
    title: "Inicio, disparador y destino",
    level: "Táctico",
    description:
      "Cada movimiento nace de un disparador: saque, calidad de recepción, señal de bloqueo, defensa positiva o balón regalado.",
    points: ["Punto de inicio", "Primer paso", "Ruta sin cruzarse", "Destino útil"]
  },
  {
    category: "Estrategia",
    title: "Plan de saque y primera defensa",
    level: "Competición",
    description:
      "El saque se conecta con bloqueo-defensa: objetivo del saque, entrada del sacador y zona que cubre el compañero.",
    points: ["Saque al centro", "Saque corto", "Saque al cuerpo", "Entrada defensiva"]
  },
  {
    category: "Sistema 2-2",
    title: "Bloqueo y defensa coordinados",
    level: "Competición",
    description:
      "Señal clara, bloqueo disciplinado y defensor ocupando la zona que el bloqueo no cubre.",
    points: ["Bloqueo línea", "Bloqueo diagonal", "Pull", "Transición tras defensa"]
  },
  {
    category: "Posicionamiento",
    title: "Base defensiva y centro útil",
    level: "Fundamento",
    description:
      "La posición correcta depende de altura del balón, hombro atacante, viento y espacio que cubre el compañero.",
    points: ["Centro útil", "Profundidad", "Zona corta", "Relación línea-diagonal"]
  },
  {
    category: "Sistema 1-1",
    title: "Lectura y recuperación individual",
    level: "Iniciación",
    description:
      "Reduce variables para entender trayectoria, orientación corporal y vuelta a base después de cada golpeo.",
    points: ["Campo reducido", "Primer paso", "Ataque seguro", "Recuperación"]
  },
  {
    category: "Entreno",
    title: "Drills con comunicación obligatoria",
    level: "Sesión",
    description:
      "Ejercicios donde el punto solo vale si la pareja comunica antes, durante y después de la acción.",
    points: ["Saque con plan", "Recepción al objetivo", "Ataque nombrado", "Corrección de 5 segundos"]
  },
  {
    category: "Movimiento",
    title: "Cobertura después del ataque",
    level: "Pareja",
    description:
      "El colocador protege el balón tocado, el atacante aterriza listo y ambos se reorganizan si el rival defiende.",
    points: ["Cubro", "Toque", "Otra", "Nueva transición"]
  },
  {
    category: "Estrategia",
    title: "Gestión de marcador y riesgo",
    level: "Avanzado",
    description:
      "No se juega igual con ventaja, punto de cambio o set point. El riesgo debe ser compartido y consciente.",
    points: ["Saque seguro", "Ataque a zona grande", "Bloqueo disciplinado", "Reset emocional"]
  }
];

modules = modules.filter((module) => !["Sistema 1-1", "Entreno"].includes(module.category));

const calls = callbook.map((item) => item.call);

const trainingPlans = {
  base: [
    {
      time: "12 min",
      title: "Activación con palabras de pista",
      text: "Parejas a media pista: cada contacto debe llevar llamada de propiedad o calidad.",
      points: ["Mía/Tuya", "Alta", "Centro", "Vuelvo"]
    },
    {
      time: "18 min",
      title: "1-1 con centro útil",
      text: "Campo reducido. Punto válido solo si el jugador recupera base antes del siguiente contacto rival.",
      points: ["Primer paso", "Equilibrio", "Recuperación", "Ataque seguro"]
    },
    {
      time: "22 min",
      title: "Recepción y entrada del colocador",
      text: "Saque fácil a tres zonas. El no receptor debe cantar 'voy' y llegar a colocar con hombros orientados.",
      points: ["Prioridad", "Ruta curva", "Ventana central", "Cobertura"]
    }
  ],
  intermedio: [
    {
      time: "14 min",
      title: "Saque con entrada defensiva",
      text: "El sacador nombra objetivo y destino antes del saque; el compañero confirma bloqueo o pull.",
      points: ["Centro/corto", "Entro", "Línea/ángulo", "Base defensiva"]
    },
    {
      time: "24 min",
      title: "Side-out con ataque nombrado",
      text: "La pareja solo suma si recepción, colocación y dirección de ataque tienen llamada antes del golpeo.",
      points: ["Mía", "Voy", "Línea/diagonal", "Cubro"]
    },
    {
      time: "20 min",
      title: "Bloqueo-defensa por señales",
      text: "Alternar bloqueo línea y diagonal. El defensor debe ocupar la zona complementaria sin cambiar tarde.",
      points: ["Señal", "Disciplina", "Corta", "Transición"]
    }
  ],
  competicion: [
    {
      time: "18 min",
      title: "Scouting y plan de saque",
      text: "Diseñar tres planes: receptor débil, pasillo central y saque corto al bloqueador.",
      points: ["Objetivo", "Entrada", "Primer bloqueo", "Ajuste por viento"]
    },
    {
      time: "28 min",
      title: "Rally de alto rendimiento",
      text: "Juego condicionado: cada pareja debe comunicar sistema antes del saque y corrección después del punto.",
      points: ["Pre-punto", "Transición", "Cobertura", "Reset"]
    },
    {
      time: "24 min",
      title: "Set final con tareas tácticas",
      text: "Set a 15 con bonus por ejecutar la estrategia acordada y penalización por errores de comunicación.",
      points: ["Presión", "Riesgo", "Rutina", "Cierre emocional"]
    }
  ]
};

const moduleFilters = ["Todos", "Comunicación", "Estrategia", "Movimiento", "Posicionamiento", "Sistema 2-2"];
const levels = [
  { id: "base", label: "Base" },
  { id: "intermedio", label: "Intermedio" },
  { id: "competicion", label: "Competición" }
];

let activeFilter = "Todos";
let activeLevel = "base";
let activeMovement = movementPhases[0].id;
let activeSignal = signalModes[0].id;
let activeSignalStep = 0;
let signalPlaying = true;
let signalTimer = null;
let activeSetting = settingModes[0].id;
let activeSettingStep = 0;
let settingPlaying = true;
let settingTimer = null;

function el(tag, className, text) {
  const node = document.createElement(tag);
  if (className) node.className = className;
  if (text) node.textContent = text;
  return node;
}

function escapeHTML(value) {
  return String(value).replace(/[&<>"']/g, (char) => {
    const entities = {
      "&": "&amp;",
      "<": "&lt;",
      ">": "&gt;",
      '"': "&quot;",
      "'": "&#39;"
    };
    return entities[char];
  });
}

const languageKey = "volley-beach-santi-language";
const translations = {
  es: {
    "nav.home": "Inicio",
    "nav.tournament": "Torneo",
    "nav.scoreboard": "Marcador",
    "language.label": "Idioma",
    "cues.eyebrow": "Código de pista",
    "cues.title": "Palabras de pista",
    "cues.description": "Llamadas cortas para decidir rápido quién toca, qué zona cubre cada jugador y cómo se construye el ataque.",
    "cues.current": "Palabras de pista · lista actual",
    "cues.ritual": "Ritual de 5 segundos",
    "cues.ritual1": "Mirada al compañero.",
    "cues.ritual2": "Señal de bloqueo o defensa.",
    "cues.ritual3": "Objetivo de saque.",
    "cues.ritual4": "Confirmación verbal.",
    "cues.ritual5": "Respirar y jugar.",
    "tournament.eyebrow": "Organizador de torneo",
    "tournament.title": "Grupos, cruces y final",
    "tournament.description": "Define los equipos, genera una fase de grupos y simula las eliminatorias hasta tener campeón.",
    "tournament.config": "Configuración",
    "tournament.create": "Crear torneo",
    "tournament.teams": "Equipos",
    "tournament.format": "Formato",
    "tournament.defineTeams": "Definir equipos",
    "tournament.generateGroups": "Generar grupos",
    "tournament.simulateGroups": "Simular grupos",
    "tournament.buildBracket": "Crear eliminatorias",
    "tournament.simulateBracket": "Simular eliminatorias",
    "tournament.phase1": "Fase 1",
    "tournament.groups": "Fase de grupos",
    "tournament.finalPhase": "Fase final",
    "tournament.knockout": "Eliminatorias",
    "scoreboard.eyebrow": "Anotación del partido",
    "scoreboard.title": "Marcador con cambio de lado",
    "scoreboard.description": "Suma o resta puntos durante el partido. Cuando el total llega a un múltiplo de 7, aparece el aviso para cambiar de lado.",
    "scoreboard.team1": "Equipo 1",
    "scoreboard.team2": "Equipo 2",
    "scoreboard.confirmSide": "Confirmar cambio de lado",
    "scoreboard.reset": "Reiniciar marcador",
    "scoreboard.control": "Control",
    "scoreboard.summary": "Resumen del partido",
    "common.reset": "Reiniciar"
  },
  en: {
    "nav.home": "Home",
    "nav.tournament": "Tournament",
    "nav.scoreboard": "Scoreboard",
    "language.label": "Language",
    "cues.eyebrow": "Court code",
    "cues.title": "Court calls",
    "cues.description": "Short calls to decide who plays the ball, which zone each player covers, and how the attack is built.",
    "cues.current": "Court calls · current list",
    "cues.ritual": "5-second ritual",
    "cues.ritual1": "Look at your partner.",
    "cues.ritual2": "Block or defense signal.",
    "cues.ritual3": "Serve target.",
    "cues.ritual4": "Verbal confirmation.",
    "cues.ritual5": "Breathe and play.",
    "tournament.eyebrow": "Tournament organizer",
    "tournament.title": "Groups, brackets and final",
    "tournament.description": "Define the teams, generate a group stage, and simulate the brackets until there is a champion.",
    "tournament.config": "Settings",
    "tournament.create": "Create tournament",
    "tournament.teams": "Teams",
    "tournament.format": "Format",
    "tournament.defineTeams": "Set teams",
    "tournament.generateGroups": "Generate groups",
    "tournament.simulateGroups": "Simulate groups",
    "tournament.buildBracket": "Create brackets",
    "tournament.simulateBracket": "Simulate brackets",
    "tournament.phase1": "Phase 1",
    "tournament.groups": "Group stage",
    "tournament.finalPhase": "Final phase",
    "tournament.knockout": "Brackets",
    "scoreboard.eyebrow": "Match scoring",
    "scoreboard.title": "Scoreboard with side changes",
    "scoreboard.description": "Add or subtract points during the match. At every multiple of 7 total points, a side-change alert appears.",
    "scoreboard.team1": "Team 1",
    "scoreboard.team2": "Team 2",
    "scoreboard.confirmSide": "Confirm side change",
    "scoreboard.reset": "Reset scoreboard",
    "scoreboard.control": "Control",
    "scoreboard.summary": "Match summary",
    "common.reset": "Reset"
  },
  it: {
    "nav.home": "Inizio",
    "nav.tournament": "Torneo",
    "nav.scoreboard": "Segnapunti",
    "language.label": "Lingua",
    "cues.eyebrow": "Codice in campo",
    "cues.title": "Parole di campo",
    "cues.description": "Chiamate brevi per decidere chi gioca la palla, quale zona copre ogni giocatore e come costruire l'attacco.",
    "cues.current": "Parole di campo · lista attuale",
    "cues.ritual": "Rituale di 5 secondi",
    "cues.ritual1": "Sguardo al compagno.",
    "cues.ritual2": "Segnale di muro o difesa.",
    "cues.ritual3": "Obiettivo del servizio.",
    "cues.ritual4": "Conferma verbale.",
    "cues.ritual5": "Respirare e giocare.",
    "tournament.eyebrow": "Organizzatore torneo",
    "tournament.title": "Gironi, tabelloni e finale",
    "tournament.description": "Definisci le squadre, genera una fase a gironi e simula i tabelloni fino al campione.",
    "tournament.config": "Configurazione",
    "tournament.create": "Crea torneo",
    "tournament.teams": "Squadre",
    "tournament.format": "Formato",
    "tournament.defineTeams": "Definisci squadre",
    "tournament.generateGroups": "Genera gironi",
    "tournament.simulateGroups": "Simula gironi",
    "tournament.buildBracket": "Crea tabelloni",
    "tournament.simulateBracket": "Simula tabelloni",
    "tournament.phase1": "Fase 1",
    "tournament.groups": "Fase a gironi",
    "tournament.finalPhase": "Fase finale",
    "tournament.knockout": "Tabelloni",
    "scoreboard.eyebrow": "Punteggio partita",
    "scoreboard.title": "Segnapunti con cambio campo",
    "scoreboard.description": "Aggiungi o togli punti durante la partita. A ogni multiplo di 7 punti totali appare l'avviso di cambio campo.",
    "scoreboard.team1": "Squadra 1",
    "scoreboard.team2": "Squadra 2",
    "scoreboard.confirmSide": "Conferma cambio campo",
    "scoreboard.reset": "Reimposta segnapunti",
    "scoreboard.control": "Controllo",
    "scoreboard.summary": "Riepilogo partita",
    "common.reset": "Reimposta"
  },
  fr: {
    "nav.home": "Accueil",
    "nav.tournament": "Tournoi",
    "nav.scoreboard": "Score",
    "language.label": "Langue",
    "cues.eyebrow": "Code terrain",
    "cues.title": "Mots de terrain",
    "cues.description": "Des appels courts pour décider qui joue le ballon, quelle zone couvre chaque joueur et comment construire l'attaque.",
    "cues.current": "Mots de terrain · liste actuelle",
    "cues.ritual": "Rituel de 5 secondes",
    "cues.ritual1": "Regard vers le partenaire.",
    "cues.ritual2": "Signal de bloc ou défense.",
    "cues.ritual3": "Cible du service.",
    "cues.ritual4": "Confirmation verbale.",
    "cues.ritual5": "Respirer et jouer.",
    "tournament.eyebrow": "Organisateur de tournoi",
    "tournament.title": "Groupes, tableaux et finale",
    "tournament.description": "Définis les équipes, génère une phase de groupes et simule les tableaux jusqu'au champion.",
    "tournament.config": "Configuration",
    "tournament.create": "Créer le tournoi",
    "tournament.teams": "Équipes",
    "tournament.format": "Format",
    "tournament.defineTeams": "Définir équipes",
    "tournament.generateGroups": "Générer groupes",
    "tournament.simulateGroups": "Simuler groupes",
    "tournament.buildBracket": "Créer tableaux",
    "tournament.simulateBracket": "Simuler tableaux",
    "tournament.phase1": "Phase 1",
    "tournament.groups": "Phase de groupes",
    "tournament.finalPhase": "Phase finale",
    "tournament.knockout": "Tableaux",
    "scoreboard.eyebrow": "Score du match",
    "scoreboard.title": "Score avec changement de côté",
    "scoreboard.description": "Ajoute ou retire des points pendant le match. À chaque multiple de 7 points totaux, une alerte de changement de côté apparaît.",
    "scoreboard.team1": "Équipe 1",
    "scoreboard.team2": "Équipe 2",
    "scoreboard.confirmSide": "Confirmer changement",
    "scoreboard.reset": "Réinitialiser score",
    "scoreboard.control": "Contrôle",
    "scoreboard.summary": "Résumé du match",
    "common.reset": "Réinitialiser"
  },
  de: {
    "nav.home": "Start",
    "nav.tournament": "Turnier",
    "nav.scoreboard": "Zähltafel",
    "language.label": "Sprache",
    "cues.eyebrow": "Feldcode",
    "cues.title": "Feldrufe",
    "cues.description": "Kurze Rufe, um schnell zu entscheiden, wer den Ball nimmt, welche Zone jeder deckt und wie der Angriff aufgebaut wird.",
    "cues.current": "Feldrufe · aktuelle Liste",
    "cues.ritual": "5-Sekunden-Ritual",
    "cues.ritual1": "Blick zum Partner.",
    "cues.ritual2": "Block- oder Abwehrsignal.",
    "cues.ritual3": "Aufschlagziel.",
    "cues.ritual4": "Verbale Bestätigung.",
    "cues.ritual5": "Atmen und spielen.",
    "tournament.eyebrow": "Turnierplaner",
    "tournament.title": "Gruppen, Raster und Finale",
    "tournament.description": "Lege Teams fest, erstelle eine Gruppenphase und simuliere die Raster bis zum Champion.",
    "tournament.config": "Einstellungen",
    "tournament.create": "Turnier erstellen",
    "tournament.teams": "Teams",
    "tournament.format": "Format",
    "tournament.defineTeams": "Teams festlegen",
    "tournament.generateGroups": "Gruppen erstellen",
    "tournament.simulateGroups": "Gruppen simulieren",
    "tournament.buildBracket": "Raster erstellen",
    "tournament.simulateBracket": "Raster simulieren",
    "tournament.phase1": "Phase 1",
    "tournament.groups": "Gruppenphase",
    "tournament.finalPhase": "Finalphase",
    "tournament.knockout": "Raster",
    "scoreboard.eyebrow": "Spielstand",
    "scoreboard.title": "Zähltafel mit Seitenwechsel",
    "scoreboard.description": "Addiere oder ziehe Punkte während des Spiels ab. Bei jedem Vielfachen von 7 Gesamtpunkten erscheint ein Seitenwechsel-Hinweis.",
    "scoreboard.team1": "Team 1",
    "scoreboard.team2": "Team 2",
    "scoreboard.confirmSide": "Seitenwechsel bestätigen",
    "scoreboard.reset": "Zähltafel zurücksetzen",
    "scoreboard.control": "Kontrolle",
    "scoreboard.summary": "Spielübersicht",
    "common.reset": "Zurücksetzen"
  }
};

let activeLanguage = localStorage.getItem(languageKey) || "es";

function t(key) {
  return translations[activeLanguage]?.[key] || translations.es[key] || key;
}

function applyTranslations() {
  document.documentElement.lang = activeLanguage;
  document.querySelectorAll("[data-i18n]").forEach((node) => {
    node.textContent = t(node.dataset.i18n);
  });
  document.querySelectorAll(".language-select").forEach((select) => {
    select.value = activeLanguage;
  });
}

function setupLanguageSwitcher() {
  document.querySelectorAll(".language-select").forEach((select) => {
    select.value = activeLanguage;
    select.addEventListener("change", () => {
      activeLanguage = select.value;
      localStorage.setItem(languageKey, activeLanguage);
      applyTranslations();
      renderScoreboard();
      renderKnockoutStage();
    });
  });
  applyTranslations();
}

function listHTML(items) {
  return `<ul>${items.map((item) => `<li>${item}</li>`).join("")}</ul>`;
}

function renderScenarioTabs() {
  const tabs = document.querySelector("#scenarioTabs");
  tabs.innerHTML = "";

  scenarios.forEach((scenario) => {
    const button = el("button", "tab-button", scenario.label);
    button.type = "button";
    button.setAttribute("role", "tab");
    button.setAttribute("aria-selected", String(scenario.id === activeScenario));
    button.addEventListener("click", () => {
      activeScenario = scenario.id;
      renderScenarioTabs();
      renderScenario();
    });
    tabs.appendChild(button);
  });
}

function renderScenario() {
  const scenario = scenarios.find((item) => item.id === activeScenario) || scenarios[0];
  const detail = document.querySelector("#scenarioDetail");
  const zonesLayer = document.querySelector("#zonesLayer");
  const markersLayer = document.querySelector("#markersLayer");
  const routeLayer = document.querySelector("#routeLayer");

  zonesLayer.innerHTML = "";
  markersLayer.innerHTML = "";
  routeLayer.innerHTML = "";

  routeLayer.innerHTML = `
    <defs>
      <marker id="arrowMove" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="5" markerHeight="5" orient="auto-start-reverse">
        <path d="M 0 0 L 10 5 L 0 10 z" fill="#276ef1"></path>
      </marker>
      <marker id="arrowBall" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="5" markerHeight="5" orient="auto-start-reverse">
        <path d="M 0 0 L 10 5 L 0 10 z" fill="#ffffff"></path>
      </marker>
    </defs>
  `;

  scenario.court.zones.forEach((zone, index) => {
    const fallbackOwner = index === 0 ? "cover-blocker" : "cover-defender";
    const zoneNode = el("div", `zone ${zone.owner || fallbackOwner}`);
    zoneNode.innerHTML = `<span class="zone-label">${escapeHTML(zone.label)}</span>`;
    zoneNode.style.left = `${zone.x}%`;
    zoneNode.style.top = `${zone.y}%`;
    zoneNode.style.width = `${zone.w}%`;
    zoneNode.style.height = `${zone.h}%`;
    zonesLayer.appendChild(zoneNode);
  });

  scenario.court.routes.forEach((route) => {
    const line = document.createElementNS("http://www.w3.org/2000/svg", "line");
    line.setAttribute("x1", route.x1);
    line.setAttribute("y1", route.y1);
    line.setAttribute("x2", route.x2);
    line.setAttribute("y2", route.y2);
    line.setAttribute("class", route.kind === "move" ? "route-move" : "route-ball");
    line.setAttribute("marker-end", route.kind === "move" ? "url(#arrowMove)" : "url(#arrowBall)");
    routeLayer.appendChild(line);
  });

  scenario.court.players.forEach((player) => {
    const marker = el("div", `marker ${player.type}`, player.label);
    marker.style.left = `${player.x}%`;
    marker.style.top = `${player.y}%`;
    markersLayer.appendChild(marker);
  });

  const ball = el("div", "marker ball", "B");
  ball.style.left = `${scenario.court.ball.x}%`;
  ball.style.top = `${scenario.court.ball.y}%`;
  markersLayer.appendChild(ball);

  const zoneSummary = scenario.court.zones.map((zone, index) => {
    const role = zone.owner === "cover-support" ? "Cobertura" : index === 0 ? "Bloqueador" : "Defensor";
    return `${role}: ${zone.label}`;
  });

  detail.innerHTML = `
    <div class="scenario-meta">
      ${scenario.meta.map((item) => `<span class="pill">${item}</span>`).join("")}
    </div>
    <h3>${scenario.title}</h3>
    <p>${scenario.summary}</p>
    <div class="detail-group">
      <h4>Objetivo</h4>
      <p>${scenario.objective}</p>
    </div>
    <div class="detail-group">
      <h4>Zonas de cobertura</h4>
      ${listHTML(zoneSummary)}
    </div>
    <div class="detail-group">
      <h4>Comunicación</h4>
      ${listHTML(scenario.calls)}
    </div>
    <div class="detail-group">
      <h4>Movimientos inicio → destino</h4>
      ${listHTML(scenario.movements)}
    </div>
    <div class="detail-group">
      <h4>Roles</h4>
      ${listHTML(scenario.roles)}
    </div>
    <div class="detail-group">
      <h4>Correcciones de entrenador</h4>
      ${listHTML(scenario.corrections)}
    </div>
    <div class="detail-group">
      <h4>Drill</h4>
      <p>${scenario.drill}</p>
    </div>
  `;
}

function renderSignalControls() {
  const controls = document.querySelector("#signalControls");
  if (!controls) return;
  controls.innerHTML = "";

  signalModes.forEach((mode) => {
    const button = el("button", "signal-button", mode.label);
    button.type = "button";
    button.setAttribute("aria-pressed", String(mode.id === activeSignal));
    button.addEventListener("click", () => {
      activeSignal = mode.id;
      activeSignalStep = 0;
      renderSignalControls();
      renderSignalDemo();
    });
    controls.appendChild(button);
  });
}

function handSignalHTML(mode) {
  const fingerNames = ["Índice", "Corazón", "Anular", "Meñique"];
  const hands = ["izquierda", "derecha"]
    .map(
      (side) => `
        <div class="signal-hand ${side}" aria-hidden="true">
          <span class="hand-wrist"></span>
          <span class="hand-palm"></span>
          ${fingerNames
            .map(
              (name, index) =>
                `<span class="hand-finger finger-${index + 1}${index < mode.fingers ? " is-raised" : ""}" title="${name}"></span>`
            )
            .join("")}
          <span class="hand-thumb"></span>
        </div>
      `
    )
    .join("");

  return `
    <div class="hands-panel" role="img" aria-label="${mode.handLabel}">
      <div class="hands-visual">${hands}</div>
      <strong>${mode.handLabel}</strong>
      <small>Señal detrás de la espalda antes del saque</small>
    </div>
  `;
}

function setSignalPosition(selector, position) {
  const node = document.querySelector(selector);
  if (!node) return;
  node.style.left = `${position[0]}%`;
  node.style.top = `${position[1]}%`;
}

function setSignalZone(selector, zone) {
  const node = document.querySelector(selector);
  if (!node) return;

  if (!zone) {
    node.style.opacity = "0";
    node.style.pointerEvents = "none";
    return;
  }

  node.style.left = `${zone.x}%`;
  node.style.top = `${zone.y}%`;
  node.style.width = `${zone.w}%`;
  node.style.height = `${zone.h}%`;
  node.style.opacity = "1";
}

function renderSignalTrails(previous, current) {
  const layer = document.querySelector("#signalTrails");
  if (!layer) return;

  const line = (from, to, className, marker) =>
    `<line class="${className}" x1="${from[0]}" y1="${from[1]}" x2="${to[0]}" y2="${to[1]}" marker-end="url(#${marker})"></line>`;

  layer.innerHTML = `
    <defs>
      <marker id="signalArrowBlock" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="5" markerHeight="5" orient="auto">
        <path d="M 0 0 L 10 5 L 0 10 z"></path>
      </marker>
      <marker id="signalArrowDefense" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="5" markerHeight="5" orient="auto">
        <path d="M 0 0 L 10 5 L 0 10 z"></path>
      </marker>
      <marker id="signalArrowBall" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="5" markerHeight="5" orient="auto">
        <path d="M 0 0 L 10 5 L 0 10 z"></path>
      </marker>
    </defs>
    ${
      previous
        ? [
            line(previous.players.blocker, current.players.blocker, "trail trail-blocker", "signalArrowBlock"),
            line(previous.players.defender, current.players.defender, "trail trail-defender", "signalArrowDefense"),
            line(previous.ball, current.ball, "trail trail-ball", "signalArrowBall")
          ].join("")
        : ""
    }
  `;
}

function renderSignalTimeline(steps) {
  const timeline = document.querySelector("#signalTimeline");
  if (!timeline) return;

  timeline.innerHTML = steps
    .map(
      (step, index) => `
        <button class="phase-step${index === activeSignalStep ? " is-active" : ""}${index < activeSignalStep ? " is-complete" : ""}"
          type="button" data-step="${index}" aria-current="${index === activeSignalStep ? "step" : "false"}">
          <span>${index + 1}</span>
          <small>${step.name}</small>
        </button>
      `
    )
    .join("");

  timeline.querySelectorAll(".phase-step").forEach((button) => {
    button.addEventListener("click", () => {
      activeSignalStep = Number(button.dataset.step);
      setSignalPlayback(false);
      renderSignalStep();
    });
  });
}

function renderSignalInfo(mode) {
  const info = document.querySelector("#signalInfo");
  if (!info) return;
  const blockZone = mode.blockZone.replace("B: cambia a ", "").replace("B: ", "");
  const defenseZone = mode.defenseZone.replace("D: cambia a ", "").replace("D: ", "");
  const signalFocus = mode.fakeCoverage
    ? `Primero muestra ${mode.fakeCoverage}; cambia en el último instante.`
    : mode.id === "pull"
      ? "Sin salto: B baja a corta y D protege el fondo."
      : "Una zona la bloquea B; la otra la defiende D.";

  info.innerHTML = `
    <span class="system-tag">${mode.fakeCoverage ? "Señal avanzada" : "Sistema 2-2"}</span>
    <h3>${mode.title}</h3>
    ${handSignalHTML(mode)}
    <p class="info-lead">${mode.meaning}</p>
    <div class="quick-rules">
      <span class="quick-rule"><strong>Bloqueo</strong><small>${blockZone}</small></span>
      <span class="quick-rule"><strong>Defensa</strong><small>${defenseZone}</small></span>
      <span class="quick-rule"><strong>Clave</strong><small>${signalFocus}</small></span>
    </div>
    <div class="current-step-card">
      <span id="signalStepName"></span>
      <p id="signalStepDetail"></p>
    </div>
    <p class="tip-line"><strong>Ajuste:</strong> ${mode.correction}</p>
  `;
}

function updateSignalPlaybackButton() {
  const button = document.querySelector("#signalPlay");
  const label = document.querySelector("#signalPlayLabel");
  if (!button || !label) return;

  button.classList.toggle("is-paused", !signalPlaying);
  button.setAttribute("aria-label", signalPlaying ? "Pausar animación" : "Reproducir animación");
  label.textContent = signalPlaying ? "Pausar" : "Reproducir";
}

function setSignalPlayback(playing) {
  signalPlaying = playing;
  if (signalTimer) window.clearInterval(signalTimer);
  signalTimer = null;

  if (signalPlaying) {
    signalTimer = window.setInterval(() => {
      const mode = signalModes.find((item) => item.id === activeSignal) || signalModes[0];
      const steps = buildSignalSteps(mode);
      activeSignalStep = activeSignalStep >= steps.length - 1 ? 0 : activeSignalStep + 1;
      renderSignalStep();
    }, 2100);
  }

  updateSignalPlaybackButton();
}

function renderSignalStep() {
  const mode = signalModes.find((item) => item.id === activeSignal) || signalModes[0];
  const steps = buildSignalSteps(mode);
  activeSignalStep = Math.max(0, Math.min(activeSignalStep, steps.length - 1));
  const step = steps[activeSignalStep];
  const previous = activeSignalStep > 0 ? steps[activeSignalStep - 1] : null;
  const court = document.querySelector("#signalCourt");
  if (!court) return;

  court.dataset.mode = mode.id;
  court.dataset.step = String(activeSignalStep);
  court.dataset.coverageTone = step.coverageTone || "none";
  document.querySelector("#signalBadge").textContent = mode.badge;
  document.querySelector("#blockZoneLabel").textContent = mode.blockZone;
  document.querySelector("#defenseZoneLabel").textContent = mode.defenseZone;
  document.querySelector("#supportZoneLabel").textContent = mode.supportZone;
  document.querySelector("#phaseCaption").innerHTML = `<strong>${step.name}</strong><span>${step.caption}</span>`;
  document.querySelector("#signalStepName").textContent = `Paso ${activeSignalStep + 1}: ${step.name}`;
  document.querySelector("#signalStepDetail").textContent = step.caption;
  document.querySelector("#signalStepCounter").textContent = `${activeSignalStep + 1} / ${steps.length}`;

  Object.entries(step.players).forEach(([role, position]) => setSignalPosition(`.demo-player.${role}`, position));
  setSignalPosition(".demo-ball", step.ball);
  setSignalZone(".block-zone", step.coverage?.block);
  setSignalZone(".defense-zone", step.coverage?.defense);
  setSignalZone(".support-zone", step.coverage?.support);
  renderSignalTrails(previous, step);
  renderSignalTimeline(steps);
}

function renderSignalDemo() {
  const mode = signalModes.find((item) => item.id === activeSignal) || signalModes[0];
  renderSignalInfo(mode);
  renderSignalStep();
  setSignalPlayback(true);
}

function setupSignalAnimationControls() {
  document.querySelector("#signalPlay")?.addEventListener("click", () => setSignalPlayback(!signalPlaying));
  document.querySelector("#signalRestart")?.addEventListener("click", () => {
    activeSignalStep = 0;
    renderSignalStep();
    setSignalPlayback(true);
  });
  document.querySelector("#signalPrevious")?.addEventListener("click", () => {
    const mode = signalModes.find((item) => item.id === activeSignal) || signalModes[0];
    const steps = buildSignalSteps(mode);
    activeSignalStep = activeSignalStep <= 0 ? steps.length - 1 : activeSignalStep - 1;
    setSignalPlayback(false);
    renderSignalStep();
  });
  document.querySelector("#signalNext")?.addEventListener("click", () => {
    const mode = signalModes.find((item) => item.id === activeSignal) || signalModes[0];
    const steps = buildSignalSteps(mode);
    activeSignalStep = activeSignalStep >= steps.length - 1 ? 0 : activeSignalStep + 1;
    setSignalPlayback(false);
    renderSignalStep();
  });
}

function renderSettingControls() {
  const controls = document.querySelector("#settingControls");
  if (!controls) return;
  controls.innerHTML = "";

  settingModes.forEach((mode) => {
    const button = el("button", "signal-button", mode.label);
    button.type = "button";
    button.setAttribute("aria-pressed", String(mode.id === activeSetting));
    button.addEventListener("click", () => {
      activeSetting = mode.id;
      activeSettingStep = 0;
      renderSettingControls();
      renderSettingDemo();
    });
    controls.appendChild(button);
  });
}

function setSettingPosition(selector, position) {
  const node = document.querySelector(selector);
  if (!node) return;
  node.style.left = `${position[0]}%`;
  node.style.top = `${position[1]}%`;
}

function setSettingZone(selector, visible, zone) {
  const node = document.querySelector(selector);
  if (!node) return;

  if (zone) {
    node.style.left = `${zone.x}%`;
    node.style.top = `${zone.y}%`;
    node.style.width = `${zone.w}%`;
    node.style.height = `${zone.h}%`;
  }

  node.style.opacity = visible ? "1" : "0";
}

function renderSettingTrails(previous, current) {
  const layer = document.querySelector("#settingTrails");
  if (!layer) return;

  const line = (from, to, className, marker) =>
    `<line class="${className}" x1="${from[0]}" y1="${from[1]}" x2="${to[0]}" y2="${to[1]}" marker-end="url(#${marker})"></line>`;

  layer.innerHTML = `
    <defs>
      <marker id="settingArrowAttack" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="5" markerHeight="5" orient="auto">
        <path d="M 0 0 L 10 5 L 0 10 z"></path>
      </marker>
      <marker id="settingArrowSetter" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="5" markerHeight="5" orient="auto">
        <path d="M 0 0 L 10 5 L 0 10 z"></path>
      </marker>
      <marker id="settingArrowBall" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="5" markerHeight="5" orient="auto">
        <path d="M 0 0 L 10 5 L 0 10 z"></path>
      </marker>
    </defs>
    ${
      previous
        ? [
            line(previous.players.attacker, current.players.attacker, "setting-trail trail-attacker", "settingArrowAttack"),
            line(previous.players.setter, current.players.setter, "setting-trail trail-setter", "settingArrowSetter"),
            line(previous.ball, current.ball, "setting-trail trail-set-ball", "settingArrowBall")
          ].join("")
        : ""
    }
  `;
}

function renderSettingTimeline(steps) {
  const timeline = document.querySelector("#settingTimeline");
  if (!timeline) return;

  timeline.innerHTML = steps
    .map(
      (step, index) => `
        <button class="phase-step${index === activeSettingStep ? " is-active" : ""}${index < activeSettingStep ? " is-complete" : ""}"
          type="button" data-step="${index}" aria-current="${index === activeSettingStep ? "step" : "false"}">
          <span>${index + 1}</span>
          <small>${step.name}</small>
        </button>
      `
    )
    .join("");

  timeline.querySelectorAll(".phase-step").forEach((button) => {
    button.addEventListener("click", () => {
      activeSettingStep = Number(button.dataset.step);
      setSettingPlayback(false);
      renderSettingStep();
    });
  });
}

function renderSettingInfo(mode) {
  const info = document.querySelector("#settingInfo");
  if (!info) return;

  info.innerHTML = `
    <span class="system-tag">Llamada de colocación</span>
    <h3>${mode.title}</h3>
    <span class="setting-call">${mode.badge}</span>
    <p class="info-lead">${mode.meaning}</p>
    <div class="quick-rules setting-rules">
      <span class="quick-rule setter-rule"><strong>Mira</strong><small>${mode.look}</small></span>
      <span class="quick-rule setter-rule"><strong>Coloca</strong><small>${mode.set}</small></span>
      <span class="quick-rule attacker-rule"><strong>Ataca</strong><small>${mode.attacker}</small></span>
    </div>
    <div class="current-step-card">
      <span id="settingStepName"></span>
      <p id="settingStepDetail"></p>
    </div>
    <p class="tip-line"><strong>Punto clave:</strong> ${mode.correction}</p>
  `;
}

function updateSettingPlaybackButton() {
  const button = document.querySelector("#settingPlay");
  const label = document.querySelector("#settingPlayLabel");
  if (!button || !label) return;

  button.classList.toggle("is-paused", !settingPlaying);
  button.setAttribute("aria-label", settingPlaying ? "Pausar animación" : "Reproducir animación");
  label.textContent = settingPlaying ? "Pausar" : "Reproducir";
}

function setSettingPlayback(playing) {
  settingPlaying = playing;
  if (settingTimer) window.clearInterval(settingTimer);
  settingTimer = null;

  if (settingPlaying) {
    settingTimer = window.setInterval(() => {
      const mode = settingModes.find((item) => item.id === activeSetting) || settingModes[0];
      const steps = buildSettingSteps(mode);
      activeSettingStep = activeSettingStep >= steps.length - 1 ? 0 : activeSettingStep + 1;
      renderSettingStep();
    }, 2200);
  }

  updateSettingPlaybackButton();
}

function renderSettingStep() {
  const mode = settingModes.find((item) => item.id === activeSetting) || settingModes[0];
  const steps = buildSettingSteps(mode);
  activeSettingStep = Math.max(0, Math.min(activeSettingStep, steps.length - 1));
  const step = steps[activeSettingStep];
  const previous = activeSettingStep > 0 ? steps[activeSettingStep - 1] : null;
  const court = document.querySelector("#settingCourt");
  if (!court) return;

  court.dataset.mode = mode.id;
  court.dataset.step = String(activeSettingStep);
  document.querySelector("#settingBadge").textContent = mode.badge;
  document.querySelector("#settingTargetLabel").textContent = `Destino ${mode.label.toLowerCase()}`;
  document.querySelector("#settingPhaseCaption").innerHTML = `<strong>${step.name}</strong><span>${step.caption}</span>`;
  document.querySelector("#settingStepName").textContent = `Paso ${activeSettingStep + 1}: ${step.name}`;
  document.querySelector("#settingStepDetail").textContent = step.caption;
  document.querySelector("#settingStepCounter").textContent = `${activeSettingStep + 1} / ${steps.length}`;

  setSettingPosition(".setting-player.attack-player", step.players.attacker);
  setSettingPosition(".setting-player.set-player", step.players.setter);
  setSettingPosition(".setting-player.rival-server", step.players.server);
  setSettingPosition(".setting-player.rival-blocker", step.players.blocker);
  setSettingPosition(".setting-ball", step.ball);

  const receptionZone = {
    x: Math.max(5, mode.receiverStart[0] - 12),
    y: Math.max(7, mode.receiverStart[1] - 18),
    w: 24,
    h: 36
  };
  setSettingZone(".reception-zone", step.zones.reception, receptionZone);
  setSettingZone(".set-window-zone", step.zones.window, { x: 34, y: 37, w: 14, h: 26 });
  setSettingZone(".attack-target-zone", step.zones.target, mode.targetZone);

  const facing = document.querySelector("#setterFacing");
  if (facing) facing.style.setProperty("--facing-angle", `${step.facing}deg`);

  renderSettingTrails(previous, step);
  renderSettingTimeline(steps);
}

function renderSettingDemo() {
  const mode = settingModes.find((item) => item.id === activeSetting) || settingModes[0];
  renderSettingInfo(mode);
  renderSettingStep();
  setSettingPlayback(true);
}

function setupSettingAnimationControls() {
  document.querySelector("#settingPlay")?.addEventListener("click", () => setSettingPlayback(!settingPlaying));
  document.querySelector("#settingRestart")?.addEventListener("click", () => {
    activeSettingStep = 0;
    renderSettingStep();
    setSettingPlayback(true);
  });
  document.querySelector("#settingPrevious")?.addEventListener("click", () => {
    const mode = settingModes.find((item) => item.id === activeSetting) || settingModes[0];
    const steps = buildSettingSteps(mode);
    activeSettingStep = activeSettingStep <= 0 ? steps.length - 1 : activeSettingStep - 1;
    setSettingPlayback(false);
    renderSettingStep();
  });
  document.querySelector("#settingNext")?.addEventListener("click", () => {
    const mode = settingModes.find((item) => item.id === activeSetting) || settingModes[0];
    const steps = buildSettingSteps(mode);
    activeSettingStep = activeSettingStep >= steps.length - 1 ? 0 : activeSettingStep + 1;
    setSettingPlayback(false);
    renderSettingStep();
  });
}

function renderMovementTabs() {
  const tabs = document.querySelector("#movementTabs");
  if (!tabs) return;
  tabs.innerHTML = "";

  movementPhases.forEach((phase) => {
    const button = el("button", "tab-button", phase.label);
    button.type = "button";
    button.setAttribute("role", "tab");
    button.setAttribute("aria-selected", String(phase.id === activeMovement));
    button.addEventListener("click", () => {
      activeMovement = phase.id;
      renderMovementTabs();
      renderMovement();
    });
    tabs.appendChild(button);
  });
}

function renderMovement() {
  const phase = movementPhases.find((item) => item.id === activeMovement) || movementPhases[0];
  const detail = document.querySelector("#movementDetail");
  const table = document.querySelector("#movementTable");
  if (!detail || !table) return;

  detail.innerHTML = `
    <span class="system-tag">${phase.label}</span>
    <h3>${phase.title}</h3>
    <p>${phase.goal}</p>
    <div class="detail-group">
      <h4>Principios</h4>
      ${listHTML(phase.principles)}
    </div>
  `;

  table.innerHTML = phase.steps
    .map(
      (step) => `
        <article class="movement-card">
          <span class="card-topline">${step.role}</span>
          <dl>
            <div><dt>Inicio</dt><dd>${step.start}</dd></div>
            <div><dt>Disparador</dt><dd>${step.trigger}</dd></div>
            <div><dt>Movimiento</dt><dd>${step.move}</dd></div>
            <div><dt>Destino</dt><dd>${step.destination}</dd></div>
            <div><dt>Llamada</dt><dd>${step.cue}</dd></div>
            <div><dt>Corrección</dt><dd>${step.correction}</dd></div>
          </dl>
        </article>
      `
    )
    .join("");
}

function renderCommunicationProtocol() {
  const grid = document.querySelector("#communicationProtocol");
  if (!grid) return;
  grid.innerHTML = communicationProtocol
    .map(
      (item, index) => `
        <article class="communication-block">
          <span class="block-index">${String(index + 1).padStart(2, "0")}</span>
          <h3>${item.phase}</h3>
          <p>${item.aim}</p>
          <div class="mini-rule"><strong>Guion:</strong> ${item.script}</div>
          <div class="mini-rule"><strong>Ejemplo:</strong> ${item.example}</div>
          ${listHTML(item.must)}
          <p class="avoid-text"><strong>Evitar:</strong> ${item.avoid}</p>
        </article>
      `
    )
    .join("");
}

function renderCallbook() {
  const grid = document.querySelector("#callbookGrid");
  if (!grid) return;
  grid.innerHTML = callbook
    .map(
      (item) => `
        <article class="callbook-card">
          <span class="call-word">${item.call}</span>
          <p><strong>Momento:</strong> ${item.moment}</p>
          <p><strong>Significa:</strong> ${item.meaning}</p>
          <p><strong>Acción:</strong> ${item.action}</p>
        </article>
      `
    )
    .join("");
}

function renderStrategies() {
  const grid = document.querySelector("#strategyGrid");
  if (!grid) return;
  grid.innerHTML = strategies
    .map(
      (strategy) => `
        <article class="strategy-card">
          <span class="card-topline">${strategy.phase}</span>
          <h3>${strategy.title}</h3>
          <p><strong>Cuándo usarla:</strong> ${strategy.when}</p>
          <div class="detail-group">
            <h4>Ejecución</h4>
            ${listHTML(strategy.execution)}
          </div>
          <div class="movement-note">${strategy.movement}</div>
          <p class="avoid-text"><strong>Riesgo:</strong> ${strategy.risk}</p>
          <div class="strategy-calls">
            ${strategy.calls.map((call) => `<span class="pill">${call}</span>`).join("")}
          </div>
        </article>
      `
    )
    .join("");
}

function renderFilters() {
  const bar = document.querySelector("#moduleFilters");
  if (!bar) return;
  bar.innerHTML = "";

  moduleFilters.forEach((filter) => {
    const button = el("button", `filter-button${filter === activeFilter ? " is-active" : ""}`, filter);
    button.type = "button";
    button.addEventListener("click", () => {
      activeFilter = filter;
      renderFilters();
      renderModules();
    });
    bar.appendChild(button);
  });
}

function renderModules() {
  const grid = document.querySelector("#moduleGrid");
  if (!grid) return;
  const visible = activeFilter === "Todos" ? modules : modules.filter((module) => module.category === activeFilter);
  grid.innerHTML = "";

  visible.forEach((module) => {
    const card = el("article", "module-card");
    card.innerHTML = `
      <span class="card-topline">${module.category} · ${module.level}</span>
      <h3>${module.title}</h3>
      <p>${module.description}</p>
      ${listHTML(module.points)}
    `;
    grid.appendChild(card);
  });
}

function renderCalls() {
  const container = document.querySelector("#callChips");
  if (!container) return;
  container.innerHTML = "";
  calls.forEach((call) => container.appendChild(el("span", "call-chip", call)));
}

function renderLevels() {
  const switcher = document.querySelector("#levelSwitch");
  if (!switcher) return;
  switcher.innerHTML = "";

  levels.forEach((level) => {
    const button = el("button", "level-button", level.label);
    button.type = "button";
    button.setAttribute("role", "tab");
    button.setAttribute("aria-selected", String(level.id === activeLevel));
    button.addEventListener("click", () => {
      activeLevel = level.id;
      renderLevels();
      renderTraining();
    });
    switcher.appendChild(button);
  });
}

function renderTraining() {
  const grid = document.querySelector("#trainingGrid");
  if (!grid) return;
  grid.innerHTML = "";

  trainingPlans[activeLevel].forEach((plan) => {
    const card = el("article", "training-card");
    card.innerHTML = `
      <span class="training-time">${plan.time}</span>
      <h3>${plan.title}</h3>
      <p>${plan.text}</p>
      ${listHTML(plan.points)}
    `;
    grid.appendChild(card);
  });
}

const tournamentState = {
  teams: [],
  groups: [],
  groupMatches: [],
  finalPhase: null,
  knockoutScores: {},
  champion: null
};

const scoreboardState = {
  scores: [0, 0],
  displayOrder: [0, 1],
  confirmedSideTotals: new Set(),
  log: []
};

function clampTournamentTeams(value) {
  const count = Number.parseInt(value, 10);
  if (!Number.isFinite(count)) return 8;
  return Math.max(4, Math.min(32, count));
}

function renderTeamInputs(count = 8) {
  const container = document.querySelector("#teamInputs");
  if (!container) return;

  const currentNames = Array.from(container.querySelectorAll(".team-name-input")).map((input) => input.value);
  container.innerHTML = Array.from({ length: count }, (_, index) => {
    const value = currentNames[index] || `Equipo ${index + 1}`;
    return `
      <label class="team-input-row">
        <span>Equipo ${index + 1}</span>
        <input class="team-name-input" type="text" value="${escapeHTML(value)}" maxlength="32" />
      </label>
    `;
  }).join("");
}

function getTournamentTeams() {
  const input = document.querySelector("#tournamentTeamCount");
  const count = clampTournamentTeams(input?.value);
  if (input) input.value = String(count);
  renderTeamInputs(count);

  const used = new Map();
  return Array.from(document.querySelectorAll(".team-name-input")).slice(0, count).map((field, index) => {
    const baseName = field.value.trim() || `Equipo ${index + 1}`;
    const seen = used.get(baseName) || 0;
    used.set(baseName, seen + 1);
    return seen ? `${baseName} ${seen + 1}` : baseName;
  });
}

function setTournamentStatus(message) {
  const status = document.querySelector("#tournamentStatus");
  if (status) status.textContent = message;
}

function createGroups(teams) {
  const groupCount = teams.length <= 5 ? 1 : Math.ceil(teams.length / 4);
  const groups = Array.from({ length: groupCount }, (_, index) => ({
    id: `grupo-${index + 1}`,
    name: `Grupo ${String.fromCharCode(65 + index)}`,
    teams: []
  }));

  teams.forEach((team, index) => {
    groups[index % groupCount].teams.push(team);
  });

  return groups;
}

function createGroupMatches(groups) {
  return groups.flatMap((group) => {
    const matches = [];
    group.teams.forEach((home, homeIndex) => {
      group.teams.slice(homeIndex + 1).forEach((away) => {
        matches.push({
          id: `${group.id}-${matches.length + 1}`,
          groupId: group.id,
          home,
          away,
          homePoints: null,
          awayPoints: null
        });
      });
    });
    return matches;
  });
}

function parseScore(value) {
  const score = Number.parseInt(value, 10);
  return Number.isFinite(score) && score >= 0 ? score : null;
}

function hasValidScore(match) {
  return match.homePoints !== null && match.awayPoints !== null && match.homePoints !== match.awayPoints;
}

function randomBeachScore() {
  const homeWins = Math.random() >= 0.5;
  const isClose = Math.random() < 0.18;
  const winnerPoints = isClose ? 22 + Math.floor(Math.random() * 3) : 21;
  const loserPoints = isClose ? winnerPoints - 2 : 10 + Math.floor(Math.random() * 10);
  return homeWins ? [winnerPoints, loserPoints] : [loserPoints, winnerPoints];
}

function calculateGroupStandings(group) {
  const table = new Map(
    group.teams.map((team) => [
      team,
      { team, played: 0, wins: 0, losses: 0, pointsFor: 0, pointsAgainst: 0, diff: 0 }
    ])
  );

  tournamentState.groupMatches
    .filter((match) => match.groupId === group.id && hasValidScore(match))
    .forEach((match) => {
      const home = table.get(match.home);
      const away = table.get(match.away);
      home.played += 1;
      away.played += 1;
      home.pointsFor += match.homePoints;
      home.pointsAgainst += match.awayPoints;
      away.pointsFor += match.awayPoints;
      away.pointsAgainst += match.homePoints;
      home.diff = home.pointsFor - home.pointsAgainst;
      away.diff = away.pointsFor - away.pointsAgainst;

      if (match.homePoints > match.awayPoints) {
        home.wins += 1;
        away.losses += 1;
      } else {
        away.wins += 1;
        home.losses += 1;
      }
    });

  return Array.from(table.values()).sort(
    (a, b) =>
      b.wins - a.wins ||
      b.diff - a.diff ||
      b.pointsFor - a.pointsFor ||
      a.team.localeCompare(b.team)
  );
}

function renderGroupStage() {
  const stage = document.querySelector("#groupStage");
  if (!stage) return;

  if (!tournamentState.groups.length) {
    stage.innerHTML = `
      <div class="empty-state">
        <p>Genera los grupos para ver partidos y clasificación.</p>
      </div>
    `;
    return;
  }

  stage.innerHTML = tournamentState.groups
    .map((group) => {
      const standings = calculateGroupStandings(group);
      const matches = tournamentState.groupMatches.filter((match) => match.groupId === group.id);
      return `
        <section class="group-card">
          <h4>${group.name}</h4>
          <div class="team-seed-list">
            ${group.teams.map((team) => `<span>${escapeHTML(team)}</span>`).join("")}
          </div>
          <div class="match-list">
            ${matches
              .map(
                (match) => `
                  <div class="match-row">
                    <span>${escapeHTML(match.home)}</span>
                    <input class="group-score-input" type="number" min="0" max="40" value="${match.homePoints ?? ""}" data-match="${match.id}" data-side="home" aria-label="Puntos de ${escapeHTML(match.home)}" />
                    <strong>vs</strong>
                    <input class="group-score-input" type="number" min="0" max="40" value="${match.awayPoints ?? ""}" data-match="${match.id}" data-side="away" aria-label="Puntos de ${escapeHTML(match.away)}" />
                    <span>${escapeHTML(match.away)}</span>
                  </div>
                `
              )
              .join("")}
          </div>
          <table class="standings-table">
            <thead>
              <tr><th>Equipo</th><th>J</th><th>V</th><th>Dif.</th><th>PF</th></tr>
            </thead>
            <tbody>
              ${standings
                .map(
                  (row, index) => `
                    <tr>
                      <td><strong>${index + 1}</strong> ${escapeHTML(row.team)}</td>
                      <td>${row.played}</td>
                      <td>${row.wins}</td>
                      <td>${row.diff}</td>
                      <td>${row.pointsFor}</td>
                    </tr>
                  `
                )
                .join("")}
            </tbody>
          </table>
        </section>
      `;
    })
    .join("");

  stage.querySelectorAll(".group-score-input").forEach((input) => {
    input.addEventListener("change", () => {
      const match = tournamentState.groupMatches.find((item) => item.id === input.dataset.match);
      if (!match) return;
      match[input.dataset.side === "home" ? "homePoints" : "awayPoints"] = parseScore(input.value);
      tournamentState.finalPhase = null;
      tournamentState.knockoutScores = {};
      tournamentState.champion = null;
      renderGroupStage();
      renderKnockoutStage();
      setTournamentStatus("Clasificación actualizada. Crea las eliminatorias cuando termines los grupos.");
    });
  });
}

function getTournamentFormat() {
  return document.querySelector("#tournamentFormat")?.value || "double";
}

function getWinnerRoundName(roundIndex) {
  return roundIndex === 0 ? "Ganadores inicial" : `Ganadores R${roundIndex + 1}`;
}

function getRankedGroupTeams() {
  if (!tournamentState.groups.length) return [];

  return tournamentState.groups.flatMap((group) =>
    calculateGroupStandings(group).map((row, index) => ({
      ...row,
      group: group.name,
      groupRank: index + 1
    }))
  );
}

function rankTeamsForFinalPhase(teams) {
  return [...teams].sort(
    (a, b) =>
      a.groupRank - b.groupRank ||
      b.wins - a.wins ||
      b.diff - a.diff ||
      b.pointsFor - a.pointsFor ||
      a.team.localeCompare(b.team)
  );
}

function createFinalFlights() {
  const ranked = getRankedGroupTeams();
  const format = getTournamentFormat();

  if (format === "gold-silver") {
    const gold = [];
    const silver = [];
    tournamentState.groups.forEach((group) => {
      const table = calculateGroupStandings(group);
      table.forEach((row, index) => {
        const entry = { ...row, group: group.name, groupRank: index + 1 };
        if (index < 2) gold.push(entry);
        else silver.push(entry);
      });
    });

    return [
      { id: "oro", name: "Fase oro", teams: rankTeamsForFinalPhase(gold).map((item) => item.team) },
      { id: "plata", name: "Fase plata", teams: rankTeamsForFinalPhase(silver).map((item) => item.team) }
    ].filter((flight) => flight.teams.length >= 2);
  }

  return [
    {
      id: "general",
      name: "Doble eliminación",
      teams: rankTeamsForFinalPhase(ranked).map((item) => item.team)
    }
  ];
}

function pairBracketTeams(teams) {
  const entries = teams.filter(Boolean);
  const pairs = [];
  const byes = [];

  for (let index = 0; index < Math.floor(entries.length / 2); index += 1) {
    pairs.push([entries[index], entries[entries.length - 1 - index]]);
  }

  if (entries.length % 2 === 1) {
    byes.push(entries[Math.floor(entries.length / 2)]);
  }

  return { pairs, byes };
}

function getStoredKnockoutScore(matchId) {
  return tournamentState.knockoutScores[matchId] || {};
}

function createBracketMatch(id, home, away) {
  const stored = getStoredKnockoutScore(id);
  const homePoints = stored.homePoints ?? null;
  const awayPoints = stored.awayPoints ?? null;
  const match = { id, home, away, homePoints, awayPoints, winner: null, loser: null };

  if (!away) {
    match.winner = home;
    return match;
  }

  if (hasValidScore(match)) {
    match.winner = homePoints > awayPoints ? home : away;
    match.loser = homePoints > awayPoints ? away : home;
  }

  return match;
}

function buildDoubleEliminationFlight(seed) {
  let winnersActive = [...seed.teams];
  const losersFromWinners = [];
  const winnersRounds = [];
  let roundIndex = 0;

  while (winnersActive.length > 1 && roundIndex < 12) {
    const { pairs, byes } = pairBracketTeams(winnersActive);
    const matches = pairs.map(([home, away], index) =>
      createBracketMatch(`${seed.id}-w-${roundIndex}-${index}`, home, away)
    );
    winnersRounds.push({ name: getWinnerRoundName(roundIndex), matches });

    if (!matches.every((match) => match.winner)) break;

    winnersActive = [...byes, ...matches.map((match) => match.winner)];
    losersFromWinners.push(...matches.map((match) => match.loser).filter(Boolean));
    roundIndex += 1;
  }

  let losersActive = [...losersFromWinners];
  const losersRounds = [];
  let loserRoundIndex = 0;

  while (losersActive.length > 1 && loserRoundIndex < 12) {
    const { pairs, byes } = pairBracketTeams(losersActive);
    const matches = pairs.map(([home, away], index) =>
      createBracketMatch(`${seed.id}-l-${loserRoundIndex}-${index}`, home, away)
    );
    losersRounds.push({ name: loserRoundIndex === 0 ? "Perdedores inicial" : `Perdedores R${loserRoundIndex + 1}`, matches });

    if (!matches.every((match) => match.winner)) break;

    losersActive = [...byes, ...matches.map((match) => match.winner)];
    loserRoundIndex += 1;
  }

  const winnerSideChampion = winnersActive.length === 1 ? winnersActive[0] : null;
  const loserSideChampion = losersActive.length === 1 ? losersActive[0] : null;
  let finalMatch = null;
  let champion = null;

  if (winnerSideChampion && loserSideChampion) {
    finalMatch = createBracketMatch(`${seed.id}-final-0`, winnerSideChampion, loserSideChampion);
    champion = finalMatch.winner;
  } else if (winnerSideChampion && seed.teams.length === 1) {
    champion = winnerSideChampion;
  }

  return { ...seed, winnersRounds, losersRounds, finalMatch, champion };
}

function buildCurrentFinalPhase() {
  if (!tournamentState.finalPhase) return [];
  return tournamentState.finalPhase.flights.map(buildDoubleEliminationFlight);
}

function getAllKnockoutMatches(flights) {
  return flights.flatMap((flight) => [
    ...flight.winnersRounds.flatMap((round) => round.matches),
    ...flight.losersRounds.flatMap((round) => round.matches),
    ...(flight.finalMatch ? [flight.finalMatch] : [])
  ]);
}

function isEditableMatch(match) {
  return match.home && match.away && !match.winner;
}

function buildBracketFromStandings() {
  if (!tournamentState.groups.length) {
    setTournamentStatus("Primero genera la fase de grupos.");
    return;
  }

  const completed = tournamentState.groupMatches.filter(hasValidScore).length;
  if (completed < tournamentState.groupMatches.length) {
    setTournamentStatus("Completa o simula todos los partidos de grupos antes de crear eliminatorias.");
    return;
  }

  const flights = createFinalFlights();
  if (!flights.length) {
    setTournamentStatus("No hay suficientes equipos para crear la fase final.");
    return;
  }

  tournamentState.finalPhase = { format: getTournamentFormat(), flights };
  tournamentState.knockoutScores = {};
  tournamentState.champion = null;
  renderKnockoutStage();
  setTournamentStatus("Fase final creada. Anota resultados en Ganadores y Perdedores.");
}

function simulateGroupStage() {
  if (!tournamentState.groups.length) {
    generateTournament();
  }

  tournamentState.groupMatches.forEach((match) => {
    const [homePoints, awayPoints] = randomBeachScore();
    match.homePoints = homePoints;
    match.awayPoints = awayPoints;
  });

  tournamentState.finalPhase = null;
  tournamentState.knockoutScores = {};
  tournamentState.champion = null;
  renderGroupStage();
  buildBracketFromStandings();
  setTournamentStatus("Fase de grupos simulada. Ya puedes anotar o simular la fase final.");
}

function simulateKnockoutStage() {
  if (!tournamentState.finalPhase) {
    buildBracketFromStandings();
  }

  if (!tournamentState.finalPhase) return;

  let safety = 0;
  while (safety < 80) {
    const flights = buildCurrentFinalPhase();
    const openMatch = getAllKnockoutMatches(flights).find(isEditableMatch);
    if (!openMatch) break;
    const [homePoints, awayPoints] = randomBeachScore();
    tournamentState.knockoutScores[openMatch.id] = { homePoints, awayPoints };
    safety += 1;
  }

  renderKnockoutStage();
  const champions = buildCurrentFinalPhase().map((flight) => flight.champion).filter(Boolean);
  setTournamentStatus(champions.length ? `Campeones: ${champions.join(" · ")}.` : "Fase final simulada parcialmente.");
}

function renderKnockoutStage() {
  const stage = document.querySelector("#knockoutStage");
  if (!stage) return;

  if (!tournamentState.finalPhase) {
    stage.innerHTML = `
      <div class="empty-state">
        <p>Completa los grupos y crea la llave final.</p>
      </div>
    `;
    return;
  }

  const flights = buildCurrentFinalPhase();
  const champions = flights.map((flight) => flight.champion).filter(Boolean);
  tournamentState.champion = champions.join(" · ");

  const renderScoreInputs = (match) => `
    <div class="bracket-scoreline ${match.winner === match.home ? "is-winner" : ""}">
      <span>${escapeHTML(match.home)}</span>
      <input class="knockout-score-input" type="number" min="0" max="40" value="${match.homePoints ?? ""}" data-match="${match.id}" data-side="home" aria-label="Puntos de ${escapeHTML(match.home)}" />
    </div>
    <div class="bracket-scoreline ${match.winner === match.away ? "is-winner" : ""}">
      <span>${escapeHTML(match.away)}</span>
      <input class="knockout-score-input" type="number" min="0" max="40" value="${match.awayPoints ?? ""}" data-match="${match.id}" data-side="away" aria-label="Puntos de ${escapeHTML(match.away)}" />
    </div>
  `;

  const renderMatch = (match) => `
    <article class="bracket-match">
      ${
        match.away
          ? renderScoreInputs(match)
          : `<div class="bracket-scoreline is-winner"><span>${escapeHTML(match.home)}</span><strong>Bye</strong></div>`
      }
    </article>
  `;

  stage.innerHTML = `
    <div class="final-format-note">
      <strong>Formato:</strong>
      <span>${tournamentState.finalPhase.format === "gold-silver" ? "Oro y plata · doble eliminación" : "Todos · doble eliminación"}</span>
    </div>
    <div class="flight-stack">
      ${flights
        .map(
          (flight) => `
            <section class="flight-card">
              <div class="flight-heading">
                <h4>${flight.name}</h4>
                <span>${flight.teams.length} equipos</span>
              </div>
              <div class="bracket-columns">
                <section class="round-column">
                  <h5>Fase ganadores</h5>
                  ${flight.winnersRounds
                    .map(
                      (round) => `
                        <div class="bracket-round">
                          <h6>${round.name}</h6>
                          ${round.matches.map(renderMatch).join("")}
                        </div>
                      `
                    )
                    .join("")}
                </section>
                <section class="round-column">
                  <h5>Fase perdedores</h5>
                  ${
                    flight.losersRounds.length
                      ? flight.losersRounds
                          .map(
                            (round) => `
                              <div class="bracket-round">
                                <h6>${round.name}</h6>
                                ${round.matches.map(renderMatch).join("")}
                              </div>
                            `
                          )
                          .join("")
                      : `<div class="empty-state small"><p>Los perdedores de ganadores aparecerán aquí.</p></div>`
                  }
                </section>
                <section class="round-column">
                  <h5>Final</h5>
                  ${flight.finalMatch ? renderMatch(flight.finalMatch) : `<div class="empty-state small"><p>Completa ambas fases para abrir la final.</p></div>`}
                </section>
              </div>
              ${
                flight.champion
                  ? `<div class="champion-banner"><span>Campeón</span><strong>${escapeHTML(flight.champion)}</strong></div>`
                  : ""
              }
            </section>
          `
        )
        .join("")}
    </div>
  `;

  stage.querySelectorAll(".knockout-score-input").forEach((input) => {
    input.addEventListener("change", () => {
      const current = tournamentState.knockoutScores[input.dataset.match] || {};
      current[input.dataset.side === "home" ? "homePoints" : "awayPoints"] = parseScore(input.value);
      tournamentState.knockoutScores[input.dataset.match] = current;
      renderKnockoutStage();
      setTournamentStatus("Resultado de eliminatoria actualizado.");
    });
  });
}

function generateTournament() {
  const teams = getTournamentTeams();
  tournamentState.teams = teams;
  tournamentState.groups = createGroups(teams);
  tournamentState.groupMatches = createGroupMatches(tournamentState.groups);
  tournamentState.finalPhase = null;
  tournamentState.knockoutScores = {};
  tournamentState.champion = null;
  renderGroupStage();
  renderKnockoutStage();
  setTournamentStatus(`${teams.length} equipos repartidos en ${tournamentState.groups.length} grupo(s).`);
}

function resetTournament() {
  tournamentState.teams = [];
  tournamentState.groups = [];
  tournamentState.groupMatches = [];
  tournamentState.finalPhase = null;
  tournamentState.knockoutScores = {};
  tournamentState.champion = null;
  const countInput = document.querySelector("#tournamentTeamCount");
  if (countInput) countInput.value = "8";
  renderTeamInputs(8);
  renderGroupStage();
  renderKnockoutStage();
  setTournamentStatus("Torneo reiniciado. Define los equipos para empezar.");
}

function setupTournamentControls() {
  const countInput = document.querySelector("#tournamentTeamCount");
  if (!countInput) return;

  renderTeamInputs(clampTournamentTeams(countInput.value));
  renderGroupStage();
  renderKnockoutStage();

  document.querySelector("#buildTeamFields")?.addEventListener("click", () => {
    renderTeamInputs(clampTournamentTeams(countInput.value));
    setTournamentStatus("Lista de equipos preparada. Cambia los nombres y genera los grupos.");
  });
  document.querySelector("#generateTournament")?.addEventListener("click", generateTournament);
  document.querySelector("#simulateGroups")?.addEventListener("click", simulateGroupStage);
  document.querySelector("#buildBracket")?.addEventListener("click", buildBracketFromStandings);
  document.querySelector("#simulateBracket")?.addEventListener("click", simulateKnockoutStage);
  document.querySelector("#resetTournament")?.addEventListener("click", resetTournament);
}

function getScoreNames() {
  const teamA = document.querySelector("#scoreTeamA")?.value.trim() || "Equipo 1";
  const teamB = document.querySelector("#scoreTeamB")?.value.trim() || "Equipo 2";
  return [teamA, teamB];
}

function addScoreLog(message) {
  scoreboardState.log = [message, ...scoreboardState.log].slice(0, 8);
}

function renderScoreboard() {
  const scoreA = document.querySelector("#scoreA");
  const scoreB = document.querySelector("#scoreB");
  if (!scoreA || !scoreB) return;

  const names = getScoreNames();
  const [leftTeam, rightTeam] = scoreboardState.displayOrder;
  const total = scoreboardState.scores[0] + scoreboardState.scores[1];
  const nextChange = total === 0 ? 7 : Math.ceil((total + 1) / 7) * 7;
  const needsSideChange =
    total > 0 && total % 7 === 0 && !scoreboardState.confirmedSideTotals.has(total);

  document.querySelector("#scoreNameA").textContent = names[leftTeam];
  document.querySelector("#scoreNameB").textContent = names[rightTeam];
  scoreA.textContent = scoreboardState.scores[leftTeam];
  scoreB.textContent = scoreboardState.scores[rightTeam];
  document.querySelector("#scoreSideA").textContent = "Lado izquierda";
  document.querySelector("#scoreSideB").textContent = "Lado derecha";

  document.querySelectorAll(".score-team").forEach((slot, slotIndex) => {
    const teamIndex = scoreboardState.displayOrder[slotIndex];
    slot.querySelectorAll(".score-control").forEach((button) => {
      button.dataset.team = String(teamIndex);
      const action = Number(button.dataset.delta) > 0 ? "Sumar" : "Restar";
      button.setAttribute("aria-label", `${action} punto a ${names[teamIndex]}`);
    });
  });

  const notice = document.querySelector("#sideChangeNotice");
  if (notice) {
    notice.classList.toggle("is-active", needsSideChange);
    notice.innerHTML = needsSideChange
      ? `<strong>Cambio de lado</strong><span>Total ${total} puntos. Cambiad antes del siguiente saque.</span>`
      : `<span>Próximo cambio de lado en el punto total ${nextChange}.</span>`;
  }

  const confirmButton = document.querySelector("#confirmSideChange");
  if (confirmButton) {
    confirmButton.disabled = !needsSideChange;
  }

  const summary = document.querySelector("#matchSummary");
  if (summary) {
    summary.innerHTML = `
      <div><strong>Total jugado</strong><span>${total} puntos</span></div>
      <div><strong>Próximo cambio</strong><span>${nextChange}</span></div>
      <div><strong>${escapeHTML(names[leftTeam])}</strong><span>Izquierda</span></div>
      <div><strong>${escapeHTML(names[rightTeam])}</strong><span>Derecha</span></div>
    `;
  }

  const log = document.querySelector("#scoreLog");
  if (log) {
    log.innerHTML = scoreboardState.log.length
      ? scoreboardState.log.map((item) => `<p>${escapeHTML(item)}</p>`).join("")
      : "<p>El historial aparecerá cuando empiece el partido.</p>";
  }
}

function updateScore(teamIndex, delta) {
  const nextScore = Math.max(0, scoreboardState.scores[teamIndex] + delta);
  if (nextScore === scoreboardState.scores[teamIndex]) return;

  scoreboardState.scores[teamIndex] = nextScore;
  const names = getScoreNames();
  const action = delta > 0 ? "suma" : "resta";
  addScoreLog(`${names[teamIndex]} ${action} punto. Marcador ${scoreboardState.scores[0]}-${scoreboardState.scores[1]}.`);

  const total = scoreboardState.scores[0] + scoreboardState.scores[1];
  if (delta > 0 && total > 0 && total % 7 === 0 && !scoreboardState.confirmedSideTotals.has(total)) {
    addScoreLog(`Aviso de cambio de lado en el punto total ${total}.`);
  }

  renderScoreboard();
}

function confirmSideChange() {
  const total = scoreboardState.scores[0] + scoreboardState.scores[1];
  if (total <= 0 || total % 7 !== 0 || scoreboardState.confirmedSideTotals.has(total)) return;
  scoreboardState.confirmedSideTotals.add(total);
  scoreboardState.displayOrder = [...scoreboardState.displayOrder].reverse();
  addScoreLog(`Cambio de lado confirmado con ${total} puntos totales.`);
  renderScoreboard();
}

function resetScoreboard() {
  scoreboardState.scores = [0, 0];
  scoreboardState.displayOrder = [0, 1];
  scoreboardState.confirmedSideTotals = new Set();
  scoreboardState.log = [];
  renderScoreboard();
}

function setupScoreboardControls() {
  if (!document.querySelector("#scoreA")) return;

  document.querySelectorAll(".score-control").forEach((button) => {
    button.addEventListener("click", () => {
      updateScore(Number(button.dataset.team), Number(button.dataset.delta));
    });
  });
  document.querySelector("#scoreTeamA")?.addEventListener("input", renderScoreboard);
  document.querySelector("#scoreTeamB")?.addEventListener("input", renderScoreboard);
  document.querySelector("#confirmSideChange")?.addEventListener("click", confirmSideChange);
  document.querySelector("#resetScoreboard")?.addEventListener("click", resetScoreboard);
  renderScoreboard();
}

if ((window.location.hash === "#torneo" || window.location.hash === "#marcador") && !document.querySelector(window.location.hash)) {
  window.location.href = `torneo.html${window.location.hash}`;
}

const legacyHashMap = {
  "#comunicacion": "#pistas",
  "#biblioteca": "#pistas",
  "#estrategias": "#movimientos"
};

if (legacyHashMap[window.location.hash] && !document.querySelector(window.location.hash)) {
  const nextHash = legacyHashMap[window.location.hash];
  const scrollToLegacyTarget = () => {
    const target = document.querySelector(nextHash);
    if (target) window.scrollTo({ top: Math.max(0, target.offsetTop - 80), behavior: "auto" });
  };
  history.replaceState(null, "", nextHash);
  scrollToLegacyTarget();
  window.addEventListener("load", scrollToLegacyTarget, { once: true });
  window.setTimeout(scrollToLegacyTarget, 300);
  window.setTimeout(scrollToLegacyTarget, 900);
}

setupLanguageSwitcher();

if (document.querySelector("#signalCourt")) {
  renderSignalControls();
  setupSignalAnimationControls();
  renderSignalDemo();
}

if (document.querySelector("#settingCourt")) {
  renderSettingControls();
  setupSettingAnimationControls();
  renderSettingDemo();
}

renderMovementTabs();
renderMovement();
renderCallbook();
renderCalls();
setupTournamentControls();
setupScoreboardControls();
