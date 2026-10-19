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

const levels = [
  { id: "base", label: "Base" },
  { id: "intermedio", label: "Intermedio" },
  { id: "competicion", label: "Competición" }
];

let activeLevel = "base";
let activeMovement = movementPhases[0].id;
let activeSignal = signalModes[0].id;
let activeSignalStep = 0;
let signalPlaying = false;
let signalMotion = null;
let activeSetting = settingModes[0].id;
let activeSettingStep = 0;
let settingPlaying = false;
let settingMotion = null;

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

function listHTML(items) {
  return `<ul>${items.map((item) => `<li>${item}</li>`).join("")}</ul>`;
}

function observeAnimationVisibility(courtId, pause) {
  const court = document.getElementById(courtId);
  if (!court) return;
  if ("IntersectionObserver" in window) {
    const observer = new IntersectionObserver((entries) => {
      if (!entries[0].isIntersecting) pause();
    });
    observer.observe(court);
  }
  document.addEventListener("visibilitychange", () => {
    if (document.hidden) pause();
  });
  window.matchMedia?.("(prefers-reduced-motion: reduce)").addEventListener("change", (event) => {
    if (event.matches) pause();
  });
}

function renderPhaseTimeline(id, steps, activeStep, onSelect) {
  const timeline = document.getElementById(id);
  if (!timeline) return;
  if (timeline.children.length !== steps.length) {
    timeline.replaceChildren(...steps.map(() => el("button", "phase-step")));
  }
  Array.from(timeline.children).forEach((button, index) => {
    button.type = "button";
    button.dataset.step = String(index);
    button.classList.toggle("is-active", index === activeStep);
    button.classList.toggle("is-complete", index < activeStep);
    button.setAttribute("aria-current", index === activeStep ? "step" : "false");
    button.innerHTML = `<span>${index + 1}</span><small>${steps[index].name}</small>`;
    button.onclick = () => onSelect(index);
  });
}

function renderSignalControls() {
  const controls = document.querySelector("#signalControls");
  if (!controls) return;
  controls.innerHTML = "";

  signalModes.forEach((mode) => {
    const button = el("button", "signal-button", mode.label);
    button.type = "button";
    button.dataset.mode = mode.id;
    button.setAttribute("aria-pressed", String(mode.id === activeSignal));
    button.addEventListener("click", () => {
      activeSignal = mode.id;
      activeSignalStep = 0;
      controls.querySelectorAll("button").forEach((option) => option.setAttribute("aria-pressed", String(option.dataset.mode === activeSignal)));
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

function renderSignalTimeline(steps) {
  renderPhaseTimeline("signalTimeline", steps, activeSignalStep, (index) => {
    signalMotion?.seek(index);
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
    <details class="learning-details">
      <summary>Cómo corregirlo</summary>
      <p>${mode.correction}</p>
    </details>
  `;
}

function updateSignalPlaybackButton() {
  const button = document.querySelector("#signalPlay");
  const label = document.querySelector("#signalPlayLabel");
  if (!button || !label) return;

  button.classList.toggle("is-paused", !signalPlaying);
  const reduceMotion = window.matchMedia?.("(prefers-reduced-motion: reduce)").matches;
  const text = signalPlaying ? "Pausar" : reduceMotion ? "Ver siguiente paso" : "Reproducir";
  button.setAttribute("aria-label", text);
  label.textContent = text;
}

function setSignalPlayback(playing) {
  if (playing) {
    setSettingPlayback(false);
    signalMotion?.play();
  } else signalMotion?.pause();
}

function renderSignalStep() {
  const mode = signalModes.find((item) => item.id === activeSignal) || signalModes[0];
  const steps = buildSignalSteps(mode);
  activeSignalStep = Math.max(0, Math.min(activeSignalStep, steps.length - 1));
  const step = steps[activeSignalStep];
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
  document.querySelector("#signalStepCounter").textContent = `${activeSignalStep + 1} / ${steps.length}`;
  if (mode.id === "pull" && activeSignalStep >= 3) {
    const cue = document.querySelector("#signalMotionCue");
    if (cue) cue.textContent = `${step.name} · B se retira a la corta; D protege el fondo sin bloqueo.`;
  }

  setSignalZone(".block-zone", step.coverage?.block);
  setSignalZone(".defense-zone", step.coverage?.defense);
  setSignalZone(".support-zone", step.coverage?.support);
  renderSignalTimeline(steps);
  document.querySelector("#signalPrevious").disabled = activeSignalStep === 0;
  document.querySelector("#signalNext").disabled = activeSignalStep === steps.length - 1;
}

function renderSignalDemo() {
  const mode = signalModes.find((item) => item.id === activeSignal) || signalModes[0];
  renderSignalInfo(mode);
  if (!signalMotion) signalMotion = createTeachingMotion("signal", {
    blocker: ".demo-player.blocker", defender: ".demo-player.defender",
    attacker: ".demo-player.attacker", setter: ".demo-player.setter"
  }, {
    ball: ".demo-ball", routeRoles: ["blocker", "defender"],
    colors: {blocker: "#176765", defender: "#276ef1"},
    contactRoles: ["defender", "attacker", "setter", "attacker", "attacker", null],
    cues: [
      "B enseña la señal; D confirma y prepara el saque.",
      "D saca y entra; A recibe. B lee al atacante.",
      "C coloca; B se ajusta a la red y D recupera el centro.",
      "B muestra su bloqueo; D cubre el espacio complementario.",
      "B y D fijan sus zonas antes del golpeo.",
      "A ataca; B bloquea y D protege la zona libre."
    ],
    onStep(index) { activeSignalStep = index; renderSignalStep(); },
    onStateChange(state) { signalPlaying = state.playing; updateSignalPlaybackButton(); }
  });
  signalMotion?.load(buildSignalSteps(mode));
}

function setupSignalAnimationControls() {
  observeAnimationVisibility("signalCourt", () => setSignalPlayback(false));
  document.querySelector("#signalPlay")?.addEventListener("click", () => setSignalPlayback(!signalPlaying));
  document.querySelector("#signalRestart")?.addEventListener("click", () => {
    signalMotion?.restart();
  });
  document.querySelector("#signalPrevious")?.addEventListener("click", () => {
    signalMotion?.seek(activeSignalStep - 1);
  });
  document.querySelector("#signalNext")?.addEventListener("click", () => {
    signalMotion?.seek(activeSignalStep + 1);
  });
  setupMotionPreferences("signal", signalMotion);
}

function renderSettingControls() {
  const controls = document.querySelector("#settingControls");
  if (!controls) return;
  controls.innerHTML = "";

  settingModes.forEach((mode) => {
    const button = el("button", "signal-button", mode.label);
    button.type = "button";
    button.dataset.mode = mode.id;
    button.setAttribute("aria-pressed", String(mode.id === activeSetting));
    button.addEventListener("click", () => {
      activeSetting = mode.id;
      activeSettingStep = 0;
      controls.querySelectorAll("button").forEach((option) => option.setAttribute("aria-pressed", String(option.dataset.mode === activeSetting)));
      renderSettingDemo();
    });
    controls.appendChild(button);
  });
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

function renderSettingTimeline(steps) {
  renderPhaseTimeline("settingTimeline", steps, activeSettingStep, (index) => {
    settingMotion?.seek(index);
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
    <details class="learning-details">
      <summary>Qué hace cada jugador</summary>
      <div class="quick-rules setting-rules">
        <span class="quick-rule setter-rule"><strong>Mira</strong><small>${mode.look}</small></span>
        <span class="quick-rule setter-rule"><strong>Coloca</strong><small>${mode.set}</small></span>
        <span class="quick-rule attacker-rule"><strong>Ataca</strong><small>${mode.attacker}</small></span>
      </div>
    </details>
    <details class="learning-details">
      <summary>Cómo corregirlo</summary>
      <p>${mode.correction}</p>
    </details>
  `;
}

function updateSettingPlaybackButton() {
  const button = document.querySelector("#settingPlay");
  const label = document.querySelector("#settingPlayLabel");
  if (!button || !label) return;

  button.classList.toggle("is-paused", !settingPlaying);
  const reduceMotion = window.matchMedia?.("(prefers-reduced-motion: reduce)").matches;
  const text = settingPlaying ? "Pausar" : reduceMotion ? "Ver siguiente paso" : "Reproducir";
  button.setAttribute("aria-label", text);
  label.textContent = text;
}

function setSettingPlayback(playing) {
  if (playing) {
    setSignalPlayback(false);
    settingMotion?.play();
  } else settingMotion?.pause();
}

function renderSettingStep() {
  const mode = settingModes.find((item) => item.id === activeSetting) || settingModes[0];
  const steps = buildSettingSteps(mode);
  activeSettingStep = Math.max(0, Math.min(activeSettingStep, steps.length - 1));
  const step = steps[activeSettingStep];
  const court = document.querySelector("#settingCourt");
  if (!court) return;

  court.dataset.mode = mode.id;
  court.dataset.step = String(activeSettingStep);
  document.querySelector("#settingBadge").textContent = mode.badge;
  document.querySelector("#settingTargetLabel").textContent = `Destino ${mode.label.toLowerCase()}`;
  document.querySelector("#settingPhaseCaption").innerHTML = `<strong>${step.name}</strong><span>${step.caption}</span>`;
  document.querySelector("#settingStepCounter").textContent = `${activeSettingStep + 1} / ${steps.length}`;

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

  renderSettingTimeline(steps);
  document.querySelector("#settingPrevious").disabled = activeSettingStep === 0;
  document.querySelector("#settingNext").disabled = activeSettingStep === steps.length - 1;
}

function renderSettingDemo() {
  const mode = settingModes.find((item) => item.id === activeSetting) || settingModes[0];
  renderSettingInfo(mode);
  if (!settingMotion) settingMotion = createTeachingMotion("setting", {
    attacker: ".setting-player.attack-player", setter: ".setting-player.set-player",
    server: ".setting-player.rival-server", blocker: ".setting-player.rival-blocker"
  }, {
    ball: ".setting-ball", routeRoles: ["attacker", "setter"],
    colors: {attacker: "#176765", setter: "#276ef1"},
    contactRoles: ["server", "attacker", null, "setter", null, "attacker"],
    cues: [
      "A espera para recibir; C prepara la entrada a colocar.",
      "A recibe y orienta el primer toque; C entra.",
      "C llega al balón; A se prepara para atacar.",
      "C se orienta y escucha la llamada de A.",
      "C coloca y sale del carril; A inicia la carrera.",
      "A ataca hacia la red; C se queda en cobertura."
    ],
    onStep(index) { activeSettingStep = index; renderSettingStep(); },
    onStateChange(state) { settingPlaying = state.playing; updateSettingPlaybackButton(); }
  });
  settingMotion?.load(buildSettingSteps(mode));
}

function setupSettingAnimationControls() {
  observeAnimationVisibility("settingCourt", () => setSettingPlayback(false));
  document.querySelector("#settingPlay")?.addEventListener("click", () => setSettingPlayback(!settingPlaying));
  document.querySelector("#settingRestart")?.addEventListener("click", () => {
    settingMotion?.restart();
  });
  document.querySelector("#settingPrevious")?.addEventListener("click", () => {
    settingMotion?.seek(activeSettingStep - 1);
  });
  document.querySelector("#settingNext")?.addEventListener("click", () => {
    settingMotion?.seek(activeSettingStep + 1);
  });
  setupMotionPreferences("setting", settingMotion);
}

function createTeachingMotion(prefix, selectors, options) {
  const court = document.querySelector(`#${prefix}Court`);
  if (!court || !window.CourtMotion) return null;
  return window.CourtMotion.create({
    ...options, court,
    players: Object.fromEntries(Object.entries(selectors).map(([role, selector]) => [role, court.querySelector(selector)])),
    ball: court.querySelector(options.ball),
    trails: document.querySelector(`#${prefix}Trails`),
    progress: document.querySelector(`#${prefix}MotionProgress`),
    cue: document.querySelector(`#${prefix}MotionCue`),
    getCue(step, index) { return `${step.name} · ${options.cues[index]}`; }
  });
}

function setupMotionPreferences(prefix, motion) {
  if (!motion) return;
  const speed = document.querySelector(`#${prefix}Speed`);
  const routes = document.querySelector(`#${prefix}Routes`);
  if (speed) {
    motion.setSpeed(speed.value);
    speed.addEventListener("change", () => motion.setSpeed(speed.value));
  }
  if (routes) {
    motion.setRoutes(routes.checked);
    routes.addEventListener("change", () => motion.setRoutes(routes.checked));
  }
}

function renderMovementTabs() {
  const tabs = document.querySelector("#movementTabs");
  if (!tabs) return;
  tabs.innerHTML = "";

  movementPhases.forEach((phase) => {
    const button = el("button", "tab-button", phase.label);
    button.type = "button";
    button.dataset.phase = phase.id;
    button.setAttribute("aria-pressed", String(phase.id === activeMovement));
    button.addEventListener("click", () => {
      activeMovement = phase.id;
      tabs.querySelectorAll("button").forEach((option) => option.setAttribute("aria-pressed", String(option.dataset.phase === activeMovement)));
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
    <details class="learning-details">
      <summary>Claves para hacerlo bien</summary>
      ${listHTML(phase.principles)}
    </details>
  `;

  table.innerHTML = phase.steps
    .map(
      (step) => `
        <article class="movement-card">
          <span class="card-topline">${step.role}</span>
          <dl>
            <div><dt>Sal desde</dt><dd>${step.start}</dd></div>
            <div><dt>Llega a</dt><dd>${step.destination}</dd></div>
          </dl>
          <details class="learning-details">
            <summary>Cuándo moverte y cómo hacerlo</summary>
            <dl>
              <div><dt>Cuándo</dt><dd>${step.trigger}</dd></div>
              <div><dt>Cómo</dt><dd>${step.move}</dd></div>
              <div><dt>Qué decir</dt><dd>${step.cue}</dd></div>
              <div><dt>Corrige</dt><dd>${step.correction}</dd></div>
            </dl>
          </details>
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
        <details class="callbook-card">
          <summary><span class="call-word">${item.call}</span><span>${item.meaning}</span></summary>
          <p><strong>Cuándo:</strong> ${item.moment}.</p>
          <p><strong>Tu pareja:</strong> ${item.action}</p>
        </details>
      `
    )
    .join("");
}

function renderLevels() {
  const switcher = document.querySelector("#levelSwitch");
  if (!switcher) return;
  switcher.innerHTML = "";

  levels.forEach((level) => {
    const button = el("button", "level-button", level.label);
    button.type = "button";
    button.dataset.level = level.id;
    button.setAttribute("aria-pressed", String(level.id === activeLevel));
    button.addEventListener("click", () => {
      activeLevel = level.id;
      switcher.querySelectorAll("button").forEach((option) => option.setAttribute("aria-pressed", String(option.dataset.level === activeLevel)));
      renderTraining();
    });
    switcher.appendChild(button);
  });
}

function renderTraining() {
  const grid = document.querySelector("#trainingGrid");
  if (!grid) return;
  grid.innerHTML = "";

  const plans = trainingPlans[activeLevel];
  const totalMinutes = plans.reduce((total, plan) => total + Number.parseInt(plan.time, 10), 0);
  const summary = document.querySelector("#trainingSummary");
  if (summary) summary.textContent = `${levels.find((level) => level.id === activeLevel).label} · ${totalMinutes} minutos de práctica · 3 ejercicios`;

  plans.forEach((plan, index) => {
    const card = el("article", "training-card");
    card.innerHTML = `
      <span class="training-time">${index + 1} · ${plan.time}</span>
      <h3>${plan.title}</h3>
      <p>${plan.text}</p>
      <details class="learning-details"><summary>En qué fijarte</summary>${listHTML(plan.points)}</details>
    `;
    grid.appendChild(card);
  });
}

const tournamentState = {
  format: null,
  teams: [],
  groups: [],
  groupMatches: [],
  finalPhase: null,
  knockoutScores: {},
  champion: null
};

const scoreboardState = {
  scores: [0, 0],
  target: 21,
  displayOrder: [0, 1],
  confirmedSideTotals: new Set(),
  log: [],
  history: []
};

const playStorageKey = "volley-beach-santi-play-v1";
let restoringPlayState = false;
let tournamentDraftNames = [];

function savePlayState() {
  if (restoringPlayState || !document.querySelector("#tournamentTeamCount")) return;
  try {
    localStorage.setItem(playStorageKey, JSON.stringify({
      version: 1,
      tournament: tournamentState,
      draft: {
        count: document.querySelector("#tournamentTeamCount").value,
        format: document.querySelector("#tournamentFormat").value,
        names: tournamentDraftNames.map((name, index) => document.querySelectorAll(".team-name-input")[index]?.value ?? name)
      },
      scoreboard: {
        ...scoreboardState,
        names: getScoreNames(),
        confirmedSideTotals: [...scoreboardState.confirmedSideTotals]
      }
    }));
    const status = document.querySelector("#tournamentSaveStatus");
    if (status) status.textContent = "Guardado automáticamente en este navegador.";
  } catch {
    const status = document.querySelector("#tournamentSaveStatus");
    if (status) status.textContent = "El navegador no permite guardar. Mantén esta pestaña abierta para conservar el progreso.";
  }
}

function restorePlayState() {
  try {
    const saved = JSON.parse(localStorage.getItem(playStorageKey));
    if (!saved || saved.version !== 1) return false;
    const draft = saved.draft;
    if (!draft || !Array.isArray(draft.names)) return false;
    restoringPlayState = true;
    document.querySelector("#tournamentTeamCount").value = String(clampTournamentTeams(draft.count));
    if (["final-four", "gold-silver-final-four", "double", "gold-silver"].includes(draft.format)) {
      document.querySelector("#tournamentFormat").value = draft.format;
    }
    renderTeamInputs(clampTournamentTeams(draft.count), draft.names);
    const savedTournament = saved.tournament;
    const validTeamList = (teams) => Array.isArray(teams) && teams.length <= 32 && new Set(teams).size === teams.length && teams.every((team) => typeof team === "string" && team.length > 0 && team.length <= 40);
    const validGroups = savedTournament && validTeamList(savedTournament.teams) && savedTournament.teams.length >= 4 &&
      Array.isArray(savedTournament.groups) && savedTournament.groups.length > 0 && savedTournament.groups.length <= 8 &&
      savedTournament.groups.every((group, index) => group && group.id === `grupo-${index + 1}` && typeof group.name === "string" && group.name.length <= 40 &&
        validTeamList(group.teams) && group.teams.length >= 2 && group.teams.every(team => savedTournament.teams.includes(team))) &&
      savedTournament.groups.flatMap(group => group.teams).length === savedTournament.teams.length &&
      new Set(savedTournament.groups.flatMap(group => group.teams)).size === savedTournament.teams.length;
    const expectedMatches = validGroups ? createGroupMatches(savedTournament.groups) : [];
    const validMatches = validGroups && Array.isArray(savedTournament.groupMatches) && savedTournament.groupMatches.length === expectedMatches.length &&
      savedTournament.groupMatches.every((match, index) => match && ['id', 'groupId', 'home', 'away'].every(key => match[key] === expectedMatches[index][key]));
    if (validMatches) {
      tournamentState.format = ["final-four", "gold-silver-final-four", "double", "gold-silver"].includes(savedTournament.format) ? savedTournament.format : draft.format;
      tournamentState.teams = savedTournament.teams;
      tournamentState.groups = savedTournament.groups;
      tournamentState.groupMatches = savedTournament.groupMatches.map((match) => ({ ...match, homePoints: parseScore(match.homePoints), awayPoints: parseScore(match.awayPoints) }));
      const phase = savedTournament.finalPhase;
      if (phase && phase.format === tournamentState.format && Array.isArray(phase.flights) && phase.flights.length <= 2 &&
        phase.flights.every((flight) => flight && ["general", "oro", "plata"].includes(flight.id) && typeof flight.name === "string" && flight.name.length <= 40 && validTeamList(flight.teams) && flight.teams.length >= 2 && flight.teams.every((team) => tournamentState.teams.includes(team))) && new Set(phase.flights.map(flight => flight.id)).size === phase.flights.length) {
        tournamentState.finalPhase = phase;
        tournamentState.knockoutScores = savedTournament.knockoutScores && typeof savedTournament.knockoutScores === "object" ? savedTournament.knockoutScores : {};
      }
    }
    const score = saved.scoreboard;
    if (score && Array.isArray(score.scores) && score.scores.length === 2 && score.scores.every((value) => Number.isInteger(value) && value >= 0 && value <= 10000) &&
      Array.isArray(score.displayOrder) && [...score.displayOrder].sort().join() === "0,1") {
      scoreboardState.scores = score.scores;
      scoreboardState.target = score.target === 15 ? 15 : 21;
      scoreboardState.displayOrder = score.displayOrder;
      scoreboardState.confirmedSideTotals = new Set(Array.isArray(score.confirmedSideTotals) ? score.confirmedSideTotals.filter(value => Number.isInteger(value) && value > 0 && value <= score.scores[0] + score.scores[1]) : []);
      scoreboardState.log = Array.isArray(score.log) ? score.log.filter((item) => typeof item === "string").slice(0, 8) : [];
      scoreboardState.history = [];
      if (Array.isArray(score.names)) score.names.slice(0, 2).forEach((name, index) => {
        document.querySelector(index === 0 ? "#scoreTeamA" : "#scoreTeamB").value = typeof name === "string" ? name.slice(0, 32) : `Equipo ${index + 1}`;
      });
      document.querySelector("#scoreSetTarget").value = String(scoreboardState.target);
    }
    return true;
  } catch {
    return false;
  } finally {
    restoringPlayState = false;
  }
}

function clampTournamentTeams(value) {
  const count = Number.parseInt(value, 10);
  if (!Number.isFinite(count)) return 8;
  return Math.max(4, Math.min(32, count));
}

function renderTeamInputs(count = 8, savedNames = null) {
  const container = document.querySelector("#teamInputs");
  if (!container) return;

  if (savedNames) tournamentDraftNames = savedNames.slice(0, 32).map((name) => typeof name === "string" ? name.slice(0, 32) : "");
  else container.querySelectorAll(".team-name-input").forEach((input, index) => { tournamentDraftNames[index] = input.value; });
  const currentNames = tournamentDraftNames;
  container.innerHTML = Array.from({ length: count }, (_, index) => {
    const value = currentNames[index] ?? `Equipo ${index + 1}`;
    tournamentDraftNames[index] = value;
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

  const used = new Set();
  return Array.from(document.querySelectorAll(".team-name-input")).slice(0, count).map((field, index) => {
    const baseName = field.value.trim() || `Equipo ${index + 1}`;
    let name = baseName;
    let suffix = 2;
    while (used.has(name.toLocaleLowerCase())) name = `${baseName} ${suffix++}`;
    used.add(name.toLocaleLowerCase());
    field.value = name;
    return name;
  });
}

function setTournamentStatus(message) {
  const status = document.querySelector("#tournamentStatus");
  if (status) status.textContent = message;
  updateTournamentControls();
  savePlayState();
}

function isTournamentDraftChanged() {
  if (!tournamentState.groups.length) return false;
  const names = Array.from(document.querySelectorAll(".team-name-input"), (input, index) => input.value.trim() || `Equipo ${index + 1}`);
  return Number(document.querySelector("#tournamentTeamCount")?.value) !== tournamentState.teams.length ||
    document.querySelector("#tournamentFormat")?.value !== tournamentState.format || names.join("\n") !== tournamentState.teams.join("\n");
}

function updateTournamentControls() {
  if (!document.querySelector("#tournamentTeamCount")) return;
  const total = tournamentState.groupMatches.length;
  const complete = tournamentState.groupMatches.filter(hasValidScore).length;
  const hasGroups = total > 0;
  const hasFinal = Boolean(tournamentState.finalPhase);
  document.querySelector("#buildBracket").disabled = !hasGroups || complete !== total || hasFinal;
  document.querySelector("#simulateGroups").disabled = !hasGroups || complete === total;
  document.querySelector("#simulateBracket").disabled = !hasFinal || getAllKnockoutMatches(buildCurrentFinalPhase()).every((match) => !isEditableMatch(match));
  document.querySelector("#generateTournament").textContent = hasGroups ? "Volver a crear grupos" : "Crear grupos";
  const progress = document.querySelector("#tournamentProgress");
  if (progress) progress.textContent = hasGroups ? `Grupos: ${complete} de ${total} partidos completados.${hasFinal ? " Fase final disponible abajo." : complete === total ? " Ya puedes crear la fase final." : " Anota los dos marcadores de cada partido."}` : "";
  const format = document.querySelector("#tournamentFormat")?.value;
  const hints = {
    "final-four": "Los cuatro mejores pasan a dos semifinales, final y partido por el bronce.",
    "gold-silver-final-four": "Los dos primeros de cada grupo van a oro; el resto, a plata. Los cuatro mejores de cada categoría juegan su podio.",
    "double": "Todos pasan a la fase final. Dos derrotas eliminan; la gran final se repite si es la primera derrota del ganador de la llave principal.",
    "gold-silver": "Los dos primeros de cada grupo van a oro; el resto, a plata. Cada categoría disputa su cuadro de doble eliminación."
  };
  const hint = document.querySelector("#tournamentFormatHint");
  if (hint) hint.textContent = `${hints[format] || ""}${isTournamentDraftChanged() ? " Tienes cambios sin aplicar: los grupos actuales conservan sus equipos y su formato. Vuelve a crearlos para aplicar los cambios." : ""}`;
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
  if (value === null || value === undefined || String(value).trim() === "") return null;
  const score = Number(value);
  return Number.isInteger(score) && score >= 0 ? score : null;
}

function hasValidScore(match) {
  return Number.isInteger(match.homePoints) && Number.isInteger(match.awayPoints) && match.homePoints >= 0 && match.awayPoints >= 0 && match.homePoints !== match.awayPoints;
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

function renderStandingsRows(group) {
  return calculateGroupStandings(group).map((row, index) => `<tr>
    <td><strong>${index + 1}</strong> ${escapeHTML(row.team)}</td>
    <td>${row.played}</td><td>${row.wins}</td><td>${row.diff}</td><td>${row.pointsFor}</td>
  </tr>`).join('');
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

      const matches = tournamentState.groupMatches.filter((match) => match.groupId === group.id);
      return `
        <section class="group-card">
          <h4>${escapeHTML(group.name)}</h4>
          <div class="team-seed-list">
            ${group.teams.map((team) => `<span>${escapeHTML(team)}</span>`).join("")}
          </div>
          <div class="match-list">
            ${matches
              .map(
                (match) => `
                  <div class="match-row">
                    <span>${escapeHTML(match.home)}</span>
                    <input class="group-score-input" type="number" min="0" step="1" inputmode="numeric" value="${match.homePoints ?? ""}" data-match="${match.id}" data-side="home" aria-label="Puntos de ${escapeHTML(match.home)}" />
                    <strong>vs</strong>
                    <input class="group-score-input" type="number" min="0" step="1" inputmode="numeric" value="${match.awayPoints ?? ""}" data-match="${match.id}" data-side="away" aria-label="Puntos de ${escapeHTML(match.away)}" />
                    <span>${escapeHTML(match.away)}</span>
                  </div>
                  <p class="match-feedback" data-feedback="${match.id}" ${match.homePoints !== null && match.awayPoints !== null && !hasValidScore(match) ? "" : "hidden"}>El partido no puede terminar empatado. Corrige el resultado.</p>
                `
              )
              .join("")}
          </div>
          <table class="standings-table">
            <caption class="visually-hidden">Clasificación de ${escapeHTML(group.name)}. J: jugados; V: victorias; Dif.: diferencia de puntos; PF: puntos a favor.</caption>
            <thead>
              <tr><th scope="col">Equipo</th><th scope="col"><abbr title="Jugados">J</abbr></th><th scope="col"><abbr title="Victorias">V</abbr></th><th scope="col"><abbr title="Diferencia de puntos">Dif.</abbr></th><th scope="col"><abbr title="Puntos a favor">PF</abbr></th></tr>
            </thead>
            <tbody>
              ${renderStandingsRows(group)}
            </tbody>
          </table>
        </section>
      `;
    })
    .join("");

  stage.querySelectorAll(".group-score-input").forEach((input) => {
    input.addEventListener("input", () => {
      const match = tournamentState.groupMatches.find((item) => item.id === input.dataset.match);
      if (!match) return;
      match[input.dataset.side === "home" ? "homePoints" : "awayPoints"] = parseScore(input.value);
      tournamentState.finalPhase = null;
      tournamentState.knockoutScores = {};
      tournamentState.champion = null;
      const group = tournamentState.groups.find(group => group.id === match.groupId);
      const card = input.closest('.group-card');
      card.querySelector('tbody').innerHTML = renderStandingsRows(group);
      const feedback = card.querySelector(`[data-feedback="${match.id}"]`);
      feedback.hidden = !(match.homePoints !== null && match.awayPoints !== null && !hasValidScore(match));
      renderKnockoutStage();
      setTournamentStatus(hasValidScore(match) ? "Resultado guardado. La clasificación se actualiza automáticamente." : "Introduce los puntos de ambos equipos, sin empates.");
    });
  });
}

function getTournamentFormat() {
  return tournamentState.format || document.querySelector("#tournamentFormat")?.value || "final-four";
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

  if (format === "gold-silver" || format === "gold-silver-final-four") {
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
      name: format === "final-four" ? "Final Four" : "Doble eliminación",
      teams: rankTeamsForFinalPhase(ranked).map((item) => item.team)
    }
  ];
}

function pairBracketTeams(teams) {
  const entries = teams.filter(Boolean);
  const pairs = [];
  const byes = [];

  if (entries.length % 2 === 1) byes.push(entries.shift());
  for (let index = 0; index < Math.floor(entries.length / 2); index += 1) {
    pairs.push([entries[index], entries[entries.length - 1 - index]]);
  }

  return { pairs, byes };
}

function getStoredKnockoutScore(matchId) {
  return tournamentState.knockoutScores[matchId] || {};
}

function createBracketMatch(id, home, away) {
  let stored = getStoredKnockoutScore(id);
  if (stored.home !== home || stored.away !== away) {
    delete tournamentState.knockoutScores[id];
    stored = {};
  }
  const homePoints = parseScore(stored.homePoints);
  const awayPoints = parseScore(stored.awayPoints);
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

  // Keep this simplified second-chance bracket stable until the winners side is complete.
  let losersActive = winnersActive.length === 1 ? [...losersFromWinners] : [];
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
  let grandFinalMatch = null;
  let resetMatch = null;
  let champion = null;

  if (winnerSideChampion && loserSideChampion) {
    grandFinalMatch = createBracketMatch(`${seed.id}-final-0`, winnerSideChampion, loserSideChampion);
    finalMatch = grandFinalMatch;
    if (grandFinalMatch.winner === loserSideChampion) {
      resetMatch = createBracketMatch(`${seed.id}-reset-0`, winnerSideChampion, loserSideChampion);
      finalMatch = resetMatch;
    }
    champion = finalMatch.winner;
  } else if (winnerSideChampion && seed.teams.length === 1) {
    champion = winnerSideChampion;
  }

  return { ...seed, winnersRounds, losersRounds, grandFinalMatch, resetMatch, finalMatch, champion };
}

function buildFinalFourFlight(seed) {
  const qualified = seed.teams.slice(0, 4);
  const semifinals = [];
  let finalMatch = null;
  let bronzeMatch = null;
  let bronze = null;
  if (qualified.length >= 4) {
    semifinals.push(createBracketMatch(`${seed.id}-semi-0`, qualified[0], qualified[3]));
    semifinals.push(createBracketMatch(`${seed.id}-semi-1`, qualified[1], qualified[2]));
    if (semifinals.every(match => match.winner)) {
      finalMatch = createBracketMatch(`${seed.id}-final-0`, semifinals[0].winner, semifinals[1].winner);
      bronzeMatch = createBracketMatch(`${seed.id}-bronze-0`, semifinals[0].loser, semifinals[1].loser);
      bronze = bronzeMatch.winner;
    }
  } else if (qualified.length === 3) {
    // Top seed advances directly; the semifinal loser takes bronze.
    semifinals.push(createBracketMatch(`${seed.id}-semi-0`, qualified[1], qualified[2]));
    if (semifinals[0].winner) {
      finalMatch = createBracketMatch(`${seed.id}-final-0`, qualified[0], semifinals[0].winner);
      bronze = semifinals[0].loser;
    }
  } else if (qualified.length === 2) {
    finalMatch = createBracketMatch(`${seed.id}-final-0`, qualified[0], qualified[1]);
  }
  return { ...seed, qualified, semifinals, bronzeMatch, bronze, finalMatch,
    champion: finalMatch?.winner || null, winnersRounds: [], losersRounds: [], isFinalFour: true };
}

function buildCurrentFinalPhase() {
  if (!tournamentState.finalPhase) return [];
  const finalFour = ["final-four", "gold-silver-final-four"].includes(tournamentState.finalPhase.format);
  return tournamentState.finalPhase.flights.map(finalFour ? buildFinalFourFlight : buildDoubleEliminationFlight);
}

function getAllKnockoutMatches(flights) {
  return flights.flatMap((flight) => [
    ...(flight.semifinals || []),
    ...flight.winnersRounds.flatMap((round) => round.matches),
    ...flight.losersRounds.flatMap((round) => round.matches),
    ...(flight.resetMatch && flight.grandFinalMatch ? [flight.grandFinalMatch] : []),
    ...(flight.finalMatch ? [flight.finalMatch] : []),
    ...(flight.bronzeMatch ? [flight.bronzeMatch] : [])
  ]);
}

function isEditableMatch(match) {
  return match.home && match.away && !match.winner;
}

function pruneKnockoutScores() {
  const currentIds = new Set(getAllKnockoutMatches(buildCurrentFinalPhase()).map((match) => match.id));
  Object.keys(tournamentState.knockoutScores).forEach((id) => {
    if (!currentIds.has(id)) delete tournamentState.knockoutScores[id];
  });
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
  setTournamentStatus(["final-four", "gold-silver-final-four"].includes(getTournamentFormat())
    ? "Fase final creada. Los cuatro mejores de cada categoría juegan semifinales; los ganadores van a la final y los perdedores al partido por el bronce."
    : "Fase final creada. Anota resultados en Ganadores y Perdedores.");
}

function simulateGroupStage() {
  if (!tournamentState.groups.length) {
    generateTournament();
  }

  tournamentState.groupMatches.filter((match) => !hasValidScore(match)).forEach((match) => {
    const [homePoints, awayPoints] = randomBeachScore();
    match.homePoints = homePoints;
    match.awayPoints = awayPoints;
  });

  tournamentState.finalPhase = null;
  tournamentState.knockoutScores = {};
  tournamentState.champion = null;
  renderGroupStage();
  renderKnockoutStage();
  setTournamentStatus("Partidos pendientes completados con ejemplos. Revisa los resultados y crea la fase final.");
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
    tournamentState.knockoutScores[openMatch.id] = { home: openMatch.home, away: openMatch.away, homePoints, awayPoints };
    safety += 1;
  }

  renderKnockoutStage();
  const champions = buildCurrentFinalPhase().map((flight) => flight.champion).filter(Boolean);
  setTournamentStatus(champions.length ? `Campeones: ${champions.join(" · ")}.` : "Fase final simulada parcialmente.");
}

function renderTournamentPodium(flight) {
  const lastLosersRound = flight.losersRounds[flight.losersRounds.length - 1];
  const bronze = flight.isFinalFour ? flight.bronze : lastLosersRound?.matches.length === 1 ? lastLosersRound.matches[0].loser : null;
  const places = [
    ['🥇', '1.º · Oro', flight.champion],
    ['🥈', '2.º · Plata', flight.champion ? flight.finalMatch?.loser : null],
    ['🥉', '3.º · Bronce', bronze]
  ];
  return `<section class="tournament-podium" aria-label="Podio de ${escapeHTML(flight.name)}">
    <h5>Podio · ${escapeHTML(flight.name)}</h5>
    <ol>${places.map(([medal, label, team], index) => `<li><span aria-hidden="true">${medal}</span><strong>${label}</strong><span>${team ? escapeHTML(team) : index === 2 && flight.teams.length < 3 ? 'No se disputa · menos de 3 equipos' : 'Pendiente de resultados'}</span></li>`).join('')}</ol>
  </section>`;
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
      <input class="knockout-score-input" type="number" min="0" step="1" inputmode="numeric" value="${match.homePoints ?? ""}" data-match="${match.id}" data-side="home" aria-label="Puntos de ${escapeHTML(match.home)}" />
    </div>
    <div class="bracket-scoreline ${match.winner === match.away ? "is-winner" : ""}">
      <span>${escapeHTML(match.away)}</span>
      <input class="knockout-score-input" type="number" min="0" step="1" inputmode="numeric" value="${match.awayPoints ?? ""}" data-match="${match.id}" data-side="away" aria-label="Puntos de ${escapeHTML(match.away)}" />
    </div>
  `;

  const renderMatch = (match) => `
    <article class="bracket-match">
      ${
        match.away
          ? renderScoreInputs(match)
          : `<div class="bracket-scoreline is-winner"><span>${escapeHTML(match.home)}</span><strong>Bye</strong></div>`
      }
      ${match.homePoints !== null && match.awayPoints !== null && !hasValidScore(match) ? '<p class="match-feedback">Corrige el empate para decidir quién avanza.</p>' : ''}
    </article>
  `;

  const renderPendingMatch = (home, away) => `<article class="bracket-match pending-match"><div class="bracket-scoreline"><span>${home}</span></div><div class="bracket-scoreline"><span>${away}</span></div><p>Pendiente de las semifinales</p></article>`;
  const renderFinalFour = (flight) => `
    <p class="final-format-note">Clasifican los ${flight.qualified.length} primeros de esta categoría, según la clasificación de grupos.${flight.qualified.length === 3 ? ' El primer clasificado pasa directamente a la final; el perdedor de la semifinal es tercero.' : ''}</p>
    <div class="bracket-columns final-four-columns">
      <section class="round-column"><h5>Semifinales</h5>
        ${flight.semifinals.map((match, index) => `<div class="bracket-round"><h6>Semifinal ${index + 1}</h6>${renderMatch(match)}</div>`).join('') || '<p>No se disputan con solo dos equipos.</p>'}
      </section>
      <section class="round-column"><h5>Finales</h5>
        <div class="bracket-round"><h6>🥇 Final · 1.º y 2.º puesto</h6>${flight.finalMatch ? renderMatch(flight.finalMatch) : renderPendingMatch('Ganador de semifinal 1', 'Ganador de semifinal 2')}</div>
        <div class="bracket-round"><h6>🥉 Partido por el 3.º puesto</h6>${flight.bronzeMatch ? renderMatch(flight.bronzeMatch) : flight.qualified.length >= 4 ? renderPendingMatch('Perdedor de semifinal 1', 'Perdedor de semifinal 2') : flight.qualified.length === 3 ? '<p>El perdedor de la semifinal obtiene el bronce.</p>' : '<p>No se disputa · menos de 3 equipos.</p>'}</div>
      </section>
    </div>`;

  stage.innerHTML = `
    <div class="final-format-note">
      <strong>Formato:</strong>
      <span>${({"final-four": "Grupos → Final Four", "gold-silver-final-four": "Oro y plata → Final Four", "gold-silver": "Oro y plata · doble eliminación", "double": "Todos · doble eliminación"})[tournamentState.finalPhase.format]}</span>
    </div>
    <div class="flight-stack">
      ${flights
        .map(
          (flight) => `
            <section class="flight-card">
              <div class="flight-heading">
                <h4>${escapeHTML(flight.name)}</h4>
                <span>${flight.teams.length} equipos</span>
              </div>
              ${flight.isFinalFour ? renderFinalFour(flight) : `
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
                      : `<div class="empty-state small"><p>Completa la fase de ganadores para abrir el cuadro de segunda oportunidad.</p></div>`
                  }
                </section>
                <section class="round-column">
                  <h5>Final</h5>
                  ${flight.grandFinalMatch ? `<div class="bracket-round"><h6>Gran final · 1.º y 2.º puesto</h6>${renderMatch(flight.grandFinalMatch)}</div>${flight.resetMatch ? `<div class="bracket-round"><h6>Final definitiva</h6><p>Ambos equipos tienen una derrota. Este partido decide el título.</p>${renderMatch(flight.resetMatch)}</div>` : '<p>Si gana el equipo de perdedores, se juega una final definitiva.</p>'}` : `<div class="empty-state small"><p>Completa ambas fases para abrir la final.</p></div>`}
                  <p>El perdedor del último partido de la fase de perdedores obtiene el tercer puesto.</p>
                </section>
              </div>
              `}
              ${renderTournamentPodium(flight)}
            </section>
          `
        )
        .join("")}
    </div>
  `;

  stage.querySelectorAll(".knockout-score-input").forEach((input) => {
    input.addEventListener("input", () => {
      const match = getAllKnockoutMatches(flights).find((item) => item.id === input.dataset.match);
      if (!match) return;
      const current = tournamentState.knockoutScores[input.dataset.match] || {};
      current.home = match.home;
      current.away = match.away;
      current[input.dataset.side === "home" ? "homePoints" : "awayPoints"] = parseScore(input.value);
      tournamentState.knockoutScores[input.dataset.match] = current;
      pruneKnockoutScores();
      setTournamentStatus("Resultado guardado. Los cruces se actualizan al salir del campo.");
    });
    input.addEventListener("blur", () => {
      window.setTimeout(() => {
        const focused = document.activeElement;
        const focusMatch = focused?.dataset.match;
        const focusSide = focused?.dataset.side;
        renderKnockoutStage();
        if (focusMatch && focusSide) stage.querySelector(`[data-match="${focusMatch}"][data-side="${focusSide}"]`)?.focus({preventScroll: true});
      }, 0);
    });
  });
}

function generateTournament() {
  if (tournamentState.groupMatches.some((match) => match.homePoints !== null || match.awayPoints !== null) &&
    !window.confirm("Volver a crear los grupos borrará los resultados de este torneo. Los nombres se conservarán. ¿Continuar?")) return;
  const teams = getTournamentTeams();
  tournamentState.format = document.querySelector("#tournamentFormat")?.value || "final-four";
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
  if (tournamentState.groups.length && !window.confirm("¿Empezar un torneo nuevo? Se borrarán los resultados. Los nombres de los equipos se conservarán.")) return;
  tournamentState.format = null;
  tournamentState.teams = [];
  tournamentState.groups = [];
  tournamentState.groupMatches = [];
  tournamentState.finalPhase = null;
  tournamentState.knockoutScores = {};
  tournamentState.champion = null;
  const countInput = document.querySelector("#tournamentTeamCount");
  renderTeamInputs(clampTournamentTeams(countInput?.value));
  renderGroupStage();
  renderKnockoutStage();
  setTournamentStatus("Torneo reiniciado. Define los equipos para empezar.");
}

function setupTournamentControls() {
  const countInput = document.querySelector("#tournamentTeamCount");
  if (!countInput) return;

  const restored = restorePlayState();
  if (!restored) renderTeamInputs(clampTournamentTeams(countInput.value));
  renderGroupStage();
  renderKnockoutStage();
  updateTournamentControls();
  if (restored) setTournamentStatus("Progreso recuperado. Puedes continuar donde lo dejaste.");

  countInput.addEventListener("change", () => {
    countInput.value = String(clampTournamentTeams(countInput.value));
    renderTeamInputs(Number(countInput.value));
    setTournamentStatus(tournamentState.groups.length ? "Cantidad preparada. Vuelve a crear los grupos para aplicar el cambio." : "Lista actualizada. Escribe los nombres y crea los grupos.");
  });
  document.querySelector("#tournamentFormat")?.addEventListener("change", () => {
    setTournamentStatus(tournamentState.groups.length ? "Formato preparado. Vuelve a crear los grupos para aplicar el cambio." : "Formato elegido. Escribe los equipos y crea los grupos.");
  });
  document.querySelector("#teamInputs")?.addEventListener("input", () => {
    updateTournamentControls();
    savePlayState();
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

function getSideChangeInterval() {
  return scoreboardState.target === 15 ? 5 : 7;
}

function getPendingSideChange() {
  const total = scoreboardState.scores[0] + scoreboardState.scores[1];
  const interval = getSideChangeInterval();
  for (let point = interval; point <= total; point += interval) {
    if (!scoreboardState.confirmedSideTotals.has(point)) return point;
  }
  return null;
}

function getSetWinner() {
  const [home, away] = scoreboardState.scores;
  return Math.max(home, away) >= scoreboardState.target && Math.abs(home - away) >= 2 ? (home > away ? 0 : 1) : null;
}

function rememberScoreState() {
  scoreboardState.history.push({
    scores: [...scoreboardState.scores],
    confirmedSideTotals: [...scoreboardState.confirmedSideTotals],
    displayOrder: [...scoreboardState.displayOrder],
    log: [...scoreboardState.log]
  });
  if (scoreboardState.history.length > 100) scoreboardState.history.shift();
}

function renderScoreboard() {
  const scoreA = document.querySelector("#scoreA");
  const scoreB = document.querySelector("#scoreB");
  if (!scoreA || !scoreB) return;

  const names = getScoreNames();
  const [leftTeam, rightTeam] = scoreboardState.displayOrder;
  const total = scoreboardState.scores[0] + scoreboardState.scores[1];
  const interval = getSideChangeInterval();
  const pendingChange = getPendingSideChange();
  const nextChange = pendingChange || (Math.floor(total / interval) + 1) * interval;
  const needsSideChange = pendingChange !== null;
  const winner = getSetWinner();

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
      button.disabled = Number(button.dataset.delta) > 0 ? winner !== null : scoreboardState.scores[teamIndex] === 0;
    });
  });

  const notice = document.querySelector("#sideChangeNotice");
  if (notice) {
    notice.classList.toggle("is-active", needsSideChange && winner === null);
    notice.innerHTML = winner !== null
      ? `<strong>Set para ${escapeHTML(names[winner])}</strong><span>${scoreboardState.scores[0]}–${scoreboardState.scores[1]}. Reinicia el marcador para el siguiente set.</span>`
      : needsSideChange
        ? `<strong>Cambio de lado pendiente</strong><span>Corresponde al punto total ${pendingChange}. Confírmalo cuando cambiéis de campo.</span>`
        : `<span>Próximo cambio de lado en el punto total ${nextChange}.</span>`;
  }

  const confirmButton = document.querySelector("#confirmSideChange");
  if (confirmButton) {
    confirmButton.disabled = !needsSideChange || winner !== null;
  }

  const summary = document.querySelector("#matchSummary");
  if (summary) {
    summary.innerHTML = `
      <div><strong>Total jugado</strong><span>${total} puntos</span></div>
      <div><strong>Objetivo del set</strong><span>${scoreboardState.target} · ventaja de 2</span></div>
      <div><strong>${needsSideChange ? "Cambio pendiente" : "Próximo cambio"}</strong><span>${winner !== null ? "Set terminado" : nextChange}</span></div>
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
  const undo = document.querySelector("#undoScore");
  if (undo) undo.disabled = scoreboardState.history.length === 0;
  savePlayState();
}

function updateScore(teamIndex, delta) {
  if (delta > 0 && getSetWinner() !== null) return;
  const nextScore = Math.max(0, scoreboardState.scores[teamIndex] + delta);
  if (nextScore === scoreboardState.scores[teamIndex]) return;

  rememberScoreState();
  scoreboardState.scores[teamIndex] = nextScore;
  const names = getScoreNames();
  const action = delta > 0 ? "suma" : "resta";
  addScoreLog(`${names[teamIndex]} ${action} punto. Marcador ${scoreboardState.scores[0]}-${scoreboardState.scores[1]}.`);

  const total = scoreboardState.scores[0] + scoreboardState.scores[1];
  if (delta < 0) {
    scoreboardState.confirmedSideTotals = new Set([...scoreboardState.confirmedSideTotals].filter((point) => point <= total));
    scoreboardState.displayOrder = scoreboardState.confirmedSideTotals.size % 2 ? [1, 0] : [0, 1];
  }
  if (delta > 0 && total > 0 && total % getSideChangeInterval() === 0 && !scoreboardState.confirmedSideTotals.has(total)) {
    addScoreLog(`Aviso de cambio de lado en el punto total ${total}.`);
  }

  renderScoreboard();
}

function confirmSideChange() {
  const point = getPendingSideChange();
  if (point === null || getSetWinner() !== null) return;
  scoreboardState.confirmedSideTotals.add(point);
  scoreboardState.displayOrder = [...scoreboardState.displayOrder].reverse();
  addScoreLog(`Cambio de lado del punto ${point} confirmado.`);
  renderScoreboard();
}

function resetScoreboard() {
  if (scoreboardState.scores.some((score) => score > 0) && !window.confirm("¿Reiniciar este set a 0–0? Los nombres y el tipo de set se conservarán.")) return;
  scoreboardState.scores = [0, 0];
  scoreboardState.displayOrder = [0, 1];
  scoreboardState.confirmedSideTotals = new Set();
  scoreboardState.log = [];
  scoreboardState.history = [];
  renderScoreboard();
}

function setupScoreboardControls() {
  if (!document.querySelector("#scoreA")) return;

  document.querySelectorAll(".score-control").forEach((button) => {
    button.addEventListener("click", () => {
      updateScore(Number(button.dataset.team), Number(button.dataset.delta));
    });
  });
  document.querySelector("#scoreSetTarget")?.addEventListener("change", (event) => {
    const target = Number(event.target.value);
    if (scoreboardState.scores.some((score) => score > 0)) {
      if (!window.confirm("Cambiar el tipo de set reiniciará el marcador a 0–0. ¿Continuar?")) {
        event.target.value = String(scoreboardState.target);
        return;
      }
      scoreboardState.scores = [0, 0];
      scoreboardState.confirmedSideTotals = new Set();
      scoreboardState.displayOrder = [0, 1];
      scoreboardState.history = [];
      scoreboardState.log = [];
    }
    scoreboardState.target = target === 15 ? 15 : 21;
    renderScoreboard();
  });
  document.querySelector("#undoScore")?.addEventListener("click", () => {
    const previous = scoreboardState.history.pop();
    if (!previous) return;
    scoreboardState.scores = previous.scores;
    scoreboardState.confirmedSideTotals = new Set(previous.confirmedSideTotals);
    scoreboardState.displayOrder = previous.displayOrder;
    scoreboardState.log = previous.log;
    renderScoreboard();
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



if (document.querySelector("#signalCourt")) {
  renderSignalControls();
  renderSignalDemo();
  setupSignalAnimationControls();
}

if (document.querySelector("#settingCourt")) {
  renderSettingControls();
  renderSettingDemo();
  setupSettingAnimationControls();
}

renderMovementTabs();
renderMovement();
renderCallbook();
renderLevels();
renderTraining();
setupTournamentControls();
setupScoreboardControls();


function updateCurrentNavigation() {
  const tournamentPage = Boolean(document.querySelector('#torneo'));
  const targetHash = tournamentPage ? (window.location.hash === '#marcador' ? '#marcador' : '#torneo') : '#entrenar';
  document.querySelectorAll('.main-nav a').forEach(link => {
    const target = new URL(link.href, window.location.href);
    if (target.pathname === window.location.pathname && target.hash === targetHash) link.setAttribute('aria-current', 'location');
    else link.removeAttribute('aria-current');
  });
}
window.addEventListener('hashchange', updateCurrentNavigation);
updateCurrentNavigation();

// Restore section links after the dynamic groups and format guide have been laid out.
if (window.location.hash && 'scrollRestoration' in window.history) window.history.scrollRestoration = 'manual';
window.addEventListener('load', () => {
  if (!window.location.hash) return;
  try {
    const target = document.getElementById(decodeURIComponent(window.location.hash.slice(1)));
    if (target) window.requestAnimationFrame(() => target.scrollIntoView({behavior: 'instant', block: 'start'}));
  } catch {
    // An invalid fragment must not interrupt the page.
  }
}, {once: true});
