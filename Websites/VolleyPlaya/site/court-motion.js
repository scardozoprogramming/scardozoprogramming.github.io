/* Shared playback for the two teaching courts. Projected routes stay straight;
   scale and shadow suggest ball height without changing its landing point. */
(function (root, factory) {
  const api = factory(root);
  if (typeof module === "object" && module.exports) module.exports = api;
  else root.CourtMotion = api;
})(typeof window === "undefined" ? globalThis : window, function (root) {
  "use strict";

  const clamp = (value, min = 0, max = 1) => Math.max(min, Math.min(max, value));
  const ease = (value) => value * value * (3 - 2 * value);
  const distance = (from, to) => Math.hypot(to[0] - from[0], to[1] - from[1]);
  const pointBetween = (from, to, progress) => from.map((value, index) => value + (to[index] - value) * progress);
  const snapshot = (step) => ({
    players: Object.fromEntries(Object.entries(step.players).map(([role, point]) => [role, [...point]])),
    ball: [...step.ball]
  });

  // The clock is injectable so cancellation, pause and speed changes can be tested.
  function createTimeline(options = {}) {
    const requestFrame = options.requestFrame || ((callback) => root.requestAnimationFrame(callback));
    const cancelFrame = options.cancelFrame || ((id) => root.cancelAnimationFrame(id));
    const now = options.now || (() => root.performance.now());
    const reducedMotion = options.reducedMotion || (() => false);
    const duration = options.duration || 2500;
    let steps = [];
    let index = 0;
    let previous = null;
    let current = null;
    let segment = null;
    let speed = 1;
    let routes = true;
    let playing = false;
    let frameId = null;
    let generation = 0;
    let lastTime = 0;

    function getState() {
      return {
        index, playing, speed, routes,
        complete: Boolean(steps.length && index === steps.length - 1 && !segment),
        transitioning: Boolean(segment),
        elapsed: segment ? segment.elapsed : 0
      };
    }

    function notifyState() {
      options.onStateChange?.(getState());
    }

    function emitFrame() {
      if (!current) return;
      const phaseProgress = segment ? clamp(segment.elapsed / duration) : 1;
      const wholeProgress = segment ? index - 1 + phaseProgress : index;
      options.render?.({
        ...current, step: steps[index], index, previous,
        phaseProgress,
        progress: steps.length > 1 ? clamp(wholeProgress / (steps.length - 1)) : 0,
        height: segment ? segment.height : 0,
        moving: Boolean(segment && phaseProgress < 0.72),
        ...getState()
      });
    }

    function cancelScheduledFrame() {
      generation += 1;
      if (frameId !== null) cancelFrame(frameId);
      frameId = null;
    }

    function notifyStep() {
      options.onStep?.(index, steps[index]);
    }

    function startNextSegment() {
      if (index >= steps.length - 1) return false;
      previous = snapshot(current);
      index += 1;
      segment = { from: snapshot(current), elapsed: 0, height: 0 };
      notifyStep();
      return true;
    }

    function schedule() {
      const scheduledGeneration = generation;
      frameId = requestFrame((time) => {
        if (!playing || scheduledGeneration !== generation) return;
        frameId = null;
        if (reducedMotion()) {
          seek(index);
          return;
        }
        // A background tab cannot skip an entire teaching sequence on return.
        const elapsed = Math.min(100, Math.max(0, time - lastTime));
        lastTime = time;
        segment.elapsed += elapsed * speed;
        const phase = clamp(segment.elapsed / duration);
        const playerProgress = ease(clamp(phase / 0.64));
        const ballProgress = ease(clamp((phase - 0.10) / 0.60));
        const target = steps[index];
        current = {
          players: Object.fromEntries(Object.entries(target.players).map(([role, position]) => [
            role, pointBetween(segment.from.players[role] || position, position, playerProgress)
          ])),
          ball: pointBetween(segment.from.ball, target.ball, ballProgress)
        };
        segment.height = distance(segment.from.ball, target.ball) > 2
          ? Math.sin(Math.PI * ballProgress)
          : 0;
        emitFrame();
        if (phase === 1) {
          current = snapshot(target);
          segment = null;
          if (!startNextSegment()) {
            playing = false;
            notifyState();
            emitFrame();
            return;
          }
          emitFrame();
        }
        schedule();
      });
    }

    function pause() {
      playing = false;
      cancelScheduledFrame();
      notifyState();
      emitFrame();
    }

    function seek(nextIndex) {
      cancelScheduledFrame();
      playing = false;
      segment = null;
      if (!steps.length) return;
      index = clamp(Math.trunc(nextIndex), 0, steps.length - 1);
      previous = index > 0 ? snapshot(steps[index - 1]) : null;
      current = snapshot(steps[index]);
      notifyStep();
      notifyState();
      emitFrame();
    }

    function play() {
      if (!steps.length || playing) return;
      if (reducedMotion()) {
        seek(index >= steps.length - 1 ? 0 : index + 1);
        return;
      }
      if (!segment && index >= steps.length - 1) seek(0);
      if (!segment && !startNextSegment()) return;
      playing = true;
      lastTime = now();
      notifyState();
      emitFrame();
      schedule();
    }

    return {
      load(nextSteps, nextIndex = 0) {
        steps = nextSteps;
        seek(nextIndex);
      },
      play, pause, seek,
      restart() { seek(0); },
      setSpeed(value) {
        const parsed = Number(value);
        speed = [0.7, 1, 1.5].includes(parsed) ? parsed : 1;
        notifyState();
      },
      setRoutes(visible) {
        routes = Boolean(visible);
        emitFrame();
      },
      getState,
      destroy() {
        pause();
        steps = [];
        current = null;
      }
    };
  }

  function create(options) {
    const court = options.court;
    const players = options.players;
    const ball = options.ball;
    const layer = options.trails;
    const svgNS = "http://www.w3.org/2000/svg";
    const reducedMotion = () => root.matchMedia?.("(prefers-reduced-motion: reduce)").matches || false;
    const shadow = court.querySelector(".court-ball-shadow") || court.appendChild(court.ownerDocument.createElement("span"));
    shadow.className = "court-ball-shadow";
    shadow.setAttribute("aria-hidden", "true");
    // rAF owns the position: old CSS transitions would otherwise trail behind it.
    Object.values(players).concat(ball, shadow).filter(Boolean).forEach((node) => {
      node.style.transition = "none";
    });
    ball.style.animation = "none";
    let routeKey = "";

    function svgNode(tag, attributes) {
      const node = court.ownerDocument.createElementNS(svgNS, tag);
      Object.entries(attributes).forEach(([name, value]) => node.setAttribute(name, value));
      return node;
    }

    function renderRoutes(frame) {
      if (!layer) return;
      layer.style.display = frame.routes ? "" : "none";
      const key = JSON.stringify([frame.previous, frame.step.players, frame.step.ball]);
      if (key === routeKey) return;
      routeKey = key;
      layer.replaceChildren();
      if (!frame.previous) return;
      const definitions = svgNode("defs", {});
      layer.appendChild(definitions);
      const segments = (options.routeRoles || Object.keys(players)).map((role) => ({
        role,
        from: frame.previous.players[role], to: frame.step.players[role],
        color: options.colors?.[role] || "#176765"
      })).concat({role: "ball", from: frame.previous.ball, to: frame.step.ball, color: "#8c610b"});
      segments.forEach(({role, from, to, color}) => {
        if (!from || distance(from, to) < 1) return;
        const markerId = `${court.id}-motion-${role}`;
        const marker = svgNode("marker", {id: markerId, viewBox: "0 0 10 10", refX: 9, refY: 5, markerWidth: 4, markerHeight: 4, orient: "auto"});
        marker.appendChild(svgNode("path", {d: "M 0 0 L 10 5 L 0 10 z", fill: color}));
        definitions.appendChild(marker);
        const coordinates = {x1: from[0], y1: from[1], x2: to[0], y2: to[1]};
        if (role === "ball") layer.appendChild(svgNode("line", {...coordinates, class: "motion-route-underlay", stroke: "#fff", "stroke-width": 1.1}));
        layer.appendChild(svgNode("line", {
          ...coordinates,
          class: `motion-route motion-route-${role === "ball" ? "ball" : "player"}`,
          "data-role": role, stroke: color, "stroke-width": 0.65,
          "marker-end": `url(#${markerId})`
        }));
      });
    }

    function position(node, point) {
      if (!node) return;
      node.style.left = `${point[0]}%`;
      node.style.top = `${point[1]}%`;
    }

    function contactBallOffset(frame) {
      const nearest = Object.values(frame.players).reduce((closest, point) =>
        !closest || distance(point, frame.ball) < distance(closest, frame.ball) ? point : closest, null);
      if (!nearest || distance(nearest, frame.ball) > 5) return [0, 0];
      // Separate the drawing from the closest role glyph, never the logical route.
      const amount = 12 * (1 - frame.height);
      return [
        amount * (frame.ball[0] < nearest[0] ? -1 : 1),
        amount * (frame.ball[1] > nearest[1] ? 1 : -1)
      ];
    }

    const timeline = createTimeline({
      duration: options.duration || 2500,
      reducedMotion,
      onStep(index, step) {
        if (options.cue) options.cue.textContent = options.getCue?.(step, index) || step.name;
        options.onStep?.(index, step);
      },
      onStateChange: options.onStateChange,
      render(frame) {
        const focusRole = options.contactRoles?.[frame.index];
        Object.entries(players).forEach(([role, node]) => {
          if (!node) return;
          position(node, frame.players[role]);
          const changing = frame.previous && distance(frame.previous.players[role], frame.step.players[role]) > 1;
          node.classList.toggle("is-moving", Boolean(frame.playing && frame.moving && changing));
          node.classList.toggle("is-ball-carrier", role === focusRole && distance(frame.players[role], frame.ball) <= 4 && frame.height < 0.2);
        });
        position(ball, frame.ball);
        position(shadow, frame.ball);
        const [offsetX, offsetY] = contactBallOffset(frame);
        ball.style.transform = `translate(-50%, -50%) translate(${offsetX}px, ${offsetY}px) scale(${1 + frame.height * 0.28})`;
        ball.classList.toggle("is-airborne", frame.height > 0.1);
        shadow.style.opacity = String(0.16 + frame.height * 0.20);
        shadow.style.transform = `translate(-50%, -50%) scale(${1 - frame.height * 0.25})`;
        court.dataset.motionState = frame.playing ? "playing" : frame.complete ? "complete" : frame.transitioning ? "paused" : "ready";
        if (options.progress) options.progress.value = frame.progress * 100;
        renderRoutes(frame);
      }
    });
    return timeline;
  }

  return {create, createTimeline};
});
