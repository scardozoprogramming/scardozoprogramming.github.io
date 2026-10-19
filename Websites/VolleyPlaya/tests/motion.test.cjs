// Run with node --test tests/motion.test.cjs.
const {test} = require('node:test');
const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');
const path = require('node:path');
const {create, createTimeline} = require('../site/court-motion.js');

const sample = [
  {name: 'Base', players: {a: [10, 20], b: [25, 30]}, ball: [10, 20]},
  {name: 'Pase', players: {a: [35, 50], b: [40, 25]}, ball: [40, 25]},
  {name: 'Ataque', players: {a: [46, 50], b: [35, 40]}, ball: [58, 50]}
];

function fixture(reduced = false) {
  let time = 0;
  let nextId = 0;
  let latest;
  let reduceMotion = reduced;
  const callbacks = new Map();
  const requested = new Map();
  const timeline = createTimeline({
    duration: 1000,
    now: () => time,
    reducedMotion: () => reduceMotion,
    requestFrame(callback) {
      const id = ++nextId;
      callbacks.set(id, callback);
      requested.set(id, callback);
      return id;
    },
    cancelFrame: (id) => callbacks.delete(id),
    render: (frame) => { latest = frame; }
  });
  timeline.load(sample);
  return {
    timeline, requested,
    frame: () => latest,
    reduce: (value) => { reduceMotion = value; },
    pending: () => callbacks.size,
    tick(milliseconds) {
      const until = time + milliseconds;
      while (time < until) {
        time = Math.min(until, time + 10);
        const pending = Array.from(callbacks.values());
        callbacks.clear();
        pending.forEach((callback) => callback(time));
      }
    }
  };
}

test('play interpolates the court, pause freezes it, resume continues without a jump', () => {
  const f = fixture();
  assert.equal(f.timeline.getState().playing, false);
  assert.equal(f.pending(), 0);
  f.timeline.play();
  f.tick(300);
  const point = [...f.frame().players.a];
  assert.ok(point[0] > 10 && point[0] < 35);
  assert.ok(f.frame().ball[0] > 10 && f.frame().ball[0] < 40);
  assert.ok(f.frame().height > 0);
  f.timeline.pause();
  f.tick(900);
  assert.deepEqual(f.frame().players.a, point);
  assert.equal(f.pending(), 0);
  f.timeline.play();
  assert.deepEqual(f.frame().players.a, point);
  f.tick(100);
  assert.ok(f.frame().players.a[0] > point[0]);
});

test('manual selection clamps boundaries and cancels in-flight work', () => {
  const f = fixture();
  f.timeline.play();
  const staleCallback = [...f.requested.values()].at(-1);
  f.tick(250);
  f.timeline.seek(-1);
  assert.equal(f.timeline.getState().index, 0);
  assert.deepEqual(f.frame().players.a, sample[0].players.a);
  staleCallback(500);
  assert.deepEqual(f.frame().players.a, sample[0].players.a);
  f.timeline.seek(500);
  assert.equal(f.timeline.getState().index, sample.length - 1);
  assert.equal(f.timeline.getState().playing, false);
  assert.equal(f.pending(), 0);
});

test('loading a different mode and restarting invalidate old callbacks', () => {
  const f = fixture();
  f.timeline.play();
  const staleCallback = [...f.requested.values()].at(-1);
  f.tick(250);
  const mode = sample.map((step) => ({...step, ball: [80, 80]}));
  f.timeline.load(mode);
  staleCallback(1000);
  assert.deepEqual(f.frame().ball, [80, 80]);
  f.timeline.play();
  f.tick(250);
  f.timeline.restart();
  f.tick(1000);
  assert.equal(f.timeline.getState().index, 0);
  assert.equal(f.timeline.getState().playing, false);
  assert.deepEqual(f.frame().players, sample[0].players);
});

test('playback ends at the last phase and only explicit Play restarts it', () => {
  const f = fixture();
  f.timeline.play();
  f.tick(2000);
  assert.deepEqual(f.frame().ball, sample.at(-1).ball);
  assert.equal(f.frame().progress, 1);
  assert.equal(f.timeline.getState().complete, true);
  assert.equal(f.timeline.getState().playing, false);
  assert.equal(f.pending(), 0);
  f.tick(3000);
  assert.equal(f.timeline.getState().index, sample.length - 1);
  f.timeline.play();
  assert.equal(f.timeline.getState().index, 1);
  assert.deepEqual(f.frame().ball, sample[0].ball);
});

test('speed changes preserve position and change remaining playback time', () => {
  const f = fixture();
  f.timeline.setSpeed(0.7);
  f.timeline.play();
  f.tick(400);
  assert.ok(Math.abs(f.timeline.getState().elapsed - 280) < 0.001);
  const current = [...f.frame().players.a];
  f.timeline.setSpeed(1.5);
  assert.deepEqual(f.frame().players.a, current);
  f.tick(400);
  assert.ok(Math.abs(f.timeline.getState().elapsed - 880) < 0.001);
  f.tick(80);
  assert.equal(f.timeline.getState().index, 2);
  f.timeline.setSpeed('invalid');
  assert.equal(f.timeline.getState().speed, 1);
});

test('reduced motion advances by static steps and never schedules motion', () => {
  const f = fixture(true);
  f.timeline.play();
  assert.equal(f.timeline.getState().index, 1);
  assert.deepEqual(f.frame().ball, sample[1].ball);
  assert.equal(f.pending(), 0);
  f.reduce(false);
  f.timeline.play();
  f.tick(100);
  f.reduce(true);
  f.tick(10);
  assert.equal(f.timeline.getState().playing, false);
  assert.deepEqual(f.frame().ball, sample[2].ball);
  assert.equal(f.pending(), 0);
});

test('route visibility is independent from a paused position and progress', () => {
  const f = fixture();
  f.timeline.play();
  f.tick(300);
  f.timeline.pause();
  const point = [...f.frame().ball];
  const progress = f.frame().progress;
  f.timeline.setRoutes(false);
  assert.equal(f.frame().routes, false);
  assert.deepEqual(f.frame().ball, point);
  assert.equal(f.frame().progress, progress);
});

test('contact ball drawing moves away from role glyphs without moving its anchor', () => {
  const node = () => ({style: {}, classList: {toggle() {}}, setAttribute() {}});
  const ball = node();
  const shadow = node();
  const blocker = node();
  const court = {querySelector: () => shadow, dataset: {}};
  const timeline = create({court, players: {blocker}, ball});
  timeline.load([{name: 'Ataque', players: {blocker: [54, 72]}, ball: [52, 72]}]);
  assert.match(ball.style.transform, /translate\(-12px, -12px\)/);
  assert.equal(ball.style.left, '52%');
  assert.equal(ball.style.top, '72%');
  assert.equal(blocker.style.left, '54%');
  timeline.load([{name: 'Contacto', players: {blocker: [52, 72]}, ball: [54, 74]}]);
  assert.match(ball.style.transform, /translate\(12px, 12px\)/);
  timeline.load([{name: 'Saque', players: {blocker: [52, 72]}, ball: [52, 72]}]);
  assert.match(ball.style.transform, /translate\(12px, -12px\)/);
  timeline.load([{name: 'Vuelo', players: {blocker: [52, 72]}, ball: [25, 40]}]);
  assert.match(ball.style.transform, /translate\(0px, 0px\)/);
});

test('all eight teaching modes preserve their final anchors', () => {
  const source = fs.readFileSync(path.join(__dirname, '../site/app.js'), 'utf8');
  const data = source.slice(0, source.indexOf('let movementPhases ='));
  const context = {};
  vm.createContext(context);
  vm.runInContext(`${data}\nthis.courtSteps = signalModes.map(buildSignalSteps).concat(settingModes.map(buildSettingSteps));`, context);
  assert.equal(context.courtSteps.length, 8);
  for (const steps of context.courtSteps) {
    const f = fixture();
    f.timeline.load(steps);
    f.timeline.play();
    f.tick(steps.length * 1000);
    assert.equal(f.timeline.getState().complete, true);
    assert.deepEqual(Array.from(f.frame().ball), Array.from(steps.at(-1).ball));
    for (const [role, point] of Object.entries(steps.at(-1).players)) {
      assert.deepEqual(Array.from(f.frame().players[role]), Array.from(point));
    }
  }
});
