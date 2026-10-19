// Regression checks for bracket progression. Run with: node --test tests/tournament.test.cjs
const { test } = require('node:test');
const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');
const path = require('node:path');
const source = fs.readFileSync(path.join(__dirname, '../site/app.js'), 'utf8');
function engine() {
  const context = { tournamentState: { knockoutScores: {}, finalPhase: null } };
  vm.createContext(context);
  const scoreHelpers = source.slice(source.indexOf('function parseScore('), source.indexOf('function randomBeachScore('));
  const bracket = source.slice(source.indexOf('function pairBracketTeams('), source.indexOf('function buildBracketFromStandings('));
  const roundNames = source.slice(source.indexOf('function getWinnerRoundName('), source.indexOf('function getRankedGroupTeams('));
  vm.runInContext(scoreHelpers + roundNames + bracket, context);
  return context;
}
function record(context, match, awayWins = false) {
  context.tournamentState.knockoutScores[match.id] = {
    home: match.home, away: match.away,
    homePoints: awayWins ? 15 : 21, awayPoints: awayWins ? 21 : 15
  };
}
function complete(context, format, teams) {
  context.tournamentState.finalPhase = {format, flights: [{id:'general',name:'Torneo',teams}]};
  for (let guard = 0; guard < 100; guard++) {
    const flight = context.buildCurrentFinalPhase()[0];
    const open = context.getAllKnockoutMatches([flight]).find(context.isEditableMatch);
    if (!open) return flight;
    // Force the winners finalist to lose the first grand final, exercising the reset.
    record(context, open, open.id === 'general-final-0' && format === 'double');
  }
  throw Error('Bracket did not terminate');
}

test('Final Four assigns winners to gold and semifinal losers to bronze', () => {
  const e = engine();
  e.tournamentState.finalPhase = {format:'final-four',flights:[{id:'general',teams:['A','B','C','D']}]};
  let f = e.buildCurrentFinalPhase()[0];
  assert.equal(f.semifinals.length, 2);
  assert.deepEqual(Array.from(f.semifinals, m => [m.home,m.away]), [['A','D'],['B','C']]);
  record(e, f.semifinals[0]); record(e, f.semifinals[1], true);
  f = e.buildCurrentFinalPhase()[0];
  assert.deepEqual([f.finalMatch.home,f.finalMatch.away], ['A','C']);
  assert.deepEqual([f.bronzeMatch.home,f.bronzeMatch.away], ['D','B']);
  record(e, f.finalMatch); record(e, f.bronzeMatch, true);
  f = e.buildCurrentFinalPhase()[0];
  assert.equal(f.champion,'A'); assert.equal(f.finalMatch.loser,'C'); assert.equal(f.bronze,'B');
  record(e, f.semifinals[0], true); e.pruneKnockoutScores();
  f = e.buildCurrentFinalPhase()[0];
  assert.equal(f.finalMatch.home,'D'); assert.equal(f.champion,null); assert.equal(f.bronze,null);
});

test('Both bracket formats complete for every supported category size', () => {
  for (const format of ['final-four','double']) for (let count=2; count<=32; count++) {
    const e=engine(); const teams=Array.from({length:count},(_,i)=>`Equipo ${i+1}`);
    const f=complete(e,format,teams);
    assert(f.champion, `${format}: ${count} teams`);
    assert(f.finalMatch.loser); assert.notEqual(f.champion,f.finalMatch.loser);
    if (format==='final-four' && count>=3) assert(f.bronze);
    if (format==='double') {
      assert(f.resetMatch?.winner, 'Must play reset when undefeated finalist loses');
      const losses=new Map(teams.map(team=>[team,0]));
      for(const match of e.getAllKnockoutMatches([f])) if(match.loser) losses.set(match.loser,losses.get(match.loser)+1);
      assert.equal(losses.get(f.champion),1);
      for(const team of teams.filter(team=>team!==f.champion)) assert.equal(losses.get(team),2);
    }
  }
});

test('Changing participants or clearing an upstream score invalidates later results', () => {
  const e=engine(); let f=complete(e,'final-four',['A','B','C','D']);
  e.tournamentState.knockoutScores[f.semifinals[0].id].homePoints=null;
  e.pruneKnockoutScores(); f=e.buildCurrentFinalPhase()[0];
  assert.equal(f.finalMatch,null); assert.equal(f.bronzeMatch,null);
  assert(!e.tournamentState.knockoutScores['general-final-0']);
  assert(!e.tournamentState.knockoutScores['general-bronze-0']);
});

test('Results require nonnegative whole points and cannot end in a tie', () => {
  const e=engine();
  for(const value of ['',null,undefined,-1,'1.5','abc']) assert.equal(e.parseScore(value),null);
  assert.equal(e.parseScore('0'),0); assert.equal(e.parseScore('21'),21);
  assert(!e.hasValidScore({homePoints:21,awayPoints:21}));
  assert(!e.hasValidScore({homePoints:null,awayPoints:15}));
  assert(e.hasValidScore({homePoints:21,awayPoints:15}));
});
