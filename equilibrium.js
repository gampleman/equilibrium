import { Elm } from "./src/Equilibrium.elm";

const initialScore = localStorage.getItem('score');
Elm.Equilibrium.init({
  node: document.getElementById('main'),
  flags: initialScore && JSON.parse(initialScore) || {}
}).ports.saveScore.subscribe(score => {
    localStorage.setItem('score', JSON.stringify(score));
});
