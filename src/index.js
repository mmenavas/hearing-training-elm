import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';

var app = Elm.Main.init({
  node: document.getElementById('root')
});

app.ports.play.subscribe(function (data) {
  if (data) {
    var note = data.toLowerCase();
    var id = "audio-" + note;
    var audio = document.getElementById(id);
    audio.play();
  }
})

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
