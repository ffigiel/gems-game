import { registerSW } from 'virtual:pwa-register'
import { Elm } from './src/Main.elm'

registerSW()

const app = Elm.Main.init({
  node: document.getElementById("app"),
  flags: {
    language: window.navigator.language,
    highScore: window.localStorage.getItem("highScore"),
  },
})
app.ports.saveHighScore.subscribe(hs => {
  window.localStorage.setItem("highScore", hs)
})
app.ports.vibrate.subscribe(() => {
  window.navigator.vibrate(1)
})
app.ports.logError.subscribe(err => {
  console.error(err)
})
