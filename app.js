/* global Elm */

const debounce = require('debounce')
const jq = require('jq-web')

const target = document.querySelector('main')
const app = Elm.Main.embed(target)

app.ports.applyfilter_.subscribe(debounce(([i, input, filter]) => {
  if (input === '') {
    app.ports.send([i, ''])
    return
  }

  try {
    let res = jq.raw(input, filter)
    app.ports.gotresult.send([i, res])
  } catch (e) {
    console.log(e)
  }
}, 600))
