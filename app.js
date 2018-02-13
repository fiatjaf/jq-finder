/* global Elm */

const debounce = require('debounce')
const jq = require('jq-web')

const target = document.querySelector('main')
const app = Elm.Main.embed(target)

app.ports.applyfilter.subscribe(debounce(([i, input, filter]) => {
  if (filter === '' || input === '') {
    app.ports.gotresult.send([i, ''])
    return
  }

  try {
    let res = jq.raw(input, filter)
    app.ports.gotresult.send([i, res])
  } catch (e) {
    console.log(e)
    app.ports.goterror.send([i, e.message])
  }
}, 600))
