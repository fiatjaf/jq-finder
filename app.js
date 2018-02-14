/* global Elm */

const debounceWithArgs = require('debounce-with-args')
const jq = require('jq-web/jq.wasm.min.js')

const target = document.querySelector('main')

const app = Elm.Main.embed(target, {
  input: localStorage.getItem('input') || '',
  filters: JSON.parse(localStorage.getItem('filters') || '["."]'),
  widths: JSON.parse(localStorage.getItem('widths') || '[250]')
})

app.ports.scrollintopanel.subscribe(paneln => {
  setTimeout(() => {
    let el = document.getElementsByClassName('panel')[paneln]
    el.scrollIntoView({behavior: 'instant', block: 'end', inline: 'nearest'})
  }, 250)
})

app.ports.savepanelwidth.subscribe(([paneln, width]) => {
  var storedwidths = JSON.parse(localStorage.getItem('widths') || '[]')
  if (storedwidths.length < paneln + 1) {
    storedwidths.push(width)
  } else {
    storedwidths[paneln] = width
  }
  if (storedwidths.length === 0) storedwidths = ['.']
  localStorage.setItem('widths', JSON.stringify(storedwidths))
})

app.ports.applyfilter.subscribe(
  debounceWithArgs(
    applyfilter,
    600,
    args => args[0][1]
  )
)

function applyfilter ([raw, i, filters]) {
  let filter = filters
    .filter(x => x.trim())
    .join('|') || '.'

  let prelude = '. as $input | '

  console.log('jq', filters)

  try {
    let res = jq.raw(raw, prelude + filter)
    app.ports.gotresult.send([i, res])
  } catch (e) {
    if (typeof e === 'string' && e.slice(0, 5) === 'abort') {
      setTimeout(applyfilter, 500, [raw, i, filters])
      return
    }
    app.ports.goterror.send([i, e.message])
  }

  var storedfilters = JSON.parse(localStorage.getItem('filters') || '[]')
  for (let i = 0; i < filters.length; i++) {
    if (storedfilters.length < i + 1) {
      storedfilters.push(filters[i])
    } else {
      storedfilters[i] = filters[i]
    }
  }
  if (storedfilters.length === 0) storedfilters = ['.']
  localStorage.setItem('filters', JSON.stringify(storedfilters))
  localStorage.setItem('input', raw)
}
