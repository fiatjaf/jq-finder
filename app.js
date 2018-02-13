/* global Elm */

const debounce = require('debounce')
const jq = require('jq-web')

const target = document.querySelector('main')


const app = Elm.Main.embed(target, {
  input: localStorage.getItem('input') || '',
  filters: JSON.parse(localStorage.getItem('filters') || '["."]')
})

app.ports.applyfilter.subscribe(debounce(([raw, i, filters]) => {
  let filter = filters
    .filter(x => x.trim())
    .join('|') || '.'

  let prelude = '. as $input | '

  console.log('jq', filters)

  try {
    let res = jq.raw(raw, prelude + filter)
    app.ports.gotresult.send([i, res])
  } catch (e) {
    console.log(e)
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
}, 600))
