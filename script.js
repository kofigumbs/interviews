window.addEventListener("load", () => {
  validateSetup(() => {
    renderInto("thead tr", viewTableHead());
    renderInto("tbody", viewTableBody());
    renderChart();
  });
});

function validateSetup(callback) {
  // Bare minimum error UI
  const showError = text => document.querySelector("error").innerText = text;

  // Check global data assumptions
  if (typeof data === "undefined") return showError("No `data` variable is defined");
  if (typeof xAxis === "undefined") return showError("No `xAxis` variable is defined");
  if (typeof Plotly === "undefined") return showError("Plotly didn't load");
  if (!data.length) return showError("`data` variable is empty, at least 1 entry required");
  if (!data.every(x => x[xAxis])) return showError("All `data` entries must include the `xAxis` key");

  // If anything else unexpected goes wrong, say so
  try { callback() } catch (e) { showError(e.message) }
}

function viewTableHead() {
  // Use the first data entry key names to populate the column names
  return Object.keys(data[0]).map(key => ({ node: "th", text: key }));
}

function viewTableBody() {
  return data.map((row, index) => ({
    node: "tr",
    children: Object.entries(row).map(pair => ({
      node: "td",
      children: [{
        node: "input",
        on: { input: onDataInput },
        attributes: { "type": "number", "data-index": index, "data-key": pair[0], "value": pair[1] },
      }],
    })),
  }));
}

function renderChart() {
  // Plotly draws lines in order of appearance in x/y arrays.
  // We want that order to be "increasing x-axis", so we sort by that first.
  const sorted = Array.from(data);
  sorted.sort((first, second) => first[xAxis] - second[xAxis]);

  const traces = [];
  for (const yAxis of Object.keys(sorted[0])) {
    if (yAxis === xAxis) continue;
    const trace = { type: "scatter", x: [], y: [], name: yAxis };
    for (const point of sorted) {
      trace.x.push(point[xAxis]);
      trace.y.push(point[yAxis]);
    }
    traces.push(trace);
  }
  Plotly.newPlot("chart", traces, {}, { responsive: true });
}

function onDataInput(event) {
  const input = event.target;
  const value = parseFloat(input.value);
  
  // Ignore non-numeric inputs.
  // The user might be in the middle of entering/deleting something that _will be_ valid.
  if (!isNaN(value)) {
    // Update `data` with new input value then re-render the chart
    data[input.dataset.index][input.dataset.key] = value;
    renderChart();
  }
}


/* RENDERING
 *
 * Very simple "virtual-dom style framework" to setup elements
 * Requirements mentioned that the challenge should be solved with a component
 * architecture. I think the project is too small to justify React or similar,
 * so instead we just use JS objects to pass around our UI components.
 */

function renderInto(selector, children) {
  renderChildren(document.querySelector(selector), children);
}

function renderChildren(parent, children) {
  for (let child of children) {
    const element = document.createElement(child.node);
    element.innerText = child.text || "";
    renderChildren(element, child.children || []);
    eachEntry(child.on || {}, (key, value) => element.addEventListener(key, value));
    eachEntry(child.attributes || {}, (key, value) => element.setAttribute(key, value));
    parent.appendChild(element);
  }
}

function eachEntry(object, callback) {
  for (const [ key, value ] of Object.entries(object)) callback(key, value);
}
