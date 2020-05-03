window.addEventListener("load", () => {
  validateSetup(() => {
    setupTableHead();
    setupTableBody();
    drawChart();
  });
});

function validateSetup(callback) {
  // Bare minimum error UI
  const showError = text => document.querySelector("error").innerText = text;

  // Check global data assumptions
  if (typeof data === "undefined") return showError("No `data` variable is defined");
  if (typeof xAxis === "undefined") return showError("No `xAxis` variable is defined");
  if (typeof Plotly === "undefined") return showError("Plotly didn't to load");
  if (!data.length) return showError("`data` variable is empty, at least 1 entry required");
  if (!data.every(x => x[xAxis])) return showError("All `data` entries must include the `xAxis` key");

  // If anything else unexpected goes wrong, say so
  try { callback() } catch (e) { showError(e.message) }
}

function setupTableHead() {
  const tr = document.querySelector("thead tr");
  for (const key of Object.keys(data[0])) {
    const th = document.createElement("th");
    th.innerText = key;
    tr.appendChild(th);
  }
}

function setupTableBody() {
  const tbody = document.querySelector("tbody");
  for (let index = 0; index < data.length; index++) {
    const row = data[index];
    const tr = document.createElement("tr");
    for (const [ key, value ] of Object.entries(row)) {
      const td = document.createElement("td");
      const input = document.createElement("input");
      input.type = "number";
      input.dataset.index = index;
      input.dataset.key = key;
      input.value = value;
      input.addEventListener("input", onDataInput);
      td.appendChild(input);
      tr.appendChild(td);
    }
    tbody.appendChild(tr);
  }
}

function drawChart() {
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
    data[input.dataset.index][input.dataset.key] = value;
    drawChart();
  }
}
