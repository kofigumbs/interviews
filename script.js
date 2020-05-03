window.addEventListener("load", function() {
  const headerRow = document.querySelector("thead tr");
  for (const header of Object.keys(data[0])) {
    const th = document.createElement("th");
    th.innerText = header;
    headerRow.appendChild(th);
  }

  const tbody = document.querySelector("tbody");
  for (let index = 0; index < data.length; index++) {
    const row = data[index];
    const tr = document.createElement("tr");
    for (const [ key, value ] of Object.entries(row)) {
      const td = document.createElement("td");
      const input = document.createElement("input");
      input.dataset.index = index;
      input.dataset.key = key;
      input.value = value;
      input.addEventListener("input", onDataInput);
      td.appendChild(input);
      tr.appendChild(td);
    }
    tbody.appendChild(tr);
  }

  plot();

  function plot() {
    const xAxis = "a";
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
    if (!isNaN(value)) {
      data[input.dataset.index][input.dataset.key] = value;
      plot();
    }
  }
});
