import { html, define, property, render } from "https://unpkg.com/hybrids@4.1.9/src";

const data = fetch("data.json").then(x => x.json());

define("file-table", {
  selected: property(new Set()),
  render: render(
    ({ selected }) => html.resolve(
      data
        .then(files => html`${viewHeader(selected, files)}${viewTable(selected, files)}`)
        .catch(viewError),
      viewLoading()
    ),
    { shadowRoot: false }
  ),
});

function viewLoading() {
  return html`<p>Loading...</p>`;
}

function viewError(error) {
  return html`<p class="error" title="${error}">Something went wrong :(</p>`;
}

function viewHeader(selected, files) {
  return html`
    <header>
      <input
        type="checkbox"
        onchange="${toggleAll(files)}"
        checked="${selected.size === files.length}"
        indeterminate="${selected.size > 0 && selected.size < files.length}">
      <h2>${selected.size === 0 ? "None Selected" : "Selected " + selected.size}</h2>
      <button disabled="${selected.size === 0}" onclick="${download(selected)}">
        <!-- https://feathericons.com/ -->
        <svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" class="feather feather-download"><path d="M21 15v4a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2v-4"/><polyline points="7 10 12 15 17 10"/><line x1="12" y1="15" x2="12" y2="3"/></svg>
        Download Selected
      </button>
    </header>
  `;
}

function viewTable(selected, files) {
  return html`
    <table cellspacing="0">
      <thead>
        <tr>
          <td></td>
          <td>Name</td>
          <td>Device</td>
          <td>Path</td>
          <td>Status</td>
        </tr>
      </thead>
      <tbody>
        ${files.map(file => viewTableRow(selected, file))}
      </tbody>
    </table>
  `;
}

function viewTableRow(selected, file) {
  const available = isAvailable(file);
  const active = selected.has(file);
  return html`
    <tr class="${{ "is-available": available, "is-active": active }}"
        onclick="${available && toggle(file)}">
      <td>
        <input
          type="checkbox"
          onchange="${toggle(file)}"
          checked="${active}"
          disabled="${!available}">
      </td>
      <td>${file.name}</td>
      <td>${file.device}</td>
      <td>${file.path}</td>
      <td class="status">${file.status}</td>
    </tr>
  `;
};

function toggle(file) {
  return (host, event) => {
    event.preventDefault();
    host.selected = new Set(host.selected); // re-assign to trigger re-render
    host.selected.has(file)
      ? host.selected.delete(file)
      : host.selected.add(file);
  };
}

function toggleAll(files) {
  return (host, event) => {
    if (event.target.checked)
      host.selected = new Set(files.filter(isAvailable));
    else
      host.selected = new Set();
  }
}

function download(selected) {
  return () => {
    alert(
      Array.from(selected)
        .map(file => `Device: ${file.device}\nPath:${file.path}`)
        .join("\n\n"));
  };
}

function isAvailable(file) {
  return file.status === "available";
}
