/* API SERVER BASE URL
 */
window.API = "{{ site.env['15SECONDLY_API'] }}";


/* MOMENT.JS IIFE WRAPPER
 *
 *   window.timeFormat : {
 *     forHumans : (timestamp: String) => String
 *     inSeconds : (timestamp: String) => Number
 *   }
 */
window.timeFormat = (function() {
  const parse = timestamp => moment(timestamp, "YYYY-MM-DD hh:mm:ss ZZZZ");
  return {
    forHumans: timestamp => parse(timestamp).calendar(),
    inSeconds: timestamp => Math.round(parse(timestamp).valueOf() / 1000),
  }
})();

/* Bare minimum error feedback
 */
window.viewError = function(node) {
  node.innerHTML += `
    <b style='color: darkred'>Sorry, but something here isn't quite right</b>
  `;
}
