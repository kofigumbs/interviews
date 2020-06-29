/* GLOBALS NAMESPACE
 */
window.FIFTEEN_SECONDLY = {

  /* API server base URL
   */
  API: "{{ site.env['15SECONDLY_API'] }}",


  /* Moment.js wrapper
   *
   *   timestamp: String
   *
   * Returns: String
   */
  formatTime: function(timestamp) {
    return moment(timestamp, "YYYY-MM-DD hh:mm:ss ZZZZ").calendar();
  },

  /* Listen for errors that may be thrown and report them
   */
  feedbackOnError: function(callback) {
    try {
      callback();
    } catch (e) {
      FIFTEEN_SECONDLY.viewError(e);
    }
  },

  /* Bare minimum error view
   */
  viewError: function(e) {
    let strong = document.getElementById("error-message");
    // Skip most of the creation steps if this isn't the first error
    if (!strong) {
      strong = document.createElement("strong");
      strong.id = "error-message";
      strong.style.color = "darkred";
      strong.innerText = "Sorry, but something went wrong!";
      document.body.prepend(strong);
    }
    strong.title = e.toString();
    strong.scrollIntoView();
  },
};
