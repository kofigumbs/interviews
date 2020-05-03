**Quick-Start:**
  - Run `npm start`
  - Open <http://127.0.0.1:8081/>


# About

This app displays "abc" data as a table and a chart.
The table is editable, and changing the value will update the chart.

The architecture is very minimal—the only dependency is [Plotly](https://plotly.com/javascript/).
An NPM script is included for local dev convenience, but the project is organized as plain HTML, CSS, and JS files.
Since I forewent a JS framework, but the spec mentions "reusable components", I decided to implement a few functions that map JS objects to DOM elements.
This gave me the opportunity to demonstrate how the design works with a component mindset, without introducing a build step or additional dependencies.

I decided not to automate any testing since the project is small enough to test exhaustively in a couple minutes.
I tested on my MacBook Pro (macOS 10.13.6) using Chrome v80, Safari v13, and Firefox v74.
This app uses several ES2015 constructs, so without a compile step it will not work properly in older browsers.
