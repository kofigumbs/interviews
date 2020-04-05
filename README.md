# Quick Start

- Dependencies: **Ruby 2.6.5+** and **PostgreSQL 9.5+**
- Create a local Postgres database: `psql -c 'CREATE DATABASE "15secondly"'`
- Start the dev servers: `env DAILY_API_KEY=<YOUR KEY HERE> make`

# Project Layout

This repo is contains a multi-page client app, which is built with Jekyll, and a JSON API server, built with Sinatra.
The client is deployed with Netlify, and the server is deployed with Heroku.
Given the scope of the project, there are a few non-traditional architecture decisions, which are all documented inline.
Here's a reference to the biggest ones:

- No client bundler: We reference our JS dependencies with <unpkg.com>.
  This is a risk since we aren't tagging version numbers.
  But this project will only be "live" for a few days, so we're taking the risk 🤠
- No migrations: We instead maintain an idempodent `schema.sql` file.
  I like this approach for 1-person projects because it makes it very easy to answer the question "what does my database look like _right now_?"
  This decision is less of a risk and more of a deferment: adding a migration framework to a 0-table project is about the same amount of work as adding one to a 2-table project.
- Static HTML client instead of JS SPA: This makes the project a little easier to share with reviewers.
  Most JS assume a NPM-based project, which this project avoided.
  There's a risk that "rolling your own JS implementation" resulted in messier code, but each of the files is less than 150 lines, which includes HTML, page styles, and documentation.


## Client

Each page is represented by a HTML file, with 3 top-level nodes: `<style>`, `<main>`, and `<script>`.
I make an effort to use semantic HTML tags and style those directly (i.e. no classes) only to make things more direct to read.
The JS always refers to DOM nodes in `<main>` by their ID, making all references easy enough to track.
In the context of each file, we prefer parameters over global variables.
The only globals are bound to the `FIFTEEN_SECONDLY` namespace, which is defined in [`_includes/shared.js`](client/_includes/shared.js).


## Server

Most of the work happens in Sinatra's main entrypoint: [`server/config.ru`](server/config.ru).
Aside from the single Daily.co API call, everything is straightforward CRUD.


# Testing

I decided not to automate any testing since the project is small enough to test exhaustively in a couple minutes.
I tested on my MacBook Pro (macOS 10.13.6) using **Chrome v80**, **Safari v13**, and **Firefox v74**.
