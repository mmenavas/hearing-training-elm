# Hearing Trainer (Elm Edition)
Hearing Trainer is a game design to help you fine tune your hearing skills. This is accomplish by playing a game where you are given a music note, and you have to guess the note from a list of options.

## Create Elm App
This project was bootstrapped with [Create Elm App](https://github.com/halfzebra/create-elm-app).

## Quickstart:
- Install the `create-elm-app` CLI tool: `npm install create-elm-app -g`
- Start development environment:  `elm-app start`
- Create a build ready for deploy: `elm-app build`
- Deploy with GitHub Pages: `gh-pages -d build` (requires gh-pages 3.0.0)

## Roadmap
- Add a score system.
- Add difficulty levels, where the easiest level just makes you guess contiguous notes. The next level will make you guess notes that are 1 or 2 steps apart. Etc.
- Guess note sequences.
- Prevent pressing buttons while a note is playing or stop the current note before another note is played.
- Back button needs to indicate more clearly that pressing that button exits the current game.
- Disbale notes until the wildcard note is heard.
- Make the instrument of choice configurable.
- Provide ability to record melodies.
- Make scale of choice configurable. 