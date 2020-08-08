# DRAW FUNCTIONS

> [Ver README en español.](docs/README.es.md)

[Quickly draw and compare mathematical functions](https://mauroc8.github.io/draw-functions/)

![Demo showing how to use this app](docs/demo.gif)

[TRY IT NOW!](https://mauroc8.github.io/draw-functions/)

## Motivation

> This is a remake of an app I made a few years ago. To see that repository, [click here](https://github.com/mauroc8/graficar) (the README is in Spanish).

This app allows you to quickly draw and compare mathematical functions in a 2D coordinate system.

There are some great web apps to draw functions. The most famous one is [Wolfram Alpha](https://www.wolframalpha.com/).
But there are some things that Wolfram Alpha can't do:

-   **Quicky draw functions.** Even the simplest function, like `y = x`, can take up to 2 seconds to draw. Using this app, you get instant feedback as you type.
-   **Move and zoom the viewport.** The default viewport in Wolfram Alpha is very small, and you can't enlarge it without paying a Pro plan.

On the other hand, there are **tons** of things that you can do with Wolfram Alpha and not with this app. **I don't intend to compete with Wolfram Alpha.**

The main motivation to make this app was to learn about [parsing](https://en.wikipedia.org/wiki/Parsing). It was _very_ helpful through the years when studying calculus or when working with functions.

## Syntax reference

We use the symbols `+`, `-`, `*` and `/` for sum, subtraction, multiplication and division.

We use `^` for power. For example _x²_ is written as `x^2`.

There is a shorter way to write multiplications. `2*x` can be written as `2x`.

> There are some subtleties here. `1/2*x` is equal to `(1/2)*x`, but `1/2x` is equal to `1/(2*x)`. In other words, multiplication not using `*` has more [precedence](https://en.wikipedia.org/wiki/Order_of_operations).

We use `sin(...)` to write functions calls. Supported functions are `sin`, `cos`, `tan`, `asin`, `acos`, `atan`, `abs` (absolute value) and `sqrt`.

> The nth-root of `x` can be written as `x^(1/n)` where `n` is a number or an expression.

## Development and build

I run the development server using [Parcel](https://parceljs.org/).

`parcel src/index.html`

To make a build:

`parcel build src/index.html --public-url ./`

> The latest build is uploaded in the `gh-pages` branch of this repository.

## Code walkthrough

-   I use [Tailwind CSS](https://tailwindcss.com/) to style the page.
-   I use [Elm](https://elm-lang.org/) (a functional programming language that compiles to Javascript) for almost everything, except drawing in the canvas.
-   `src/index.js` just starts the Elm app and connects the Elm code with the Javascript code.
-   `src/graph.js` defines a custom element that is used in the Elm app to draw the functions.
-   `src/Main.elm` has the main application code, including logic to handle input, and the view.
-   `src/Expression.elm` has the expression parser, using the beautifully designed `elm/parser` package.
-   Other Elm files in `src/` contain helper functions.
