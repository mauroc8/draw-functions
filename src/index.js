import { Elm } from "./Main.elm";

import Graph from "./graph.js";

import "./styles.pcss";

const app = Elm.Main.init({
	node: document.getElementById("elm-app"),
	flags: {
		firstInputValue: localStorage.getItem("firstInputValue") || "sin(x)",
		secondInputValue: localStorage.getItem("secondInputValue") || "",
	},
});

window.customElements.define("graph-element", Graph);

app.ports.saveFirstInputValue.subscribe(function (firstInputValue) {
	localStorage.setItem("firstInputValue", firstInputValue);
});

app.ports.saveSecondInputValue.subscribe(function (secondInputValue) {
	localStorage.setItem("secondInputValue", secondInputValue);
});

window.addEventListener("touchend", function (event) {
	app.ports.onTouchEnd.send(event);
});
