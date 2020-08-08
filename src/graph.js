export default class Graph extends HTMLElement {
    constructor() {
        super();

        this.canvas = document.createElement("canvas");

        this.cx = this.canvas.getContext("2d");

        this.appendChild(this.canvas);

        this.offset = { x: 60.5, y: 60.5 };
        this.scale = { x: 20, y: 20 };
    }

    // Lifecycle events

    connectedCallback() {
        this.drawGraph();
    }

    attributeChangedCallback(attr, _, value) {
        this.drawGraph();
    }

    static get observedAttributes() {
        return [
            "first-function",
            "second-function",
            "width",
            "height",
            "offset-x",
            "offset-y",
            "scale-x",
            "scale-y",
        ];
    }

    // Math stuff

    viewportToWorld({ x, y }) {
        return {
            x: (x - this.offset.x) / this.scale.x,
            y: (this.height - y - this.offset.y) / this.scale.y,
        };
    }

    worldToViewport({ x, y }) {
        return {
            x: x * this.scale.x + this.offset.x,
            y: this.height - (y * this.scale.y + this.offset.y),
        };
    }

    isOutOfBounds(viewportY) {
        return viewportY < 0 || viewportY > this.height;
    }

    // Draw methods

    drawGraph() {
        //console.time("draw");

        // Update attributes

        this.offset = {
            x: (Number(this.getAttribute("offset-x"))) + 0.5,
            y: (Number(this.getAttribute("offset-y"))) + 0.5,
        };

        this.scale = {
            x: Number(this.getAttribute("scale-x")) || 20,
            y: Number(this.getAttribute("scale-y")) || 20,
        };

        this.canvas.width = this.width = Number(this.getAttribute("width"));

        this.canvas.height = this.height = Number(this.getAttribute("height"));

        // Note: No need to cx.clearRect() because changing canvas' width automatically does that.

        this.drawGrid();

        this.drawAxis();

        // The color is equivalent to tailwind's text-green-500
        this.drawFunction("second-function", "#48bb78");

        // The color is equivalent to tailwind's text-blue-500
        this.drawFunction("first-function", "#4299e1");

        //console.timeEnd("draw");
    }

    drawGrid() {
        let { cx, width, height } = this;

        // Drawing bounds, i.e. the lower and higher positions in world space that we care to draw

        let lowerBound = this.viewportToWorld({ x: 0, y: this.height });

        let upperBound = this.viewportToWorld({ x: this.width, y: 0 });

        cx.beginPath();

        for (let y = lowerBound.y; y < upperBound.y + 1; y += 1) {
            const viewportY = this.worldToViewport({
                x: 0,
                y: Math.floor(y),
            }).y;

            if (viewportY === 0) continue;

            cx.moveTo(0, viewportY);
            cx.lineTo(width, viewportY);
        }

        for (let x = lowerBound.x; x < upperBound.x + 1; x += 1) {
            const viewportX = this.worldToViewport({
                x: Math.floor(x),
                y: 0,
            }).x;

            if (viewportX === 0) continue;

            cx.moveTo(viewportX, 0);
            cx.lineTo(viewportX, height);
        }

        /* We make the grid more invisible when the scale is lower.
           Otherwise we'd get a lot of visual noise from the grid lines.
        */

        const lerp = (a, b, t) => {
            // Clamp between 0 and 1
            t = t > 1 ? 1 : t < 0 ? 0 : t;

            return (1 - t) * a + b * t;
        };

        // When scale is 0, opacity is 0.01
        // When scale is 100, opacity is 0.1

        const opacity = lerp(0.01, 0.1, this.scale.x / 100);

        cx.strokeStyle = `rgba(0, 0, 0, ${opacity})`;

        cx.stroke();
    }

    drawAxis() {
        let axisCenter = this.worldToViewport({ x: 0, y: 0 });

        let cx = this.cx;

        cx.beginPath();

        cx.moveTo(0, axisCenter.y);
        cx.lineTo(this.width, axisCenter.y);

        cx.moveTo(axisCenter.x, 0);
        cx.lineTo(axisCenter.x, this.height);

        cx.strokeStyle = "#bbb";

        cx.stroke();
    }

    drawFunction(functionName, functionColor) {
        let cx = this.cx;

        /* We use the Function constructor to produce a function from a string like "Math.sin(x)".

        Is this unsafe? The stringified function is produced in Elm code. It is not read directly from
        input. This means that if you write "alert(1)" in the input, you don't actually execute that code.
        Instead, you try to parse it and fail, so the actual value you read here will be some fallback, like
        "-999999".

        I claim no one will be able to inject code in this app (unless github servers are hacked, but then
        it won't make a difference if I'm using new Function or not).

        Why not taking a different approach? Evaluating functions as native code is the fastest way I can think of.
        And performance is important! If you pay attention, you'll notice that dragging the graph viewport
        sometimes looks glitchy because the drawing takes too long.

        What are other ways to do this, without using the Function constructor? I could represent expressions as
        JSON values that get evaluated using a recursive function. For example, the expression `x + 5` could be
        represented like:

            { type: "+", args: [ { type: "x" }, { type: "number", value: 5 } ] }

        This would be way slower, and thus I decided to avoid it.

        */

        let fn = new Function("x", "return " + this.getAttribute(functionName));

        let width = this.width;

        cx.beginPath();

        let lastViewportY = 0;

        for (let viewportX = 0; viewportX < width; viewportX++) {
            let x = this.viewportToWorld({ x: viewportX, y: 0 }).x;

            let y = fn(x);

            let viewportY = this.worldToViewport({ x: 0, y: y }).y;

            if (
                viewportX === 0 ||
                (this.isOutOfBounds(lastViewportY) &&
                    this.isOutOfBounds(viewportY))
            ) {
                cx.moveTo(viewportX, viewportY);
            } else {
                cx.lineTo(viewportX, viewportY);
            }

            lastViewportY = viewportY;
        }

        cx.strokeStyle = functionColor;

        cx.lineWidth = 1;

        cx.stroke();
    }
}
